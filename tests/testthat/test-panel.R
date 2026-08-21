# Tests for panel data utility functions

# Staggered panel setup shared across blocks
make_staggered_panel <- function(n = 30, seed = 42) {
  set.seed(seed)
  id <- rep(seq_len(n), each = 4)
  t <- rep(1:4, n)
  y <- rnorm(n * 4)
  g <- rep(sample(c(0, 2, 3), n, replace = TRUE), each = 4)
  treat <- as.integer(t >= g & g > 0)
  data.frame(id = id, t = t, y = y, treat = treat, group = g)
}

# get_group and check_staggered: basic structural checks
test_that("get_group and check_staggered work correctly", {
  dta <- make_staggered_panel()
  dta$group_calc <- get_group(dta, idname = "id", tname = "t", treatname = "treat")

  # group should match original group assignment
  expect_equal(dta$group_calc, dta$group)
  # all group values should be 0, 2, or 3
  expect_true(all(dta$group_calc %in% c(0, 2, 3)))
  # treatment reversal (1->0) is not staggered
  dta_rev <- data.frame(id = c(1, 1, 2, 2), t = c(1, 2, 1, 2), treat = c(0, 1, 1, 0))
  expect_false(check_staggered(dta_rev, idname = "id", treatname = "treat"))
})

# get_lagYi and get_first_difference: each unit should have exactly one NA (first period)
test_that("get_lagYi and get_first_difference produce correct NA pattern", {
  dta <- make_staggered_panel()
  lag_y <- get_lagYi(dta, idname = "id", yname = "y", tname = "t")
  fd <- get_first_difference(dta, idname = "id", yname = "y", tname = "t")

  expect_equal(sum(is.na(lag_y)), 30L)
  expect_equal(sum(is.na(fd)), 30L)
  expect_true(is.numeric(lag_y))
  expect_true(is.numeric(fd))
})

# panel2cs2 and time_invariant_to_panel: shape and column checks
test_that("panel2cs2 and time_invariant_to_panel return correct structure", {
  set.seed(42)
  id <- rep(seq_len(20), each = 2)
  t <- rep(1:2, 20)
  y <- rnorm(40)
  dta2 <- data.frame(id = id, t = t, y = y)

  out <- panel2cs2(dta2, yname = "y", idname = "id", tname = "t")
  expect_equal(nrow(out), 20L)
  expect_true(all(c(".y0", ".y1", ".dy") %in% names(out)))
  expect_equal(out$.dy, out$.y1 - out$.y0)

  # time_invariant_to_panel repeats each unit value for all its periods
  x_unit <- rnorm(20)
  x_panel <- time_invariant_to_panel(x_unit, dta2, idname = "id")
  expect_equal(length(x_panel), 40L)
  expect_equal(length(unique(x_panel[1:2])), 1L)
})

# get_lagYi lags along tname, not along row order, and returns values aligned
# with the rows of the input (this is what dplyr::lag(order_by=) provided)
test_that("get_lagYi respects time ordering and input row order", {
  dta <- data.frame(
    id = c(1, 1, 1, 2, 2, 2),
    t = c(3, 1, 2, 2, 3, 1),
    y = c(30, 10, 20, 200, 300, 100)
  )
  expect_equal(get_lagYi(dta, "id", "y", "t"), c(20, NA, 10, 100, 200, NA))
  expect_equal(get_lagYi(dta, "id", "y", "t", nlags = 2), c(10, NA, NA, NA, 100, NA))
  expect_equal(get_first_difference(dta, "id", "y", "t"), c(10, NA, 10, 100, 100, NA))
})

# the unit-level getters expand one value per unit back out to nT
test_that("unit-level getters expand to one value per row", {
  dta <- data.frame(
    id = rep(1:2, each = 3),
    t = rep(1:3, 2),
    y = c(1, 2, 3, 10, 20, 30),
    group = rep(c(0, 3), each = 3)
  )
  expect_equal(get_Yi1(dta, "id", "y", "t", "group"), c(1, 1, 1, 10, 10, 10))
  expect_equal(get_Yit(dta, 2, "id", "y", "t"), c(2, 2, 2, 20, 20, 20))
  expect_equal(get_Yibar(dta, "id", "y"), c(2, 2, 2, 20, 20, 20))
  # group 0 units fall back to the last period / overall mean
  expect_equal(get_YiGmin1(dta, "id", "y", "t", "group"), c(3, 3, 3, 20, 20, 20))
  expect_equal(get_Yibar_pre(dta, "id", "y", "t", "group"), c(2, 2, 2, 15, 15, 15))
})

# none of these should modify the caller's data, including data.table input
test_that("panel getters do not modify their input", {
  dta <- data.table::data.table(
    id = rep(1:2, each = 2), t = rep(1:2, 2), y = c(1, 2, 3, 4), treat = c(0, 1, 0, 0)
  )
  before <- data.table::copy(dta)
  invisible(get_lagYi(dta, "id", "y", "t"))
  invisible(get_first_difference(dta, "id", "y", "t"))
  invisible(get_group(dta, "id", "t", "treat"))
  invisible(get_Yibar(dta, "id", "y"))
  expect_equal(dta, before)
})

# get_principal_components: shape and argument handling
test_that("get_principal_components returns the expected shape", {
  set.seed(42)
  n <- 20
  nperiods <- 4
  dta <- data.frame(
    id = rep(seq_len(n), each = nperiods),
    t = rep(seq_len(nperiods), n),
    x1 = rnorm(n * nperiods),
    x2 = rnorm(n * nperiods)
  )

  pcs <- get_principal_components(~ x1 + x2, dta, idname = "id", tname = "t")
  # one column per (variable, period) pair, one row per observation
  expect_equal(dim(pcs), c(n * nperiods, 2L * nperiods))
  expect_equal(names(pcs), c(paste0("x1_PC", 1:4), paste0("x2_PC", 1:4)))
  # principal components are unit-specific, so constant within a unit
  expect_equal(length(unique(pcs$x1_PC1[1:nperiods])), 1L)

  wide <- get_principal_components(~ x1 + x2, dta, "id", "t", ret_wide = TRUE, ret_id = TRUE)
  expect_equal(nrow(wide), n)
  expect_equal(wide$.id, sort(unique(dta$id)))

  two <- get_principal_components(~ x1 + x2, dta, "id", "t", n_components = 2)
  expect_equal(names(two), c(paste0("x1_PC", 1:2), paste0("x2_PC", 1:2)))
})

# dcast() (used internally) sorts rows by id, which can differ from the order
# units first appear in the data; the long-format output must realign to each
# row's own unit rather than assume that order, or units' PCs get swapped.
test_that("get_principal_components long format aligns by id when units are unsorted", {
  set.seed(7)
  n <- 6
  nperiods <- 3
  dta <- data.frame(
    id = rep(sample(seq_len(n)), each = nperiods),
    t = rep(seq_len(nperiods), n),
    x1 = rnorm(n * nperiods)
  )

  wide <- get_principal_components(~x1, dta, "id", "t", ret_wide = TRUE, ret_id = TRUE)
  long <- get_principal_components(~x1, dta, "id", "t")

  expected <- wide$x1_PC1[match(dta$id, wide$.id)]
  expect_equal(long$x1_PC1, expected)
})

# The vectorised getters must stay in step with the exported *_inner functions,
# which remain the reference definition of what each one computes. Units are
# deliberately left in an unsorted, first-appearance row order (not sorted by
# id) so this also catches getters that silently assume sorted input.
test_that("vectorised getters agree with the *_inner functions", {
  by_unit <- function(df, fun) {
    vals <- sapply(split(df, df$id), fun)
    unname(vals[as.character(df$id)])
  }

  set.seed(11)
  for (i in 1:25) {
    n <- sample(2:8, 1)
    nper <- sample(2:5, 1)
    ids <- sample(seq_len(3 * n), n)
    dta <- expand.grid(t = seq_len(nper), id = ids)[, c("id", "t")]
    dta$y <- round(rnorm(nrow(dta)), 3)
    # groups include 0 (never treated) and 1 (treated from the first period,
    # so no pre-treatment periods at all)
    gs <- sample(c(0, seq_len(nper)), n, replace = TRUE)
    dta$group <- gs[match(dta$id, ids)]
    dta$treat <- as.integer(dta$t >= dta$group & dta$group > 0)

    expect_equal(
      get_group(dta, "id", "t", "treat"),
      by_unit(dta, function(d) get_group_inner(d, "t", "treat"))
    )
    expect_equal(
      get_Yi1(dta, "id", "y", "t", "group"),
      by_unit(dta, function(d) get_Yi1_inner(d, "y", "t", "group"))
    )
    expect_equal(
      get_Yibar(dta, "id", "y"),
      by_unit(dta, function(d) get_Yibar_inner(d, "y"))
    )
    expect_equal(
      get_YiGmin1(dta, "id", "y", "t", "group"),
      by_unit(dta, function(d) get_YiGmin1_inner(d, "y", "t", "group"))
    )
    expect_equal(
      get_Yibar_pre(dta, "id", "y", "t", "group"),
      by_unit(dta, function(d) get_Yibar_pre_inner(d, "y", "t", "group"))
    )
    expect_equal(
      get_Yit(dta, 1, "id", "y", "t"),
      by_unit(dta, function(d) get_Yit_inner(d, 1, "y", "t"))
    )
    expect_equal(
      check_staggered(dta, "id", "treat"),
      all(by_unit(dta, function(d) check_staggered_inner(d, "treat")))
    )
  }
})

# a unit that is never observed in period tp now yields NA rather than
# silently shortening the returned vector
test_that("get_Yit returns NA for units not observed in period tp", {
  dta <- data.frame(id = c(1, 1, 2), t = c(1, 2, 1), y = c(10, 20, 30))
  expect_equal(get_Yit(dta, 2, "id", "y", "t"), c(20, 20, NA))
})

# check_staggered asks whether treatment is absorbing: units may switch on at
# different times, but no unit may ever switch back off
test_that("check_staggered identifies absorbing treatment", {
  panel <- function(treat) {
    data.frame(id = rep(1:2, each = 4), t = rep(1:4, 2), treat = treat)
  }
  # units adopting in different periods is the canonical staggered design
  expect_true(check_staggered(panel(c(0, 1, 1, 1, 0, 0, 1, 1)), "id", "treat"))
  # common adoption timing is still absorbing
  expect_true(check_staggered(panel(c(0, 1, 1, 1, 0, 1, 1, 1)), "id", "treat"))
  # so are always-treated and never-treated units
  expect_true(check_staggered(panel(c(1, 1, 1, 1, 0, 0, 0, 0)), "id", "treat"))
  # a single unit switching back off is enough to fail
  expect_false(check_staggered(panel(c(0, 1, 0, 1, 0, 0, 1, 1)), "id", "treat"))
  expect_false(check_staggered(panel(c(0, 1, 1, 1, 0, 1, 1, 0)), "id", "treat"))
})

# without tname the rows are assumed to be in time order; with it they are
# sorted first, so a scrambled panel is still read correctly
test_that("check_staggered sorts by tname when it is supplied", {
  dta <- data.frame(id = rep(1:2, each = 3), t = rep(1:3, 2),
                    treat = c(0, 0, 1, 0, 1, 1))
  scrambled <- dta[c(3, 1, 2, 5, 6, 4), ]

  expect_true(check_staggered(dta, "id", "treat"))
  expect_true(check_staggered(scrambled, "id", "treat", tname = "t"))
  # rows out of time order look like a reversal if tname is not given
  expect_false(check_staggered(scrambled, "id", "treat"))
  # the inner function behaves the same way on a single unit
  one <- scrambled[scrambled$id == 1, ]
  expect_true(check_staggered_inner(one, "treat", tname = "t"))
  expect_false(check_staggered_inner(one, "treat"))
})
