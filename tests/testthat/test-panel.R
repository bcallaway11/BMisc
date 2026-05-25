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
