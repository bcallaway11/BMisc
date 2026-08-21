test_that("TorF", {
  expect_equal(TorF(c(1, 2, 3) == c(0, NA, 3)), c(FALSE, FALSE, TRUE))
  expect_equal(TorF(c(1, 2, 3) == c(0, NA, 3), use_isTRUE = TRUE),
               c(FALSE, FALSE, TRUE))
  expect_error(TorF("BMisc"), "cond should be a logical vector")
})

test_that("make_balanced_panel drops unbalanced units", {
  id <- rep(seq(1, 100, 1), 2)
  period <- rep(seq(1, 2), 100)
  y <- rnorm(200)
  data <- data.frame(id = id, period = period, y = y)
  data <- data[-1, ]
  data <- make_balanced_panel(data, idname = "id", tname = "period")

  expect_false(1 %in% data$id)
  expect_equal(length(unique(data$id)), 99)

  # tibble input should also work
  data <- tibble::as_tibble(data)
  data <- data[-1, ]
  data <- make_balanced_panel(data, idname = "id", tname = "period")

  expect_false(2 %in% data$id)
  expect_equal(length(unique(data$id)), 98)
})

test_that("make_balanced_panel returns the same class it was given and leaves it untouched", {
  data <- data.frame(id = rep(1:5, each = 2), period = rep(1:2, 5), y = rnorm(10))
  data <- data[-1, ]
  before_class <- class(data)

  out <- make_balanced_panel(data, idname = "id", tname = "period")
  expect_s3_class(out, "data.frame")
  expect_false(inherits(out, "data.table"))
  expect_equal(class(data), before_class) # caller's data.frame is untouched

  dt <- data.table::as.data.table(data)
  out_dt <- make_balanced_panel(dt, idname = "id", tname = "period")
  expect_true(data.table::is.data.table(out_dt))
})

test_that("set_balanced_panel drops unbalanced units and mutates its input into a data.table", {
  data <- data.frame(id = rep(1:5, each = 2), period = rep(1:2, 5), y = rnorm(10))
  data <- data[-1, ]

  out <- set_balanced_panel(data, idname = "id", tname = "period")
  expect_true(data.table::is.data.table(out))
  expect_false(1 %in% out$id)
  expect_equal(length(unique(out$id)), 4)
  expect_true(data.table::is.data.table(data)) # mutated by reference
})

test_that("sort_panel orders by (idname, tname), returns the input's class, and leaves it untouched", {
  set.seed(5)
  id <- rep(sample(1:5), each = 2)
  t <- rep(c(2, 1), 5)
  data <- data.frame(id = id, t = t, y = rnorm(10))
  before_class <- class(data)

  out <- sort_panel(data, idname = "id", tname = "t")
  expect_equal(out$id, sort(unique(id)) |> rep(each = 2))
  expect_equal(out$t, rep(1:2, 5))
  expect_equal(class(data), before_class) # caller's data.frame is untouched

  dt <- data.table::as.data.table(data)
  out_dt <- sort_panel(dt, idname = "id", tname = "t")
  expect_true(data.table::is.data.table(out_dt))
})

test_that("panel2cs2 does not shift outcomes across ids in unbalanced panels", {
  data <- data.frame(
    id = c(1, 2, 2),
    period = c(1, 1, 2),
    y = c(10, 20, 25)
  )

  out <- panel2cs2(
    data,
    yname = "y",
    idname = "id",
    tname = "period",
    balance_panel = FALSE
  )

  expect_equal(out$.y1[out$id == 1], NA_real_)
  expect_equal(out$.dy[out$id == 1], NA_real_)
  expect_equal(out$.y1[out$id == 2], 25)
  expect_equal(out$.dy[out$id == 2], 5)
})
