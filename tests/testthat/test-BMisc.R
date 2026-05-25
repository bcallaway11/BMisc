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
