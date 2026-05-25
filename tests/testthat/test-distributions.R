# Tests for distribution function and weighted statistics utilities

# make_dist and invert_ecdf: class and stability checks
test_that("make_dist and invert_ecdf produce valid ecdf objects", {
  set.seed(42)
  y <- sort(rnorm(100))
  Fx <- seq(0.01, 1, length.out = 100)
  ecdf_fn <- make_dist(y, Fx)

  expect_s3_class(ecdf_fn, "ecdf")
  expect_equal(ecdf_fn(0), 0.46, tolerance = 1e-4)

  Finv <- invert_ecdf(ecdf(y))
  expect_true(is.function(Finv))
  expect_equal(Finv(0.5), 0.0898, tolerance = 1e-3)
})

# combine_ecdfs: weighted mixture of two distributions
test_that("combine_ecdfs returns a valid mixture distribution", {
  set.seed(42)
  Fx <- ecdf(rnorm(100))
  Fy <- ecdf(rnorm(100, mean = 1))
  both <- combine_ecdfs(seq(-3, 4, 0.1), list(Fx, Fy), weights = c(0.5, 0.5))

  expect_s3_class(both, "ecdf")
  # mixture CDF at 0 should be between the two individual CDFs
  expect_true(both(0) > Fx(0) * 0.5)
  expect_equal(both(0), 0.315, tolerance = 1e-4)
})

# weighted_mean, weighted_quantile, weighted_ecdf: numerical stability
test_that("weighted statistics return numerically stable results", {
  set.seed(42)
  y <- rnorm(200)
  w <- runif(200)

  wm <- weighted_mean(y, weights = w)
  expect_true(is.numeric(wm))
  expect_equal(wm, -0.049035, tolerance = 1e-4)

  wq <- weighted_quantile(0.5, y, weights = w)
  expect_true(is.numeric(wq))
  expect_equal(wq, 1e-04, tolerance = 1e-3)

  wF <- weighted_ecdf(y, weights = w)
  expect_s3_class(wF, "ecdf")
  expect_equal(wF(0), 0.4931, tolerance = 1e-3)
})
