# =============================================================================
# Title: Tests for distribution functions
# Description: make_dist(), invert_ecdf(), quantile(), combine_ecdfs()
#   (including the exact mixture) and weighted statistics.
# Author: Brant Callaway
# Last update: 2026-11-19
# Date created: 2026-11-19
# =============================================================================

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
  # the type-1 sample median (0.0898 before #10, one order statistic low)
  expect_equal(Finv(0.5), unname(quantile(y, 0.5, type = 1)))
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

# combine_ecdfs() without y.seq: the exact mixture, from every support point
test_that("combine_ecdfs() mixes exactly without y.seq", {
  set.seed(1)
  x <- rnorm(100)
  y <- round(rnorm(100, 1), 1) # ties, and points shared with x
  x[1:5] <- y[1:5]
  # equal-size ECDFs with equal weights: the ECDF of the pooled sample
  both <- combine_ecdfs(ecdflist = list(ecdf(x), ecdf(y)))
  pooled <- ecdf(c(x, y))
  pts <- sort(unique(c(x, y)))
  expect_equal(both(pts), pooled(pts))
  expect_s3_class(both, "BMisc_dist")
  probs <- c(0, 0.01, seq(0.05, 0.95, 0.05), 0.99, 1)
  # type-1 quantile: the ceiling(n * p)-th order statistic, with n * p
  # rounded first (quantile(type = 1) computes 200 * 0.15 as
  # 30.000000000000004 and so returns the 31st, not the 30th)
  s <- sort(c(x, y))
  expect_equal(unname(quantile(both, probs)),
               s[pmax(1, ceiling(round(200 * probs, 8)))])

  # unequal weights and weighted ECDFs: brute force at every pooled point
  fx <- weighted_ecdf(x, weights = runif(100))
  fy <- weighted_ecdf(y, weights = runif(100))
  w <- c(0.3, 0.7)
  mix <- combine_ecdfs(ecdflist = list(fx, fy), weights = w)
  expect_equal(mix(pts), w[1] * fx(pts) + w[2] * fy(pts))
  # the grid version agrees at its own points
  grid <- seq(-2, 3, 0.25)
  expect_equal(mix(grid), combine_ecdfs(grid, list(fx, fy), w)(grid))

  # zero weights are skipped
  expect_equal(combine_ecdfs(ecdflist = list(fx, fy), weights = c(1, 0))(pts),
               fx(pts))

  # integer weights give the same quantiles as replicated data
  k <- sample(1:3, 100, replace = TRUE)
  fw <- combine_ecdfs(ecdflist = list(weighted_ecdf(x, weights = k),
                                      ecdf(y)), weights = c(0.5, 0.5))
  fr <- combine_ecdfs(ecdflist = list(ecdf(rep(x, k)), ecdf(y)),
                      weights = c(0.5, 0.5))
  expect_equal(quantile(fw, probs), quantile(fr, probs))

  # errors
  lin <- make_dist(sort(x), seq(0.01, 1, 0.01), method = "linear")
  expect_error(combine_ecdfs(ecdflist = list(lin, fy)), "step functions")
  expect_error(combine_ecdfs(ecdflist = list(fx, fy), weights = 1),
               "one value per element")
  expect_error(combine_ecdfs(ecdflist = list(fx, fy), weights = c(0, 0)),
               "all weights are 0")
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

# quantile() of make_dist / weighted_ecdf objects inverts F directly (#10)
test_that("quantile() is correct with ties and unequal weights (#10)", {
  # 0/1 with P(1) = 0.6: the median is 1
  F01 <- weighted_ecdf(c(0, 1), weights = c(0.4, 0.6))
  expect_s3_class(F01, "BMisc_dist")
  expect_s3_class(F01, "ecdf")
  expect_equal(unname(quantile(F01, c(0.3, 0.4, 0.5, 1))), c(0, 0, 1, 1))

  # equal weights with ties agrees with stats::ecdf()
  set.seed(1)
  y <- rbinom(1000, 1, 0.6)
  probs <- c(0.1, 0.39, 0.5, 0.9)
  expect_equal(unname(quantile(weighted_ecdf(y), probs)),
               unname(quantile(ecdf(y), probs, type = 1)))

  # continuous data, equal weights: same as stats::ecdf(), type 1
  z <- rnorm(200)
  expect_equal(unname(quantile(weighted_ecdf(z), probs)),
               unname(quantile(ecdf(z), probs, type = 1)))

  # unequal weights: the smallest point where the weighted CDF reaches p
  w <- runif(200)
  Fw <- weighted_ecdf(z, weights = w)
  zs <- sort(z)
  cw <- cumsum(w[order(z)]) / sum(w)
  expect_equal(unname(quantile(Fw, probs)),
               vapply(probs, function(p) zs[which(cw >= p - 1e-12)[1]], 1))

  # make_dist directly: masses not multiples of 1 / (number of points)
  M <- make_dist(c(0, 1, 2), c(0.2, 0.45, 1))
  expect_equal(unname(quantile(M, c(0.2, 0.4, 0.45, 0.5))), c(0, 1, 1, 2))
  expect_named(quantile(M, 0.5), "50%")

  # method = "linear" inverts the piecewise-linear F
  L <- make_dist(c(0, 1, 2), c(0, 0.5, 1), method = "linear")
  expect_equal(unname(quantile(L, c(0.25, 0.75))), c(0.5, 1.5))

  # floating-point dips in F (e.g. from combining ECDFs) are tolerated
  D <- make_dist(c(1, 2, 3), c(0.5, 1, 1 - 1e-16))
  expect_equal(unname(quantile(D, c(0.5, 0.9, 1))), c(1, 2, 2))

  # other sample-quantile types don't apply
  expect_warning(quantile(F01, 0.5, type = 7), "ignored")
  expect_error(quantile(F01, 1.5), "outside")
})

test_that("invert_ecdf() returns the smallest point with F >= p (#10)", {
  Finv <- invert_ecdf(weighted_ecdf(c(0, 1), weights = c(0.4, 0.6)))
  expect_equal(Finv(c(0.2, 0.4, 0.5, 1)), c(0, 0, 1, 1))
  set.seed(2)
  z <- rnorm(50)
  expect_equal(invert_ecdf(ecdf(z))(c(0.1, 0.5, 0.9)),
               unname(quantile(ecdf(z), c(0.1, 0.5, 0.9), type = 1)))
})
