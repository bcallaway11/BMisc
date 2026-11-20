# Tests for miscellaneous and Rcpp utility functions

# mv_mult, t2orig, orig2t, weighted_combine_list: deterministic outputs
test_that("mv_mult, t2orig, orig2t, and weighted_combine_list return correct values", {
  expect_equal(mv_mult(matrix(1:9, 3, 3), c(1, 2, 3)), c(30, 36, 42))

  otp <- c(2001, 2003, 2005, 2007)
  expect_equal(t2orig(1:4, otp), c(2001, 2003, 2005, 2007))
  expect_equal(orig2t(c(2001, 2005), otp), c(1, 3))

  l <- list(c(1, 2, 3), c(4, 5, 6))
  expect_equal(weighted_combine_list(l, w = c(0.5, 0.5)), c(2.5, 3.5, 4.5))
})

# vectorized orig2t/t2orig, block_boot_sample and weighted_ecdf must match
# the original per-element implementations
test_that("vectorized helpers match their element-wise definitions", {
  otp <- c(2001, 2003, 2005, 2007)
  x <- c(2005, 0, 2001, 2007, 2005)
  expect_identical(orig2t(x, otp), sapply(x, orig2t_inner, original_time.periods = otp))
  expect_identical(t2orig(c(3, 0, 1), otp), sapply(c(3, 0, 1), t2orig_inner, original_time.periods = otp))
  expect_true(is.na(orig2t(2002, otp)))

  set.seed(3)
  dat <- data.frame(id = rep(c(5, 2, 9), each = 2), t = 1:2, y = rnorm(6))
  set.seed(1)
  bb <- block_boot_sample(dat, "id")
  set.seed(1)
  ids <- sample(unique(dat$id), replace = TRUE)
  ref <- do.call(rbind, lapply(seq_along(ids), function(i) {
    bd <- dat[dat$id == ids[i], ]
    bd$id <- i
    bd
  }))
  expect_equal(bb, ref, ignore_attr = TRUE)

  y <- round(rnorm(500), 1)
  w <- runif(500)
  ys <- sort(unique(y))
  expect_equal(weighted_ecdf(y, weights = w)(ys),
               vapply(ys, function(v) mean(w / mean(w) * (y <= v)), 1.0))
})

# element_wise_mult and multiplier_bootstrap: Rcpp output shape and type
test_that("Rcpp functions return matrices of correct dimensions", {
  set.seed(42)
  n <- 10
  B <- 4
  k <- 2
  U <- matrix(rnorm(n * B), n, B)
  inf_func <- matrix(rnorm(n * k), n, k)

  result <- element_wise_mult(U, inf_func)
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(B, k))

  mb <- multiplier_bootstrap(inf_func, 5L)
  expect_true(is.matrix(mb))
  expect_equal(dim(mb), c(5L, k))
  expect_true(is.numeric(mb))
})

# multiplier_bootstrap: draws have the right first and second moments.
# Each iteration draws a fresh Rademacher vector U and returns
# dot(inf_func[, j], U) / n, so E[out[, j]] = 0 and
# Var(out[, j]) = sum(inf_func[, j]^2) / n^2.
test_that("multiplier_bootstrap draws have the correct moments", {
  set.seed(20260908)
  n <- 200
  k <- 3
  biters <- 20000L
  # Correlated columns, so the cross-covariances are big enough to test.
  Sigma <- matrix(c(
    1.0, 0.5, 0.3,
    0.5, 2.0, 0.2,
    0.3, 0.2, 1.5
  ), k, k)
  inf_func <- matrix(rnorm(n * k), n, k) %*% chol(Sigma)

  out <- multiplier_bootstrap(inf_func, biters)
  expect_equal(dim(out), c(biters, k))

  theoretical_vcov <- crossprod(inf_func) / n^2
  se <- sqrt(diag(theoretical_vcov) / biters)
  expect_true(all(abs(colMeans(out)) < 4 * se))

  expect_equal(cov(out) / theoretical_vcov, matrix(1, k, k), tolerance = 0.05)
})

# The two accumulation methods draw the same weights, so for a given seed they
# should only differ by floating point summation order.
test_that("multiplier_bootstrap methods agree for the same seed", {
  inf_func <- matrix(rnorm(5000 * 3), 5000, 3)  # 5000 rows spans two tiles
  b <- 50L

  set.seed(1234)
  gemm <- multiplier_bootstrap(inf_func, b, method = "gemm")
  set.seed(1234)
  loop <- multiplier_bootstrap(inf_func, b, method = "loop")

  expect_equal(gemm, loop, tolerance = 1e-10)

  expect_error(
    multiplier_bootstrap(inf_func, b, method = "nope"),
    "should be one of"
  )
})
