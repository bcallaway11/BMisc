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
