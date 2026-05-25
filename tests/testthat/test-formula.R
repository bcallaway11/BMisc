# Tests for formula utility functions

# rhs_vars, lhs_vars: variable extraction
test_that("rhs_vars and lhs_vars extract correct variable names", {
  ff <- y ~ x1 + x2
  expect_equal(rhs_vars(ff), c("x1", "x2"))
  expect_equal(lhs_vars(ff), "y")
  expect_null(lhs_vars(~x1 + x2))
})

# toformula, add_cov_to_formula, drop_cov_from_formula: formula construction
test_that("formula construction and modification functions work correctly", {
  ff <- toformula("y", c("x1", "x2"))
  expect_equal(deparse(ff), "y ~ x1 + x2")

  ff_add <- add_cov_to_formula("z", ff)
  expect_true("z" %in% rhs_vars(ff_add))
  expect_equal(length(rhs_vars(ff_add)), 3L)

  ff_drop <- drop_cov_from_formula("x1", ff)
  expect_false("x1" %in% rhs_vars(ff_drop))
  expect_equal(rhs_vars(ff_drop), "x2")
})
