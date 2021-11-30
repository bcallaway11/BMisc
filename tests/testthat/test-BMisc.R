test_that("TorF", {
  expect_equal(TorF(c(1,2,3) == c(0,NA,3)), c(FALSE, FALSE, TRUE))
  expect_equal(TorF(c(1,2,3) == c(0,NA,3), use_isTRUE=TRUE),
                c(FALSE, FALSE, TRUE))
  expect_error(TorF("BMisc"), "cond should be a logical vector")
})
