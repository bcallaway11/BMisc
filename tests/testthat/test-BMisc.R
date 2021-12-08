test_that("TorF", {
  expect_equal(TorF(c(1,2,3) == c(0,NA,3)), c(FALSE, FALSE, TRUE))
  expect_equal(TorF(c(1,2,3) == c(0,NA,3), use_isTRUE=TRUE),
                c(FALSE, FALSE, TRUE))
  expect_error(TorF("BMisc"), "cond should be a logical vector")
})

test_that("makeBalancedPanel", {
  id <- rep(seq(1,100,1),2) 
  period <- rep(seq(1,2),100) 
  y <- rnorm(200) 
  data <- data.frame(id=id, period=period, y=y) 
  data <- data[-1,] 
  data <- makeBalancedPanel(data, idname="id", tname="period")

  expect_false(1 %in% data$id)
  expect_equal(length(unique(data$id)), 99)

  # check it works with tibble
  data <- tibble::as_tibble(data)
  data <- data[-1,]
  data <- makeBalancedPanel(data, idname="id", tname="period")

  expect_false(2 %in% data$id)
  expect_equal(length(unique(data$id)), 98)
})
