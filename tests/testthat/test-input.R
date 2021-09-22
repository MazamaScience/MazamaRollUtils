context("input")

test_that("bad input is detected", {

  x <- rnorm(50)
  expect_error(roll_mean(x, width = 0))
  expect_error(roll_mean(x, width = 1e6))
  expect_error(roll_mean(x, by = 0))
  expect_error(roll_mean(x, by = 1e6))
  expect_error(roll_mean(x, align = "forward"))
  expect_error(roll_mean(x, width = 3, weights = c(1)))
  expect_error(roll_mean(x, width = 3, weights = c(1,1,1,1,1)))
  expect_error(roll_mean(x, width = 3, weights = c(-1,1,1)))
  expect_error(roll_mean(x, width = 3, weights = c(0,0,0)))

})

