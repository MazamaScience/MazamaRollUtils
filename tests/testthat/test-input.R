context("input")

test_that("bad input is detected", {

  x <- rnorm(50)
  expect_error(roll_mean(x, width = 0))
  expect_error(roll_mean(x, width = 1e6))

})

