test_that("roll_mean returns expected values for a simple centered example", {
  x <- c(1, 2, 3, 4, 5)

  result <- roll_mean(x, 3, by = 1, align = "center")

  expect_equal(result, c(NA, 2, 3, 4, NA))
})

test_that("roll_mean returns expected values for left alignment", {
  x <- c(1, 2, 3, 4, 5)

  result <- roll_mean(x, 3, by = 1, align = "left")

  expect_equal(result, c(2, 3, 4, NA, NA))
})

test_that("roll_mean returns expected values for right alignment", {
  x <- c(1, 2, 3, 4, 5)

  result <- roll_mean(x, 3, by = 1, align = "right")

  expect_equal(result, c(NA, NA, 2, 3, 4))
})

test_that("roll_mean respects the by argument", {
  x <- c(1, 2, 3, 4, 5, 6, 7)

  result <- roll_mean(x, 3, by = 2, align = "center")

  expect_equal(result, c(NA, 2, NA, 4, NA, 6, NA))
})

test_that("roll_mean with width 1 returns the input values", {
  x <- c(10, 20, 30, 40, 50)

  result <- roll_mean(x, 1, by = 1, align = "center")

  expect_equal(result, x)
})

test_that("roll_mean includes the final valid centered window for even widths", {
  x <- c(1, 2, 3, 4, 5, 6)

  result <- roll_mean(x, 2, by = 1, align = "center")

  expect_equal(result, c(NA, 1.5, 2.5, 3.5, 4.5, 5.5))
})

test_that("roll_mean returns a single centered value when even width equals input length", {
  x <- c(1, 2, 3, 4)

  result <- roll_mean(x, 4, by = 1, align = "center")

  expect_equal(result, c(NA, NA, 2.5, NA))
})

test_that("roll_mean returns a single value for left alignment when width equals input length", {
  x <- c(1, 2, 3, 4)

  result <- roll_mean(x, 4, by = 1, align = "left")

  expect_equal(result, c(2.5, NA, NA, NA))
})

test_that("roll_mean returns a single value for right alignment when width equals input length", {
  x <- c(1, 2, 3, 4)

  result <- roll_mean(x, 4, by = 1, align = "right")

  expect_equal(result, c(NA, NA, NA, 2.5))
})

test_that("roll_mean handles negative values correctly", {
  x <- c(-2, -1, 0, 1, 2)

  result <- roll_mean(x, 3, by = 1, align = "center")

  expect_equal(result, c(NA, -1, 0, 1, NA))
})

test_that("roll_mean handles decimal values correctly", {
  x <- c(0.5, 1.5, 2.5, 3.5, 4.5)

  result <- roll_mean(x, 3, by = 1, align = "center")

  expect_equal(result, c(NA, 1.5, 2.5, 3.5, NA))
})

test_that("roll_mean applies weights in a simple example", {
  x <- c(1, 2, 3, 4, 5)

  result <- roll_mean(x, 3, by = 1, align = "center", weights = c(1, 2, 1))

  expect_equal(result, c(NA, 2, 3, 4, NA))
})
