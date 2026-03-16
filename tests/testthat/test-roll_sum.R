test_that("roll_sum returns expected values for a simple centered example", {
  x <- c(1, 2, 3, 4, 5)

  result <- roll_sum(x, 3, by = 1, align = "center")

  expect_equal(result, c(NA, 6, 9, 12, NA))
})

test_that("roll_sum returns expected values for left alignment", {
  x <- c(1, 2, 3, 4, 5)

  result <- roll_sum(x, 3, by = 1, align = "left")

  expect_equal(result, c(6, 9, 12, NA, NA))
})

test_that("roll_sum returns expected values for right alignment", {
  x <- c(1, 2, 3, 4, 5)

  result <- roll_sum(x, 3, by = 1, align = "right")

  expect_equal(result, c(NA, NA, 6, 9, 12))
})

test_that("roll_sum respects the by argument", {
  x <- c(1, 2, 3, 4, 5, 6, 7)

  result <- roll_sum(x, 3, by = 2, align = "center")

  expect_equal(result, c(NA, 6, NA, 12, NA, 18, NA))
})

test_that("roll_sum with width 1 returns the input values", {
  x <- c(10, 20, 30, 40, 50)

  result <- roll_sum(x, 1, by = 1, align = "center")

  expect_equal(result, x)
})

test_that("roll_sum handles centered even widths correctly", {
  x <- c(1, 2, 3, 4, 5, 6)

  result <- roll_sum(x, 2, by = 1, align = "center")

  expect_equal(result, c(NA, 3, 5, 7, 9, 11))
})

test_that("roll_sum returns a single centered value when even width equals input length", {
  x <- c(1, 2, 3, 4)

  result <- roll_sum(x, 4, by = 1, align = "center")

  expect_equal(result, c(NA, NA, 10, NA))
})

test_that("roll_sum returns a single value for left alignment when width equals input length", {
  x <- c(1, 2, 3, 4)

  result <- roll_sum(x, 4, by = 1, align = "left")

  expect_equal(result, c(10, NA, NA, NA))
})

test_that("roll_sum returns a single value for right alignment when width equals input length", {
  x <- c(1, 2, 3, 4)

  result <- roll_sum(x, 4, by = 1, align = "right")

  expect_equal(result, c(NA, NA, NA, 10))
})

test_that("roll_sum handles negative values correctly", {
  x <- c(-2, -1, 0, 1, 2)

  result <- roll_sum(x, 3, by = 1, align = "center")

  expect_equal(result, c(NA, -3, 0, 3, NA))
})

test_that("roll_sum handles decimal values correctly", {
  x <- c(0.5, 1.5, 2.5, 3.5, 4.5)

  result <- roll_sum(x, 3, by = 1, align = "center")

  expect_equal(result, c(NA, 4.5, 7.5, 10.5, NA))
})
