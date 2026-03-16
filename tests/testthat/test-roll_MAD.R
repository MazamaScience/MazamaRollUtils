test_that("roll_MAD returns a vector the same length as input", {
  x <- c(1, 2, 100, 4, 5, -100, 7, 8, 9)

  result <- roll_MAD(x, 3)

  expect_length(result, length(x))
})

test_that("roll_MAD returns NA at incomplete boundary windows", {
  x <- c(1, 2, 100, 4, 5, -100, 7, 8, 9)

  result <- roll_MAD(x, 3)

  expect_true(is.na(result[1]))
  expect_true(is.na(result[length(x)]))
})

test_that("roll_MAD returns zero for a constant series", {
  x <- rep(5, 9)

  result <- roll_MAD(x, 3)

  expect_equal(result, c(NA, 0, 0, 0, 0, 0, 0, 0, NA))
})

test_that("roll_MAD matches known values for a simple example", {
  x <- c(1, 2, 100, 4, 5, -100, 7, 8, 9)

  result <- roll_MAD(x, 3)

  expected <- c(
    NA,
    1,
    2,
    1,
    1,
    2,
    1,
    1,
    NA
  )

  expect_equal(result, expected)
})

test_that("roll_MAD is one for a smooth monotone series with width 3", {
  x <- 1:9

  result <- roll_MAD(x, 3)

  expect_equal(result, c(NA, 1, 1, 1, 1, 1, 1, 1, NA))
})

test_that("roll_MAD is robust to a single large outlier", {
  x <- c(1, 2, 100, 4, 5)

  result <- roll_MAD(x, 3)

  expect_equal(result, c(NA, 1, 2, 1, NA))
})

test_that("roll_MAD propagates NA when na.rm = FALSE and a window contains missing values", {
  x <- c(1, 2, NA, 4, 5, 6, 7)

  result <- roll_MAD(x, 3, na.rm = FALSE)

  expect_true(any(is.na(result)))
})

test_that("roll_MAD can compute values with na.rm = TRUE when valid values remain", {
  x <- c(1, 2, NA, 4, 5, 6, 7)

  result <- roll_MAD(x, 3, na.rm = TRUE)

  expect_length(result, length(x))
  expect_true(any(is.finite(result), na.rm = TRUE))
})

test_that("roll_MAD returns all NA when all values are missing and na.rm = TRUE", {
  x <- c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)

  result <- roll_MAD(x, 3, na.rm = TRUE)

  expect_true(all(is.na(result)))
})

test_that("roll_MAD matches Wikipedia example", {

  # See:  https://en.wikipedia.org/wiki/Median_absolute_deviation
  x <- c(NA, NA, NA, 1, 1, 2, 2, 4, 6, 9, NA, NA, NA)
  expect_equal(
    roll_MAD(x, 7),
    c(NA, NA, NA, NA, NA, NA, 1, NA, NA, NA, NA, NA, NA)
  )

})

