test_that("roll_hampel returns a vector the same length as input", {
  x <- c(1, 2, 100, 4, 5, -100, 7, 8, 9)

  result <- roll_hampel(x, 3)

  expect_length(result, length(x))
})

test_that("roll_hampel returns NA at incomplete boundary windows", {
  x <- c(1, 2, 100, 4, 5, -100, 7, 8, 9)

  result <- roll_hampel(x, 3)

  expect_true(is.na(result[1]))
  expect_true(is.na(result[length(x)]))
})

test_that("roll_hampel returns zero for a constant series", {
  x <- rep(5, 9)

  result <- roll_hampel(x, 3)

  expect_equal(result, c(NA, 0, 0, 0, 0, 0, 0, 0, NA))
})

test_that("roll_hampel gives small scores for non-outliers and large scores for obvious outliers", {
  hampel_threshold <- 10
  x <- c(1, 2, 100, 4, 5, -100, 7, 8, 9)

  result <- roll_hampel(x, 3)

  expect_gt(result[3], hampel_threshold)
  expect_gt(result[6], hampel_threshold)

  expect_lt(result[2], hampel_threshold)
  expect_lt(result[4], hampel_threshold)
  expect_lt(result[5], hampel_threshold)
  expect_lt(result[7], hampel_threshold)
  expect_lt(result[8], hampel_threshold)
})

test_that("roll_hampel matches known values for a simple example", {
  x <- c(1, 2, 100, 4, 5, -100, 7, 8, 9)

  result <- roll_hampel(x, 3)

  expected <- c(
    NA,
    0.0000000,
    32.3755565,
    0.6744908,
    0.6744908,
    35.4107649,
    0.0000000,
    0.0000000,
    NA
  )

  expect_equal(result, expected, tolerance = 1e-7)
})

test_that("roll_hampel returns Inf when MAD is zero but the center value differs", {
  x <- c(5, 5, 100, 5, 5)

  result <- roll_hampel(x, 3)

  expect_true(is.infinite(result[3]))
})

test_that("roll_hampel returns zero when MAD is zero and the center value matches the neighborhood", {
  x <- c(5, 5, 5, 5, 5)

  result <- roll_hampel(x, 3)

  expect_equal(result, c(NA, 0, 0, 0, NA))
})

test_that("roll_hampel does not flag a smooth monotone series as outliers", {
  x <- 1:9

  result <- roll_hampel(x, 3)

  expect_true(all(result[2:8] < 10))
})

test_that("roll_hampel propagates NA when na.rm = FALSE and a window contains missing values", {
  x <- c(1, 2, NA, 4, 5, 6, 7)

  result <- roll_hampel(x, 3, na.rm = FALSE)

  expect_true(any(is.na(result)))
})

test_that("roll_hampel can compute finite scores with na.rm = TRUE when valid values remain", {
  x <- c(1, 2, NA, 4, 5, 6, 7)

  result <- roll_hampel(x, 3, na.rm = TRUE)

  expect_length(result, length(x))
  expect_true(any(is.finite(result), na.rm = TRUE))
})
