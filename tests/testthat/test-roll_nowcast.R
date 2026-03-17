test_that("roll_nowcast works properly", {
  # Data from:
  # https://forum.airnowtech.org/t/the-nowcast-for-pm2-5-and-pm10/172
  # Answer should be 28.4 ug/m3 or 85 AQI

  pm25 <- c(34.9, 43, 50, 64.9, 69.2, 66.2, 53.7, 48.6, 49.2, 35, NA, 21)
  nowcast <- roll_nowcast(pm25)

  expect_length(nowcast, length(pm25))
  expect_true(is.na(nowcast[1]))
  expect_equal(nowcast[12], 28.4)
})

test_that("roll_nowcast handles missing values properly", {
  pm25 <- c(34.9, 43, 50, 64.9, 69.2, 66.2, 53.7, 48.6, 49.2, 35, NA, NA)

  expect_true(is.na(roll_nowcast(pm25)[12]))
})

test_that("roll_nowcast returns all NA if all input is NA", {
  pm25 <- rep(NA_real_, 12)
  nowcast <- roll_nowcast(pm25)

  expect_length(nowcast, 12)
  expect_true(all(is.na(nowcast)))
})

test_that("roll_nowcast returns average for flat data", {
  pm25 <- rep(20, 12)
  nowcast <- roll_nowcast(pm25)

  expect_equal(nowcast[12], 20)
})

test_that("roll_nowcast values are rounded to one decimal place", {
  pm25 <- rep(20.12345, 12)
  nowcast <- roll_nowcast(pm25)

  expect_equal(nowcast[12], 20.1)
})

test_that("roll_nowcast returns a vector the same length as x", {
  x <- c(10, 12, 14, 16, 18)

  result <- roll_nowcast(x)

  expect_type(result, "double")
  expect_length(result, length(x))
})

test_that("roll_nowcast uses right-aligned partial windows at the beginning", {
  x <- c(10, 20, 30)

  result <- roll_nowcast(x)

  # First value has only one recent valid hour, so should be NA
  expect_true(is.na(result[1]))

  # Second value has two valid values in the most recent 3 hours
  expect_false(is.na(result[2]))

  # Third value should also be calculable
  expect_false(is.na(result[3]))
})

test_that("roll_nowcast requires at least 2 valid values in the most recent 3 hours", {
  x <- c(10, NA, NA, 20)

  result <- roll_nowcast(x)

  # At index 4, the most recent 3 hours are NA, NA, 20 -> only one valid value
  expect_true(is.na(result[4]))
})

test_that("roll_nowcast rejects non-numeric input", {
  expect_error(
    roll_nowcast(c("a", "b", "c")),
    "'x' must be a numeric vector."
  )
})
