context("na.rm")

test_that("na removal is supported", {

  x <- c(1:4, NA, 6:10)

  # roll_max
  expect_equal(
    roll_max(x, 3, na.rm = FALSE),
    c(NA, 3, 4, NA, NA, NA, 8, 9, 10, NA)
  )
  expect_equal(
    roll_max(x, 3, na.rm = TRUE),
    c(NA, 3, 4, 4, 6, 7, 8, 9, 10, NA)
  )

  # roll_mean
  expect_equal(
    roll_mean(x, 3, na.rm = FALSE),
    c(NA, 2, 3, NA, NA, NA, 7, 8, 9, NA)
  )
  expect_equal(
    roll_mean(x, 3, na.rm = TRUE),
    c(NA, 2, 3, 2.33333, 3.33333, 4.33333, 7, 8, 9, NA),
    tolerance = .00001
  )

  # roll_median
  expect_equal(
    roll_median(x, 3, na.rm = FALSE),
    c(NA, 2, 3, NA, NA, NA, 7, 8, 9, NA)
  )
  expect_equal(
    roll_median(x, 3, na.rm = TRUE),
    c(NA, 2, 3, 4, NA, 6, 7, 8, 9, NA)
  )

  # roll_min
  expect_equal(
    roll_min(x, 3, na.rm = FALSE),
    c(NA, 1, 2, NA, NA, NA, 6, 7, 8, NA)
  )
  expect_equal(
    roll_min(x, 3, na.rm = TRUE),
    c(NA, 1, 2, 3, 4, 6, 6, 7, 8, NA) # TODO:  get x[5] to work
  )

  # roll_prod
  expect_equal(
    roll_prod(x, 3, na.rm = FALSE),
    c(NA, 6, 24, NA, NA, NA, 336, 504, 720, NA)
  )
  expect_equal(
    roll_prod(x, 3, na.rm = TRUE),
    c(NA, 6, 24, 12, 24, 42, 336, 504, 720, NA)
  )

  # roll_sum
  expect_equal(
    roll_sum(x, 3, na.rm = FALSE),
    c(NA, 6, 9, NA, NA, NA, 21, 24, 27, NA)
  )
  expect_equal(
    roll_sum(x, 3, na.rm = TRUE),
    c(NA, 6, 9, 7, 10, 13, 21, 24, 27, NA)
  )


})

