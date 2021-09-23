context("Median Absolute Deviation")

test_that("roll_MAD works", {

  # See:  https://en.wikipedia.org/wiki/Median_absolute_deviation
  x <- c(NA, NA, NA, 1, 1, 2, 2, 4, 6, 9, NA, NA, NA)
  expect_equal(
    roll_MAD(x, 7),
    c(NA, NA, NA, NA, NA, NA, 1, NA, NA, NA, NA, NA, NA)
  )

  # More examples
  x <- c(0, 0, 0, 1, 1, 2, 2, 4, 6, 9, 0, 0, 0)

  expect_equal(
    roll_MAD(x, 1),
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )

  expect_equal(
    roll_MAD(x, 3),
    c(NA, 0, 0, 0, 0, 0, 0, 2, 2, 3, 0, 0, NA)
  )

  expect_equal(
    roll_MAD(x, 5),
    c(NA, NA, 0, 1, 1, 1, 1, 2, 2, 4, 0, NA, NA)
  )


})

