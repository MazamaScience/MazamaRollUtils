test_that("roll_mean accepts valid weights", {
  x <- 1:5

  expect_no_error(roll_mean(x, 3, weights = c(1, 1, 1)))
  expect_no_error(roll_mean(x, 3, weights = c(1, 2, 1)))
  expect_no_error(roll_mean(x, 3, weights = c(0.2, 0.6, 0.2)))
})

test_that("roll_mean requires weights to have length equal to width", {
  x <- 1:5

  expect_error(roll_mean(x, 3, weights = c(1, 1)))
  expect_error(roll_mean(x, 3, weights = c(1, 1, 1, 1)))
})

test_that("roll_mean requires numeric weights", {
  x <- 1:5

  expect_error(roll_mean(x, 3, weights = c("a", "b", "c")))
  expect_error(roll_mean(x, 3, weights = c(TRUE, FALSE, TRUE)))
})

test_that("roll_mean rejects missing or invalid weights", {
  x <- 1:5

  expect_error(roll_mean(x, 3, weights = NA))
  expect_error(roll_mean(x, 3, weights = c(1, NA, 1)))
  expect_error(roll_mean(x, 3, weights = list(1, 1, 1)))
})

test_that("uniform weights give the same result as unweighted roll_mean", {
  x <- c(1, 2, 3, 4, 5)

  expect_equal(
    roll_mean(x, 3),
    roll_mean(x, 3, weights = c(1, 1, 1))
  )
})

test_that("weighted roll_mean changes results when weights are non-uniform", {
  x <- c(1, 2, 10, 4, 5)

  expect_false(identical(
    roll_mean(x, 3),
    roll_mean(x, 3, weights = c(1, 2, 1))
  ))
})

test_that("weighted roll_mean matches hand-calculated values for complete windows", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(1, 2, 1)

  result <- roll_mean(x, 3, weights = w)

  expected <- c(
    NA,
    (1 * 1 + 2 * 2 + 3 * 1) / 4,
    (2 * 1 + 3 * 2 + 4 * 1) / 4,
    (3 * 1 + 4 * 2 + 5 * 1) / 4,
    NA
  )

  expect_equal(result, expected)
})

test_that("zero weights can select the center element for complete windows", {
  x <- c(1, 2, 3, 4, 5)

  expect_equal(
    roll_mean(x, 3, weights = c(0, 1, 0)),
    c(NA, 2, 3, 4, NA)
  )
})

