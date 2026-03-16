test_that("roll_var returns expected values for a simple centered example", {
  x <- c(1, 2, 3, 4, 5)

  result <- roll_var(x, 3, by = 1, align = "center")

  expect_equal(result, c(NA, 1, 1, 1, NA))
})

test_that("roll_var returns expected values for left alignment", {
  x <- c(1, 2, 3, 4, 5)

  result <- roll_var(x, 3, by = 1, align = "left")

  expect_equal(result, c(1, 1, 1, NA, NA))
})

test_that("roll_var returns expected values for right alignment", {
  x <- c(1, 2, 3, 4, 5)

  result <- roll_var(x, 3, by = 1, align = "right")

  expect_equal(result, c(NA, NA, 1, 1, 1))
})

test_that("roll_var respects the by argument", {
  x <- c(1, 2, 3, 4, 5, 6, 7)

  result <- roll_var(x, 3, by = 2, align = "center")

  expect_equal(result, c(NA, 1, NA, 1, NA, 1, NA))
})

test_that("roll_var with width 1 returns NA for every position", {
  x <- c(10, 20, 30, 40, 50)

  result <- roll_var(x, 1, by = 1, align = "center")

  expect_true(all(is.na(result)))
})

test_that("roll_var handles centered even widths correctly", {
  x <- c(1, 2, 3, 4, 5, 6)

  result <- roll_var(x, 2, by = 1, align = "center")

  expect_equal(
    result,
    c(NA, var(c(1, 2)), var(c(2, 3)), var(c(3, 4)), var(c(4, 5)), var(c(5, 6)))
  )
})

test_that("roll_var returns a single centered value when even width equals input length", {
  x <- c(1, 2, 3, 4)

  result <- roll_var(x, 4, by = 1, align = "center")

  expect_equal(result, c(NA, NA, var(x), NA))
})

test_that("roll_var returns a single value for left alignment when width equals input length", {
  x <- c(1, 2, 3, 4)

  result <- roll_var(x, 4, by = 1, align = "left")

  expect_equal(result, c(var(x), NA, NA, NA))
})

test_that("roll_var returns a single value for right alignment when width equals input length", {
  x <- c(1, 2, 3, 4)

  result <- roll_var(x, 4, by = 1, align = "right")

  expect_equal(result, c(NA, NA, NA, var(x)))
})

test_that("roll_var returns zero for a constant series", {
  x <- rep(5, 7)

  result <- roll_var(x, 3, by = 1, align = "center")

  expect_equal(result, c(NA, 0, 0, 0, 0, 0, NA))
})

test_that("roll_var handles negative values correctly", {
  x <- c(-2, -1, 0, 1, 2)

  result <- roll_var(x, 3, by = 1, align = "center")

  expect_equal(result, c(NA, 1, 1, 1, NA))
})

test_that("roll_var is affected by an outlier", {
  x <- c(1, 2, 100, 4, 5)

  result <- roll_var(x, 3, by = 1, align = "center")

  expect_gt(result[2], 1)
  expect_gt(result[3], 1)
  expect_gt(result[4], 1)
})

test_that("roll_var matches roll_sd squared", {
  x <- c(1, 2, 3, 4, 5, 6, 7)

  expect_equal(
    roll_var(x, 3, by = 1, align = "center"),
    roll_sd(x, 3, by = 1, align = "center")^2
  )
})
