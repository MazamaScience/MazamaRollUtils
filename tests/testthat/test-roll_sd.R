test_that("roll_sd returns expected values for a simple centered example", {
  x <- c(1, 2, 3, 4, 5)

  result <- roll_sd(x, 3, by = 1, align = "center")

  expect_equal(result, c(NA, 1, 1, 1, NA))
})

test_that("roll_sd returns expected values for left alignment", {
  x <- c(1, 2, 3, 4, 5)

  result <- roll_sd(x, 3, by = 1, align = "left")

  expect_equal(result, c(1, 1, 1, NA, NA))
})

test_that("roll_sd returns expected values for right alignment", {
  x <- c(1, 2, 3, 4, 5)

  result <- roll_sd(x, 3, by = 1, align = "right")

  expect_equal(result, c(NA, NA, 1, 1, 1))
})

test_that("roll_sd respects the by argument", {
  x <- c(1, 2, 3, 4, 5, 6, 7)

  result <- roll_sd(x, 3, by = 2, align = "center")

  expect_equal(result, c(NA, 1, NA, 1, NA, 1, NA))
})

test_that("roll_sd with width 1 returns NA for every position", {
  x <- c(10, 20, 30, 40, 50)

  result <- roll_sd(x, 1, by = 1, align = "center")

  expect_true(all(is.na(result)))
})

test_that("roll_sd handles centered even widths correctly", {
  x <- c(1, 2, 3, 4, 5, 6)

  result <- roll_sd(x, 2, by = 1, align = "center")

  expect_equal(
    result,
    c(NA, sd(c(1, 2)), sd(c(2, 3)), sd(c(3, 4)), sd(c(4, 5)), sd(c(5, 6)))
  )
})

test_that("roll_sd returns a single centered value when even width equals input length", {
  x <- c(1, 2, 3, 4)

  result <- roll_sd(x, 4, by = 1, align = "center")

  expect_equal(result, c(NA, NA, sd(x), NA))
})

test_that("roll_sd returns a single value for left alignment when width equals input length", {
  x <- c(1, 2, 3, 4)

  result <- roll_sd(x, 4, by = 1, align = "left")

  expect_equal(result, c(sd(x), NA, NA, NA))
})

test_that("roll_sd returns a single value for right alignment when width equals input length", {
  x <- c(1, 2, 3, 4)

  result <- roll_sd(x, 4, by = 1, align = "right")

  expect_equal(result, c(NA, NA, NA, sd(x)))
})

test_that("roll_sd returns zero for a constant series", {
  x <- rep(5, 7)

  result <- roll_sd(x, 3, by = 1, align = "center")

  expect_equal(result, c(NA, 0, 0, 0, 0, 0, NA))
})

test_that("roll_sd handles negative values correctly", {
  x <- c(-2, -1, 0, 1, 2)

  result <- roll_sd(x, 3, by = 1, align = "center")

  expect_equal(result, c(NA, 1, 1, 1, NA))
})

test_that("roll_sd is affected by an outlier", {
  x <- c(1, 2, 100, 4, 5)

  result <- roll_sd(x, 3, by = 1, align = "center")

  expect_gt(result[2], 1)
  expect_gt(result[3], 1)
  expect_gt(result[4], 1)
})
