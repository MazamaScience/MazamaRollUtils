# These tests are modeled on the tests in the RcppRoll test/ directory
#   https://github.com/kevinushey/RcppRoll

test_that("MazamaRollUtils matches zoo::rollapply for width = 1, center aligned", {
  skip_if_not_installed("zoo")

  set.seed(1)
  x <- rnorm(50)

  expect_equal(
    roll_max(x, 1, by = 1, align = "center"),
    zoo::rollapply(x, 1, FUN = max, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_mean(x, 1, by = 1, align = "center"),
    zoo::rollapply(x, 1, FUN = mean, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_median(x, 1, by = 1, align = "center"),
    zoo::rollapply(x, 1, FUN = median, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_min(x, 1, by = 1, align = "center"),
    zoo::rollapply(x, 1, FUN = min, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_prod(x, 1, by = 1, align = "center"),
    zoo::rollapply(x, 1, FUN = prod, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_sd(x, 1, by = 1, align = "center"),
    zoo::rollapply(x, 1, FUN = sd, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_sum(x, 1, by = 1, align = "center"),
    zoo::rollapply(x, 1, FUN = sum, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_var(x, 1, by = 1, align = "center"),
    zoo::rollapply(x, 1, FUN = var, by = 1, fill = NA, align = "center")
  )
})

test_that("MazamaRollUtils matches zoo::rollapply for width = 5, center aligned", {
  skip_if_not_installed("zoo")

  set.seed(2)
  x <- rnorm(50)

  expect_equal(
    roll_max(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = max, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_mean(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = mean, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_median(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = median, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_min(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = min, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_prod(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = prod, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_sd(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = sd, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_sum(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = sum, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_var(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = var, by = 1, fill = NA, align = "center")
  )
})

test_that("MazamaRollUtils matches zoo::rollapply for width = 49, center aligned", {
  skip_if_not_installed("zoo")

  set.seed(3)
  x <- rnorm(50)

  expect_equal(
    roll_max(x, 49, by = 1, align = "center"),
    zoo::rollapply(x, 49, FUN = max, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_mean(x, 49, by = 1, align = "center"),
    zoo::rollapply(x, 49, FUN = mean, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_median(x, 49, by = 1, align = "center"),
    zoo::rollapply(x, 49, FUN = median, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_min(x, 49, by = 1, align = "center"),
    zoo::rollapply(x, 49, FUN = min, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_prod(x, 49, by = 1, align = "center"),
    zoo::rollapply(x, 49, FUN = prod, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_sd(x, 49, by = 1, align = "center"),
    zoo::rollapply(x, 49, FUN = sd, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_sum(x, 49, by = 1, align = "center"),
    zoo::rollapply(x, 49, FUN = sum, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_var(x, 49, by = 1, align = "center"),
    zoo::rollapply(x, 49, FUN = var, by = 1, fill = NA, align = "center")
  )
})

test_that("MazamaRollUtils matches zoo::rollapply for very small numbers", {
  skip_if_not_installed("zoo")

  set.seed(4)
  x <- rnorm(1000)^100

  expect_equal(
    roll_max(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = max, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_mean(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = mean, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_median(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = median, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_min(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = min, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_prod(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = prod, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_sd(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = sd, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_sum(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = sum, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_var(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = var, by = 1, fill = NA, align = "center")
  )
})

test_that("MazamaRollUtils matches zoo::rollapply for very large numbers", {
  skip_if_not_installed("zoo")

  set.seed(5)
  x <- rnorm(1000, mean = 1e200, sd = 1e201)

  expect_equal(
    roll_max(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = max, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_mean(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = mean, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_median(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = median, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_min(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = min, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_prod(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = prod, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_sd(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = sd, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_sum(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = sum, by = 1, fill = NA, align = "center")
  )
  expect_equal(
    roll_var(x, 5, by = 1, align = "center"),
    zoo::rollapply(x, 5, FUN = var, by = 1, fill = NA, align = "center")
  )
})

test_that("MazamaRollUtils matches zoo::rollapply for left alignment and by = 1", {
  skip_if_not_installed("zoo")

  set.seed(6)
  x <- rnorm(100, mean = 100, sd = 50)

  expect_equal(
    roll_max(x, 9, by = 1, align = "left"),
    zoo::rollapply(x, 9, FUN = max, by = 1, fill = NA, align = "left")
  )
  expect_equal(
    roll_mean(x, 9, by = 1, align = "left"),
    zoo::rollapply(x, 9, FUN = mean, by = 1, fill = NA, align = "left")
  )
  expect_equal(
    roll_median(x, 9, by = 1, align = "left"),
    zoo::rollapply(x, 9, FUN = median, by = 1, fill = NA, align = "left")
  )
  expect_equal(
    roll_min(x, 9, by = 1, align = "left"),
    zoo::rollapply(x, 9, FUN = min, by = 1, fill = NA, align = "left")
  )
  expect_equal(
    roll_prod(x, 9, by = 1, align = "left"),
    zoo::rollapply(x, 9, FUN = prod, by = 1, fill = NA, align = "left")
  )
  expect_equal(
    roll_sd(x, 9, by = 1, align = "left"),
    zoo::rollapply(x, 9, FUN = sd, by = 1, fill = NA, align = "left")
  )
  expect_equal(
    roll_sum(x, 9, by = 1, align = "left"),
    zoo::rollapply(x, 9, FUN = sum, by = 1, fill = NA, align = "left")
  )
  expect_equal(
    roll_var(x, 9, by = 1, align = "left"),
    zoo::rollapply(x, 9, FUN = var, by = 1, fill = NA, align = "left")
  )
})

test_that("MazamaRollUtils matches zoo::rollapply for center alignment and by = 2", {
  skip_if_not_installed("zoo")

  set.seed(7)
  x <- rnorm(100, mean = 100, sd = 50)

  expect_equal(
    roll_max(x, 9, by = 2, align = "center"),
    zoo::rollapply(x, 9, FUN = max, by = 2, fill = NA, align = "center")
  )
  expect_equal(
    roll_mean(x, 9, by = 2, align = "center"),
    zoo::rollapply(x, 9, FUN = mean, by = 2, fill = NA, align = "center")
  )
  expect_equal(
    roll_median(x, 9, by = 2, align = "center"),
    zoo::rollapply(x, 9, FUN = median, by = 2, fill = NA, align = "center")
  )
  expect_equal(
    roll_min(x, 9, by = 2, align = "center"),
    zoo::rollapply(x, 9, FUN = min, by = 2, fill = NA, align = "center")
  )
  expect_equal(
    roll_prod(x, 9, by = 2, align = "center"),
    zoo::rollapply(x, 9, FUN = prod, by = 2, fill = NA, align = "center")
  )
  expect_equal(
    roll_sd(x, 9, by = 2, align = "center"),
    zoo::rollapply(x, 9, FUN = sd, by = 2, fill = NA, align = "center")
  )
  expect_equal(
    roll_sum(x, 9, by = 2, align = "center"),
    zoo::rollapply(x, 9, FUN = sum, by = 2, fill = NA, align = "center")
  )
  expect_equal(
    roll_var(x, 9, by = 2, align = "center"),
    zoo::rollapply(x, 9, FUN = var, by = 2, fill = NA, align = "center")
  )
})

test_that("MazamaRollUtils matches zoo::rollapply for right alignment and by = 5", {
  skip_if_not_installed("zoo")

  set.seed(8)
  x <- rnorm(100, mean = 100, sd = 50)

  expect_equal(
    roll_max(x, 9, by = 5, align = "right"),
    zoo::rollapply(x, 9, FUN = max, by = 5, fill = NA, align = "right")
  )
  expect_equal(
    roll_mean(x, 9, by = 5, align = "right"),
    zoo::rollapply(x, 9, FUN = mean, by = 5, fill = NA, align = "right")
  )
  expect_equal(
    roll_median(x, 9, by = 5, align = "right"),
    zoo::rollapply(x, 9, FUN = median, by = 5, fill = NA, align = "right")
  )
  expect_equal(
    roll_min(x, 9, by = 5, align = "right"),
    zoo::rollapply(x, 9, FUN = min, by = 5, fill = NA, align = "right")
  )
  expect_equal(
    roll_prod(x, 9, by = 5, align = "right"),
    zoo::rollapply(x, 9, FUN = prod, by = 5, fill = NA, align = "right")
  )
  expect_equal(
    roll_sd(x, 9, by = 5, align = "right"),
    zoo::rollapply(x, 9, FUN = sd, by = 5, fill = NA, align = "right")
  )
  expect_equal(
    roll_sum(x, 9, by = 5, align = "right"),
    zoo::rollapply(x, 9, FUN = sum, by = 5, fill = NA, align = "right")
  )
  expect_equal(
    roll_var(x, 9, by = 5, align = "right"),
    zoo::rollapply(x, 9, FUN = var, by = 5, fill = NA, align = "right")
  )
})

test_that("MazamaRollUtils matches zoo::rollapply when input contains NA values", {
  skip_if_not_installed("zoo")

  set.seed(9)
  x <- rnorm(100, mean = 100, sd = 50)
  x[sample(length(x), length(x) / 3)] <- NA_real_

  expect_equal(
    roll_max(x, 9, by = 2, align = "center"),
    zoo::rollapply(x, 9, FUN = max, by = 2, fill = NA, align = "center")
  )
  expect_equal(
    roll_mean(x, 9, by = 2, align = "center"),
    zoo::rollapply(x, 9, FUN = mean, by = 2, fill = NA, align = "center")
  )
  expect_equal(
    roll_median(x, 9, by = 2, align = "center"),
    zoo::rollapply(x, 9, FUN = median, by = 2, fill = NA, align = "center")
  )
  expect_equal(
    roll_min(x, 9, by = 2, align = "center"),
    zoo::rollapply(x, 9, FUN = min, by = 2, fill = NA, align = "center")
  )
  expect_equal(
    roll_prod(x, 9, by = 2, align = "center"),
    zoo::rollapply(x, 9, FUN = prod, by = 2, fill = NA, align = "center")
  )
  expect_equal(
    roll_sd(x, 9, by = 2, align = "center"),
    zoo::rollapply(x, 9, FUN = sd, by = 2, fill = NA, align = "center")
  )
  expect_equal(
    roll_sum(x, 9, by = 2, align = "center"),
    zoo::rollapply(x, 9, FUN = sum, by = 2, fill = NA, align = "center")
  )
  expect_equal(
    roll_var(x, 9, by = 2, align = "center"),
    zoo::rollapply(x, 9, FUN = var, by = 2, fill = NA, align = "center")
  )
})
