test_that("functions require numeric input", {
  x_chr <- c("a", "b", "c", "d", "e")
  x_lgl <- c(TRUE, FALSE, TRUE, FALSE, TRUE)

  expect_error(roll_mean(x_chr, 3))
  expect_error(roll_sum(x_chr, 3))
  expect_error(roll_min(x_chr, 3))
  expect_error(roll_max(x_chr, 3))
  expect_error(roll_prod(x_chr, 3))
  expect_error(roll_median(x_chr, 3))
  expect_error(roll_var(x_chr, 3))
  expect_error(roll_sd(x_chr, 3))
  expect_error(roll_MAD(x_chr, 3))
  expect_error(roll_hampel(x_chr, 3))

  expect_error(roll_mean(x_lgl, 3))
  expect_error(roll_sum(x_lgl, 3))
  expect_error(roll_min(x_lgl, 3))
  expect_error(roll_max(x_lgl, 3))
  expect_error(roll_prod(x_lgl, 3))
  expect_error(roll_median(x_lgl, 3))
  expect_error(roll_var(x_lgl, 3))
  expect_error(roll_sd(x_lgl, 3))
  expect_error(roll_MAD(x_lgl, 3))
  expect_error(roll_hampel(x_lgl, 3))
})

test_that("functions require width to be a single positive integer not larger than input length", {
  x <- 1:10

  expect_error(roll_mean(x, 0))
  expect_error(roll_mean(x, -1))
  expect_error(roll_mean(x, 2.5))
  expect_error(roll_mean(x, c(3, 5)))
  expect_error(roll_mean(x, NA_integer_))
  expect_error(roll_mean(x, 11))

  expect_error(roll_sum(x, 0))
  expect_error(roll_sum(x, 11))
  expect_error(roll_min(x, 0))
  expect_error(roll_min(x, 11))
  expect_error(roll_max(x, 0))
  expect_error(roll_max(x, 11))
  expect_error(roll_prod(x, 0))
  expect_error(roll_prod(x, 11))
  expect_error(roll_median(x, 0))
  expect_error(roll_median(x, 11))
  expect_error(roll_var(x, 0))
  expect_error(roll_var(x, 11))
  expect_error(roll_sd(x, 0))
  expect_error(roll_sd(x, 11))
  expect_error(roll_MAD(x, 0))
  expect_error(roll_MAD(x, 11))
  expect_error(roll_hampel(x, 0))
  expect_error(roll_hampel(x, 11))
})

test_that("width greater than input length gives a clear error", {
  x <- 1:5
  expect_error(
    roll_mean(x, 10),
    "cannot be larger than 'x'"
  )
})

test_that("functions return vectors of the same length as input", {
  x <- 1:10

  expect_length(roll_mean(x, 3), length(x))
  expect_length(roll_sum(x, 3), length(x))
  expect_length(roll_min(x, 3), length(x))
  expect_length(roll_max(x, 3), length(x))
  expect_length(roll_prod(x, 3), length(x))
  expect_length(roll_median(x, 3), length(x))
  expect_length(roll_var(x, 3), length(x))
  expect_length(roll_sd(x, 3), length(x))
  expect_length(roll_MAD(x, 3), length(x))
  expect_length(roll_hampel(x, 3), length(x))
})

test_that("functions accept integer and double input", {
  x_int <- 1:10
  x_dbl <- as.numeric(1:10)

  expect_no_error(roll_mean(x_int, 3))
  expect_no_error(roll_mean(x_dbl, 3))

  expect_no_error(roll_sum(x_int, 3))
  expect_no_error(roll_sum(x_dbl, 3))

  expect_no_error(roll_min(x_int, 3))
  expect_no_error(roll_min(x_dbl, 3))

  expect_no_error(roll_max(x_int, 3))
  expect_no_error(roll_max(x_dbl, 3))

  expect_no_error(roll_prod(x_int, 3))
  expect_no_error(roll_prod(x_dbl, 3))

  expect_no_error(roll_median(x_int, 3))
  expect_no_error(roll_median(x_dbl, 3))

  expect_no_error(roll_var(x_int, 3))
  expect_no_error(roll_var(x_dbl, 3))

  expect_no_error(roll_sd(x_int, 3))
  expect_no_error(roll_sd(x_dbl, 3))

  expect_no_error(roll_MAD(x_int, 3))
  expect_no_error(roll_MAD(x_dbl, 3))

  expect_no_error(roll_hampel(x_int, 3))
  expect_no_error(roll_hampel(x_dbl, 3))
})

test_that("missing values in width are rejected", {
  x <- 1:10

  expect_error(roll_mean(x, NA))
  expect_error(roll_sum(x, NA))
  expect_error(roll_min(x, NA))
  expect_error(roll_max(x, NA))
  expect_error(roll_prod(x, NA))
  expect_error(roll_median(x, NA))
  expect_error(roll_var(x, NA))
  expect_error(roll_sd(x, NA))
  expect_error(roll_MAD(x, NA))
  expect_error(roll_hampel(x, NA))
})
