test_that("na.rm = FALSE propagates NA when a window contains missing values", {
  x <- c(1, 2, NA, 4, 5)

  expect_true(any(is.na(roll_mean(x, 3, na.rm = FALSE))))
  expect_true(any(is.na(roll_sum(x, 3, na.rm = FALSE))))
  expect_true(any(is.na(roll_min(x, 3, na.rm = FALSE))))
  expect_true(any(is.na(roll_max(x, 3, na.rm = FALSE))))
  expect_true(any(is.na(roll_prod(x, 3, na.rm = FALSE))))
  expect_true(any(is.na(roll_median(x, 3, na.rm = FALSE))))
  #expect_true(any(is.na(roll_var(x, 3, na.rm = FALSE))))
  # expect_true(any(is.na(roll_sd(x, 3, na.rm = FALSE))))
  expect_true(any(is.na(roll_MAD(x, 3, na.rm = FALSE))))
  expect_true(any(is.na(roll_hampel(x, 3, na.rm = FALSE))))
})

test_that("na.rm = TRUE removes NA when valid values remain in the window", {
  x <- c(1, 2, NA, 4, 5)

  expect_false(all(is.na(roll_mean(x, 3, na.rm = TRUE))))
  expect_false(all(is.na(roll_sum(x, 3, na.rm = TRUE))))
  expect_false(all(is.na(roll_min(x, 3, na.rm = TRUE))))
  expect_false(all(is.na(roll_max(x, 3, na.rm = TRUE))))
  expect_false(all(is.na(roll_prod(x, 3, na.rm = TRUE))))
  expect_false(all(is.na(roll_median(x, 3, na.rm = TRUE))))
  #expect_false(all(is.na(roll_var(x, 3, na.rm = TRUE))))
  # expect_false(all(is.na(roll_sd(x, 3, na.rm = TRUE))))
  expect_false(all(is.na(roll_MAD(x, 3, na.rm = TRUE))))
  expect_false(all(is.na(roll_hampel(x, 3, na.rm = TRUE))))
})

test_that("na.rm = TRUE still returns NA when all values in the window are missing", {
  x <- c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)

  expect_true(all(is.na(roll_mean(x, 3, na.rm = TRUE))))
  expect_true(all(is.na(roll_sum(x, 3, na.rm = TRUE))))
  expect_true(all(is.na(roll_min(x, 3, na.rm = TRUE))))
  expect_true(all(is.na(roll_max(x, 3, na.rm = TRUE))))
  expect_true(all(is.na(roll_prod(x, 3, na.rm = TRUE))))
  expect_true(all(is.na(roll_median(x, 3, na.rm = TRUE))))
  #expect_true(all(is.na(roll_var(x, 3, na.rm = TRUE))))
  # expect_true(all(is.na(roll_sd(x, 3, na.rm = TRUE))))
  expect_true(all(is.na(roll_MAD(x, 3, na.rm = TRUE))))
  expect_true(all(is.na(roll_hampel(x, 3, na.rm = TRUE))))
})

test_that("na.rm affects results when NA values are present", {
  x <- c(1, 2, NA, 4, 5)

  expect_false(identical(
    roll_mean(x, 3, na.rm = FALSE),
    roll_mean(x, 3, na.rm = TRUE)
  ))
  expect_false(identical(
    roll_sum(x, 3, na.rm = FALSE),
    roll_sum(x, 3, na.rm = TRUE)
  ))
  expect_false(identical(
    roll_min(x, 3, na.rm = FALSE),
    roll_min(x, 3, na.rm = TRUE)
  ))
  expect_false(identical(
    roll_max(x, 3, na.rm = FALSE),
    roll_max(x, 3, na.rm = TRUE)
  ))
  expect_false(identical(
    roll_prod(x, 3, na.rm = FALSE),
    roll_prod(x, 3, na.rm = TRUE)
  ))
  expect_false(identical(
    roll_median(x, 3, na.rm = FALSE),
    roll_median(x, 3, na.rm = TRUE)
  ))
  # expect_false(identical(
  #   roll_var(x, 3, na.rm = FALSE),
  #   roll_var(x, 3, na.rm = TRUE)
  # ))
  # expect_false(identical(
  #   roll_sd(x, 3, na.rm = FALSE),
  #   roll_sd(x, 3, na.rm = TRUE)
  # ))
  expect_false(identical(
    roll_MAD(x, 3, na.rm = FALSE),
    roll_MAD(x, 3, na.rm = TRUE)
  ))
  expect_false(identical(
    roll_hampel(x, 3, na.rm = FALSE),
    roll_hampel(x, 3, na.rm = TRUE)
  ))
})

test_that("na.rm has no effect when there are no missing values", {
  x <- 1:10

  expect_identical(
    roll_mean(x, 3, na.rm = FALSE),
    roll_mean(x, 3, na.rm = TRUE)
  )
  expect_identical(
    roll_sum(x, 3, na.rm = FALSE),
    roll_sum(x, 3, na.rm = TRUE)
  )
  expect_identical(
    roll_min(x, 3, na.rm = FALSE),
    roll_min(x, 3, na.rm = TRUE)
  )
  expect_identical(
    roll_max(x, 3, na.rm = FALSE),
    roll_max(x, 3, na.rm = TRUE)
  )
  expect_identical(
    roll_prod(x, 3, na.rm = FALSE),
    roll_prod(x, 3, na.rm = TRUE)
  )
  expect_identical(
    roll_median(x, 3, na.rm = FALSE),
    roll_median(x, 3, na.rm = TRUE)
  )
  # expect_identical(
  #   roll_var(x, 3, na.rm = FALSE),
  #   roll_var(x, 3, na.rm = TRUE)
  # )
  # expect_identical(
  #   roll_sd(x, 3, na.rm = FALSE),
  #   roll_sd(x, 3, na.rm = TRUE)
  # )
  expect_identical(
    roll_MAD(x, 3, na.rm = FALSE),
    roll_MAD(x, 3, na.rm = TRUE)
  )
  expect_identical(
    roll_hampel(x, 3, na.rm = FALSE),
    roll_hampel(x, 3, na.rm = TRUE)
  )
})

test_that("na.rm must be a single logical value", {
  x <- 1:10

  expect_error(roll_mean(x, 3, na.rm = NA))
  expect_error(roll_mean(x, 3, na.rm = c(TRUE, FALSE)))
  expect_error(roll_mean(x, 3, na.rm = 1))
  expect_error(roll_mean(x, 3, na.rm = "TRUE"))

  expect_error(roll_sum(x, 3, na.rm = NA))
  expect_error(roll_min(x, 3, na.rm = NA))
  expect_error(roll_max(x, 3, na.rm = NA))
  expect_error(roll_prod(x, 3, na.rm = NA))
  expect_error(roll_median(x, 3, na.rm = NA))
  # expect_error(roll_var(x, 3, na.rm = NA))
  # expect_error(roll_sd(x, 3, na.rm = NA))
  expect_error(roll_MAD(x, 3, na.rm = NA))
  expect_error(roll_hampel(x, 3, na.rm = NA))
})

test_that("roll_var and roll_sd do not accept na.rm", {
  x <- c(1, 2, NA, 4, 5)

  expect_error(roll_var(x, 3, na.rm = TRUE))
  expect_error(roll_sd(x, 3, na.rm = TRUE))
})

