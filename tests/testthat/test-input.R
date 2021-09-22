context("input")

test_that("bad input is detected", {

  functions <- c("max", "mean", "median", "min", "prod", "sd", "sum", "var")

  run_tests <- function(
    x,
    width = 5,
    by = 1,
    align = "center"
  ) {
    for (f in functions) {
      MRU_FUN <- get(paste("roll", f, sep = "_"), envir = asNamespace("MazamaRollUtils"))
      expect_error(MRU_FUN(x, width, by , align))
    }
  }

  x <- rnorm(50)

  # Argument validation in all functions
  run_tests(x = "abcd")
  run_tests(x, width = "abcd")
  run_tests(x, width = 0)
  run_tests(x, width = 1e6)
  run_tests(x, by = "abcd")
  run_tests(x, by = 0)
  run_tests(x, align = 1)
  run_tests(x, align = "forward")

  # Only roll_mean() has weights
  expect_error(roll_mean(x, width = 3, weights = "abcd"))
  expect_error(roll_mean(x, width = 3, weights = c(1)))
  expect_error(roll_mean(x, width = 3, weights = c(1,1,1,1,1)))
  expect_error(roll_mean(x, width = 3, weights = c(-1,1,1)))
  expect_error(roll_mean(x, width = 3, weights = c(0,0,0)))

})

