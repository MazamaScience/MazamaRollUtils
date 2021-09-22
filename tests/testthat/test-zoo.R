# These tests are modeled on the tests in the RcppRoll test/ directory
#   https://github.com/kevinushey/RcppRoll

context("zoo comparison")

test_that("we match results from zoo::rollapply", {

  if (!requireNamespace("zoo", quietly = TRUE))
    skip("zoo not installed")

  functions <- c("max", "median", "mean", "min", "prod", "sd", "sum", "var")

  run_tests <- function(
    x,
    n = 5,
    by = 1,
    align = "center",
    functions
  ) {
    for (f in functions) {
      zoo_result <- zoo::rollapply(x, n, FUN = get(f), by = by, fill = NA, align = align)
      MRU_FUN <- get(paste("roll", f, sep = "_"), envir = asNamespace("MazamaRollUtils"))
      MRU_result <- MRU_FUN(x, n, by , align)
      expect_equal(MRU_result, zoo_result)
    }
  }

  x <- rnorm(50)

  run_tests(x, 1, by = 1, align = "center", functions = functions)
  run_tests(x, 5, by = 1, align = "center", functions = functions)
  run_tests(x, 49, by = 1, align = "center", functions = functions)

  # NOTE:  MazamaRollUtils returns all NA when width = 50 as there is no index
  # NOTE:  at the middle of the window. This is different from zoo::rollapply()

  # Test with small numbers
  x <- rnorm(1E3) ^ 100
  run_tests(x, 5, by = 1, align = "center", functions = functions)

  # Test with large numbers
  x <- rnorm(1E3, mean = 1E200, sd = 1E201)
  run_tests(x, 5, by = 1, align = "center", functions = functions)

  # Try out different widths and alignments
  args <- expand.grid(
    n = c(3, 9, 99),
    by = c(1, 2, 5),
    align = c("left", "center", "right")
  )

  x <- rnorm(100, 100, 50)
  for (i in 1:nrow(args)) {
    run_tests(
      x,
      n = args$n[[i]],
      by = args$by[[i]],
      align = as.character(args$align[[i]]),
      functions = functions
    )
  }

  # Make sure we properly handle NAs
  functions <- c("max", "median", "mean", "min", "prod", "sum")

  x[sample(length(x), length(x) / 3)] <- NA
  for ( i in 1:nrow(args) ) {
      run_tests(
        x,
        n = args$n[[i]],
        by = args$by[[i]],
        align = as.character(args$align[[i]]),
        functions = functions
      )
  }

})
