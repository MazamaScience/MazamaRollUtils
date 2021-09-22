context("weights")

test_that("weights are normalized", {

  x <- rnorm(50)

  a <- roll_mean(x, 3, weights = c(1,2,1))
  b <- roll_mean(x, 3, weights = c(0.5,1,0.5))
  c <- roll_mean(x, 3, weights = c(.1,.2,.1))
  expect_equal(a, b)
  expect_equal(a, c)

})

test_that("weights are applied to the proper positions", {

  x <- rnorm(50)

  a <- roll_mean(x, 3, align = "left", weights = c(1,0,0))
  b <- roll_mean(x, 3, align = "center", weights = c(0,1,0))
  c <- roll_mean(x, 3, align = "right", weights = c(0,0,1))
  mask <- !is.na(a) & !is.na(b) & !is.na(c)
  # NOTE:  testthat complains when we expect_true(a[mask], b[mask])
  am <- a[mask]
  bm <- b[mask]
  cm <- c[mask]
  expect_true(all.equal(am, bm))
  expect_true(all.equal(am, cm))

})

