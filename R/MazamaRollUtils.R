#' Roll Hampel
#'
#' @description Apply a moving-window Hampel function to a numeric vector.
#'
#' @details
#'
#' The Hampel filter is a robust outlier detector using Median Absolute Deviation (MAD).
#'
#' For every index in the incoming vector `x`, a value is returned that
#' is the Hampel function of all values in `x` that fall within a window of width
#' `width`.
#'
#' The `align` parameter determines the alignment of the return value
#' within the window. Thus:
#'
#' \itemize{
#'   \item{`align = "left"   [*------]` will cause the returned vector to have width - 1 `NA` values at the right end.}
#'   \item{`align = "center" [---*---]` will cause the returned vector to have `NA` values at either end as needed for centered alignment.}
#'   \item{`align = "right"  [------*]` will cause the returned vector to have width - 1 `NA` values at the left end.}
#' }
#'
#' For large vectors, the `by` parameter can be used to force the window
#' to jump ahead `by` indices for the next calculation. Indices that are
#' skipped over will be assigned `NA` values so that the return vector still has
#' the same length as the incoming vector. This can dramatically speed up
#' calculations for high resolution time series data.
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by Integer shift by which the window is moved each iteration.
#' @param align Character position of the return value within the window. One of:
#' `"left" | "center" | "right"`.
#' @param na.rm Logical specifying whether `NA` values should be removed
#' before the calculations within each window.
#'
#' @return Numeric vector of the same length as `x`.
#'
#' @examples
#' x <- c(0, 0, 0, 1, 1, 2, 2, 4, 6, 9, 0, 0, 0)
#' roll_hampel(x, 3)
roll_hampel <- function(
    x,
    width = 1L,
    by = 1L,
    align = c("center", "left", "right"),
    na.rm = FALSE
) {

  args <- .validateRollArgs(
    x = x,
    width = width,
    by = by,
    align = align,
    na.rm = na.rm
  )

  result <- .roll_hampel_cpp(
    args$x,
    args$width,
    args$by,
    args$align,
    args$na.rm
  )

  return(result)
}

#' Roll MAD
#'
#' @description Apply a moving-window Median Absolute Deviation function to a numeric vector.
#'
#' @details
#'
#' For every index in the incoming vector `x`, a value is returned that
#' is the Median Absolute Deviation (MAD) of all values in `x` that fall within
#' a window of width `width`.
#'
#' The `align` parameter determines the alignment of the return value
#' within the window. Thus:
#'
#' \itemize{
#'   \item{`align = "left"   [*------]` will cause the returned vector to have width - 1 `NA` values at the right end.}
#'   \item{`align = "center" [---*---]` will cause the returned vector to have `NA` values at either end as needed for centered alignment.}
#'   \item{`align = "right"  [------*]` will cause the returned vector to have width - 1 `NA` values at the left end.}
#' }
#'
#' For large vectors, the `by` parameter can be used to force the window
#' to jump ahead `by` indices for the next calculation. Indices that are
#' skipped over will be assigned `NA` values so that the return vector still has
#' the same length as the incoming vector. This can dramatically speed up
#' calculations for high resolution time series data.
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by Integer shift by which the window is moved each iteration.
#' @param align Character position of the return value within the window. One of:
#' `"left" | "center" | "right"`.
#' @param na.rm Logical specifying whether `NA` values should be removed
#' before the calculations within each window.
#'
#' @return Numeric vector of the same length as `x`.
#'
#' @examples
#' # Wikipedia example
#' x <- c(0, 0, 0, 1, 1, 2, 2, 4, 6, 9, 0, 0, 0)
#' roll_MAD(x, 3)
#' roll_MAD(x, 5)
#' roll_MAD(x, 7)
roll_MAD <- function(
    x,
    width = 1L,
    by = 1L,
    align = c("center", "left", "right"),
    na.rm = FALSE
) {

  args <- .validateRollArgs(
    x = x,
    width = width,
    by = by,
    align = align,
    na.rm = na.rm
  )

  result <- .roll_MAD_cpp(
    args$x,
    args$width,
    args$by,
    args$align,
    args$na.rm
  )

  return(result)
}

#' Roll Max
#'
#' @description Apply a moving-window maximum function to a numeric vector.
#'
#' @details
#'
#' For every index in the incoming vector `x`, a value is returned that
#' is the maximum of all values in `x` that fall within a window of width
#' `width`.
#'
#' The `align` parameter determines the alignment of the return value
#' within the window. Thus:
#'
#' \itemize{
#'   \item{`align = "left"   [*------]` will cause the returned vector to have width - 1 `NA` values at the right end.}
#'   \item{`align = "center" [---*---]` will cause the returned vector to have `NA` values at either end as needed for centered alignment.}
#'   \item{`align = "right"  [------*]` will cause the returned vector to have width - 1 `NA` values at the left end.}
#' }
#'
#' For large vectors, the `by` parameter can be used to force the window
#' to jump ahead `by` indices for the next calculation. Indices that are
#' skipped over will be assigned `NA` values so that the return vector still has
#' the same length as the incoming vector. This can dramatically speed up
#' calculations for high resolution time series data.
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by Integer shift by which the window is moved each iteration.
#' @param align Character position of the return value within the window. One of:
#' `"left" | "center" | "right"`.
#' @param na.rm Logical specifying whether `NA` values should be removed
#' before the calculations within each window.
#'
#' @return Numeric vector of the same length as `x`.
#'
#' @examples
#' # Example air quality time series
#' t <- example_pm25$datetime
#' x <- example_pm25$pm25
#'
#' plot(t, x, pch = 16, cex = 0.5)
#' lines(t, roll_max(x, width = 12), col = "red")
#' lines(t, roll_min(x, width = 12), col = "deepskyblue")
#' title("12-hr Rolling Max and Min")
#'
#' plot(t, x, pch = 16, cex = 0.5)
#' points(t, roll_max(x, width = 12, na.rm = TRUE),
#'        pch = 16, col = "red")
#' points(t, roll_max(x, width = 12, na.rm = FALSE),
#'        pch = 16, col = adjustcolor("black", 0.4))
#' legend("topright", pch = c(1, 16),
#'        col = c("red", adjustcolor("black", 0.4)),
#'        legend = c("na.rm = TRUE", "na.rm = FALSE"))
#' title("12-hr Rolling max with/out na.rm")
roll_max <- function(
    x,
    width = 1L,
    by = 1L,
    align = c("center", "left", "right"),
    na.rm = FALSE
) {

  args <- .validateRollArgs(
    x = x,
    width = width,
    by = by,
    align = align,
    na.rm = na.rm
  )

  result <- .roll_max_cpp(
    args$x,
    args$width,
    args$by,
    args$align,
    args$na.rm
  )

  return(result)
}

#' Roll Mean
#'
#' @description Apply a moving-window mean function to a numeric vector.
#'
#' @details
#'
#' For every index in the incoming vector `x`, a value is returned that
#' is the mean of all values in `x` that fall within a window of width
#' `width`.
#'
#' The `align` parameter determines the alignment of the return value
#' within the window. Thus:
#'
#' \itemize{
#'   \item{`align = "left"   [*------]` will cause the returned vector to have width - 1 `NA` values at the right end.}
#'   \item{`align = "center" [---*---]` will cause the returned vector to have `NA` values at either end as needed for centered alignment.}
#'   \item{`align = "right"  [------*]` will cause the returned vector to have width - 1 `NA` values at the left end.}
#' }
#'
#' For large vectors, the `by` parameter can be used to force the window
#' to jump ahead `by` indices for the next calculation. Indices that are
#' skipped over will be assigned `NA` values so that the return vector still has
#' the same length as the incoming vector. This can dramatically speed up
#' calculations for high resolution time series data.
#'
#' The `roll_mean()` function supports an additional `weights`
#' argument that can be used to calculate a weighted moving average,
#' a convolution of the incoming data with the kernel provided in `weights`.
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by Integer shift by which the window is moved each iteration.
#' @param align Character position of the return value within the window. One of:
#' `"left" | "center" | "right"`.
#' @param na.rm Logical specifying whether `NA` values should be removed
#' before the calculations within each window.
#' @param weights Numeric vector of length `width` specifying each window
#' index weight. If `NULL`, unit weights are used.
#'
#' @return Numeric vector of the same length as `x`.
#'
#' @examples
#' # Example air quality time series
#' t <- example_pm25$datetime
#' x <- example_pm25$pm25
#'
#' plot(t, x, pch = 16, cex = 0.5)
#' lines(t, roll_mean(x, width = 3), col = "goldenrod")
#' lines(t, roll_mean(x, width = 23), col = "purple")
#' legend("topright", lty = c(1, 1),
#'        col = c("goldenrod", "purple"),
#'        legend = c("3-hr mean", "23-hr mean"))
#' title("3- and 23-hr Rolling mean")
roll_mean <- function(
    x,
    width = 1L,
    by = 1L,
    align = c("center", "left", "right"),
    na.rm = FALSE,
    weights = NULL
) {

  args <- .validateRollArgs(
    x = x,
    width = width,
    by = by,
    align = align,
    na.rm = na.rm,
    weights = weights
  )

  result <- .roll_mean_cpp(
    args$x,
    args$width,
    args$by,
    args$align,
    args$na.rm,
    args$weights
  )

  return(result)
}

#' Roll Median
#'
#' @description Apply a moving-window median function to a numeric vector.
#'
#' @details
#'
#' For every index in the incoming vector `x`, a value is returned that
#' is the median of all values in `x` that fall within a window of width
#' `width`.
#'
#' The `align` parameter determines the alignment of the return value
#' within the window. Thus:
#'
#' \itemize{
#'   \item{`align = "left"   [*------]` will cause the returned vector to have width - 1 `NA` values at the right end.}
#'   \item{`align = "center" [---*---]` will cause the returned vector to have `NA` values at either end as needed for centered alignment.}
#'   \item{`align = "right"  [------*]` will cause the returned vector to have width - 1 `NA` values at the left end.}
#' }
#'
#' For large vectors, the `by` parameter can be used to force the window
#' to jump ahead `by` indices for the next calculation. Indices that are
#' skipped over will be assigned `NA` values so that the return vector still has
#' the same length as the incoming vector. This can dramatically speed up
#' calculations for high resolution time series data.
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by Integer shift by which the window is moved each iteration.
#' @param align Character position of the return value within the window. One of:
#' `"left" | "center" | "right"`.
#' @param na.rm Logical specifying whether `NA` values should be removed
#' before the calculations within each window.
#'
#' @return Numeric vector of the same length as `x`.
#'
#' @examples
#' # Example air quality time series
#' t <- example_pm25$datetime
#' x <- example_pm25$pm25
#'
#' plot(t, x, pch = 16, cex = 0.5)
#' lines(t, roll_median(x, width = 3), col = "goldenrod")
#' lines(t, roll_median(x, width = 23), col = "purple")
#' legend("topright", lty = c(1, 1),
#'        col = c("goldenrod", "purple"),
#'        legend = c("3-hr median", "23-hr median"))
#' title("3- and 23-hr Rolling median")
roll_median <- function(
    x,
    width = 1L,
    by = 1L,
    align = c("center", "left", "right"),
    na.rm = FALSE
) {

  args <- .validateRollArgs(
    x = x,
    width = width,
    by = by,
    align = align,
    na.rm = na.rm
  )

  result <- .roll_median_cpp(
    args$x,
    args$width,
    args$by,
    args$align,
    args$na.rm
  )

  return(result)
}

#' Roll Min
#'
#' @description Apply a moving-window minimum function to a numeric vector.
#'
#' @details
#'
#' For every index in the incoming vector `x`, a value is returned that
#' is the minimum of all values in `x` that fall within a window of width
#' `width`.
#'
#' The `align` parameter determines the alignment of the return value
#' within the window. Thus:
#'
#' \itemize{
#'   \item{`align = "left"   [*------]` will cause the returned vector to have width - 1 `NA` values at the right end.}
#'   \item{`align = "center" [---*---]` will cause the returned vector to have `NA` values at either end as needed for centered alignment.}
#'   \item{`align = "right"  [------*]` will cause the returned vector to have width - 1 `NA` values at the left end.}
#' }
#'
#' For large vectors, the `by` parameter can be used to force the window
#' to jump ahead `by` indices for the next calculation. Indices that are
#' skipped over will be assigned `NA` values so that the return vector still has
#' the same length as the incoming vector. This can dramatically speed up
#' calculations for high resolution time series data.
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by Integer shift by which the window is moved each iteration.
#' @param align Character position of the return value within the window. One of:
#' `"left" | "center" | "right"`.
#' @param na.rm Logical specifying whether `NA` values should be removed
#' before the calculations within each window.
#'
#' @return Numeric vector of the same length as `x`.
#'
#' @examples
#' # Example air quality time series
#' t <- example_pm25$datetime
#' x <- example_pm25$pm25
#'
#' plot(t, x, pch = 16, cex = 0.5)
#' lines(t, roll_max(x, width = 12), col = "red")
#' lines(t, roll_min(x, width = 12), col = "deepskyblue")
#' title("12-hr Rolling Max and Min")
#'
#' plot(t, x, pch = 16, cex = 0.5)
#' points(t, roll_min(x, width = 12, na.rm = TRUE),
#'        pch = 16, col = "deepskyblue")
#' points(t, roll_min(x, width = 12, na.rm = FALSE),
#'        pch = 16, col = adjustcolor("black", 0.4))
#' legend("topright", pch = c(16, 16),
#'        col = c("deepskyblue", adjustcolor("black", 0.4)),
#'        legend = c("na.rm = TRUE", "na.rm = FALSE"))
#' title("12-hr Rolling min with/out na.rm")
roll_min <- function(
    x,
    width = 1L,
    by = 1L,
    align = c("center", "left", "right"),
    na.rm = FALSE
) {

  args <- .validateRollArgs(
    x = x,
    width = width,
    by = by,
    align = align,
    na.rm = na.rm
  )

  result <- .roll_min_cpp(
    args$x,
    args$width,
    args$by,
    args$align,
    args$na.rm
  )

  return(result)
}

#' Roll Product
#'
#' @description Apply a moving-window product function to a numeric vector.
#'
#' @details
#'
#' For every index in the incoming vector `x`, a value is returned that
#' is the product of all values in `x` that fall within a window of width
#' `width`.
#'
#' The `align` parameter determines the alignment of the return value
#' within the window. Thus:
#'
#' \itemize{
#'   \item{`align = "left"   [*------]` will cause the returned vector to have width - 1 `NA` values at the right end.}
#'   \item{`align = "center" [---*---]` will cause the returned vector to have `NA` values at either end as needed for centered alignment.}
#'   \item{`align = "right"  [------*]` will cause the returned vector to have width - 1 `NA` values at the left end.}
#' }
#'
#' For large vectors, the `by` parameter can be used to force the window
#' to jump ahead `by` indices for the next calculation. Indices that are
#' skipped over will be assigned `NA` values so that the return vector still has
#' the same length as the incoming vector. This can dramatically speed up
#' calculations for high resolution time series data.
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by Integer shift by which the window is moved each iteration.
#' @param align Character position of the return value within the window. One of:
#' `"left" | "center" | "right"`.
#' @param na.rm Logical specifying whether `NA` values should be removed
#' before the calculations within each window.
#'
#' @return Numeric vector of the same length as `x`.
#'
#' @examples
#' # Example air quality time series
#' t <- example_pm25$datetime
#' x <- example_pm25$pm25
#'
#' x[1:10]
#' roll_prod(x, width = 5)[1:10]
roll_prod <- function(
    x,
    width = 1L,
    by = 1L,
    align = c("center", "left", "right"),
    na.rm = FALSE
) {

  args <- .validateRollArgs(
    x = x,
    width = width,
    by = by,
    align = align,
    na.rm = na.rm
  )

  result <- .roll_prod_cpp(
    args$x,
    args$width,
    args$by,
    args$align,
    args$na.rm
  )

  return(result)
}

#' Roll Standard Deviation
#'
#' @description Apply a moving-window standard deviation function to a
#' numeric vector.
#'
#' @details
#'
#' For every index in the incoming vector `x`, a value is returned that
#' is the standard deviation of all values in `x` that fall within a window of
#' width `width`.
#'
#' The `align` parameter determines the alignment of the return value
#' within the window. Thus:
#'
#' \itemize{
#'   \item{`align = "left"   [*------]` will cause the returned vector to have width - 1 `NA` values at the right end.}
#'   \item{`align = "center" [---*---]` will cause the returned vector to have `NA` values at either end as needed for centered alignment.}
#'   \item{`align = "right"  [------*]` will cause the returned vector to have width - 1 `NA` values at the left end.}
#' }
#'
#' For large vectors, the `by` parameter can be used to force the window
#' to jump ahead `by` indices for the next calculation. Indices that are
#' skipped over will be assigned `NA` values so that the return vector still has
#' the same length as the incoming vector. This can dramatically speed up
#' calculations for high resolution time series data.
#'
#' @note A `na.rm` argument is not provided for `roll_sd()` because the
#' statistical meaning of standard deviation computed from partially missing
#' windows may be ambiguous.
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by Integer shift by which the window is moved each iteration.
#' @param align Character position of the return value within the window. One of:
#' `"left" | "center" | "right"`.
#'
#' @return Numeric vector of the same length as `x`.
#'
#' @examples
#' # Example air quality time series
#' t <- example_pm25$datetime
#' x <- example_pm25$pm25
#'
#' x[1:10]
#' roll_sd(x, width = 5)[1:10]
roll_sd <- function(
    x,
    width = 1L,
    by = 1L,
    align = c("center", "left", "right")
) {

  args <- .validateRollArgs(
    x = x,
    width = width,
    by = by,
    align = align
  )

  result <- .roll_sd_cpp(
    args$x,
    args$width,
    args$by,
    args$align,
    FALSE
  )

  return(result)
}

#' Roll Sum
#'
#' @description Apply a moving-window sum to a numeric vector.
#'
#' @details
#'
#' For every index in the incoming vector `x`, a value is returned that
#' is the sum of all values in `x` that fall within a window of width
#' `width`.
#'
#' The `align` parameter determines the alignment of the return value
#' within the window. Thus:
#'
#' \itemize{
#'   \item{`align = "left"   [*------]` will cause the returned vector to have width - 1 `NA` values at the right end.}
#'   \item{`align = "center" [---*---]` will cause the returned vector to have `NA` values at either end as needed for centered alignment.}
#'   \item{`align = "right"  [------*]` will cause the returned vector to have width - 1 `NA` values at the left end.}
#' }
#'
#' For large vectors, the `by` parameter can be used to force the window
#' to jump ahead `by` indices for the next calculation. Indices that are
#' skipped over will be assigned `NA` values so that the return vector still has
#' the same length as the incoming vector. This can dramatically speed up
#' calculations for high resolution time series data.
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by Integer shift by which the window is moved each iteration.
#' @param align Character position of the return value within the window. One of:
#' `"left" | "center" | "right"`.
#' @param na.rm Logical specifying whether `NA` values should be removed
#' before the calculations within each window.
#'
#' @return Numeric vector of the same length as `x`.
#'
#' @examples
#' # Example air quality time series
#' t <- example_pm25$datetime
#' x <- example_pm25$pm25
#'
#' x[1:10]
#' roll_sum(x, width = 5)[1:10]
roll_sum <- function(
    x,
    width = 1L,
    by = 1L,
    align = c("center", "left", "right"),
    na.rm = FALSE
) {

  args <- .validateRollArgs(
    x = x,
    width = width,
    by = by,
    align = align,
    na.rm = na.rm
  )

  result <- .roll_sum_cpp(
    args$x,
    args$width,
    args$by,
    args$align,
    args$na.rm
  )

  return(result)
}

#' Roll Variance
#'
#' @description Apply a moving-window variance function to a numeric vector.
#'
#' @details
#'
#' For every index in the incoming vector `x`, a value is returned that
#' is the variance of all values in `x` that fall within a window of width
#' `width`.
#'
#' The `align` parameter determines the alignment of the return value
#' within the window. Thus:
#'
#' \itemize{
#'   \item{`align = "left"   [*------]` will cause the returned vector to have width - 1 `NA` values at the right end.}
#'   \item{`align = "center" [---*---]` will cause the returned vector to have `NA` values at either end as needed for centered alignment.}
#'   \item{`align = "right"  [------*]` will cause the returned vector to have width - 1 `NA` values at the left end.}
#' }
#'
#' For large vectors, the `by` parameter can be used to force the window
#' to jump ahead `by` indices for the next calculation. Indices that are
#' skipped over will be assigned `NA` values so that the return vector still has
#' the same length as the incoming vector. This can dramatically speed up
#' calculations for high resolution time series data.
#'
#' @note A `na.rm` argument is not provided for `roll_var()` because the
#' statistical meaning of variance computed from partially missing windows may
#' be ambiguous.
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by Integer shift by which the window is moved each iteration.
#' @param align Character position of the return value within the window. One of:
#' `"left" | "center" | "right"`.
#'
#' @return Numeric vector of the same length as `x`.
#'
#' @examples
#' # Example air quality time series
#' t <- example_pm25$datetime
#' x <- example_pm25$pm25
#'
#' x[1:10]
#' roll_var(x, width = 5)[1:10]
roll_var <- function(
    x,
    width = 1L,
    by = 1L,
    align = c("center", "left", "right")
) {

  args <- .validateRollArgs(
    x = x,
    width = width,
    by = by,
    align = align
  )

  result <- .roll_var_cpp(
    args$x,
    args$width,
    args$by,
    args$align,
    FALSE
  )

  return(result)
}

.validateRollArgs <- function(
    x,
    width,
    by,
    align,
    na.rm = NULL,
    weights = NULL
) {

  if ( !is.atomic(x) || !is.numeric(x) || !is.null(dim(x)) ) {
    stop("'x' must be a numeric vector.")
  }

  if ( length(width) != 1 || !is.numeric(width) || is.na(width) ||
       !is.finite(width) || width < 1 || width != as.integer(width) ) {
    stop("'width' must be a single positive integer.")
  }

  if ( length(by) != 1 || !is.numeric(by) || is.na(by) ||
       !is.finite(by) || by < 1 || by != as.integer(by) ) {
    stop("'by' must be a single positive integer.")
  }

  align <- match.arg(align, c("center", "left", "right"))

  if ( !is.null(na.rm) ) {
    if ( !is.logical(na.rm) || length(na.rm) != 1 || is.na(na.rm) ) {
      stop("'na.rm' must be TRUE or FALSE.")
    }
  }

  if ( !is.null(weights) ) {
    if ( !is.atomic(weights) || !is.numeric(weights) || !is.null(dim(weights)) ) {
      stop("'weights' must be NULL or a numeric vector.")
    }
    if ( anyNA(weights) || any(!is.finite(weights)) ) {
      stop("'weights' must not contain NA, NaN, or infinite values.")
    }
    if ( length(weights) != as.integer(width) ) {
      stop("'weights' must have length equal to 'width'.")
    }
  }

  return(list(
    x = x,
    width = as.integer(width),
    by = as.integer(by),
    align = align,
    na.rm = na.rm,
    weights = weights
  ))
}
