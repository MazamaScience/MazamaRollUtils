#' @title Roll Hampel
#'
#' @description Apply a moving-window Hampel function to a numeric vector.
#'
#' @details
#'
#' The Hampel filter is a robust outlier detector using Median Absolute Deviation (MAD).
#'
#' For every index in the incoming vector \code{x}, a value is returned that
#' is the Hampel funcion of all values in \code{x} that fall within a window of width
#' \code{width}.
#'
#' The \code{align} parameter determines the alignment of the return value
#' within the window. Thus:
#'
#' \itemize{
#'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
#'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
#'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
#' }
#'
#' For large vectors, the\code{by} parameter can be used to force the window
#' to jump ahead \code{by} indices for the next calculation. Indices that are
#' skipped over will be assigned \code{NA} values so that the return vector still has
#' the same length as the incoming vector. This can dramatically speed up
#' calculations for high resolution time series data.
#'
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by Integer shift to use when sliding the window to the next location
#' @param align Character position of the return value within the window. One of:
#' \code{"left" | "center" | "right"}.
#' @param na.rm Logical specifying whether \code{NA} values should be removed
#' before the calculations within each window.
#'
#' @return Numeric vector of the same length as \code{x}.
#'
#' @examples
#' library(MazamaRollUtils)
#'
#' x <- c(0, 0, 0, 1, 1, 2, 2, 4, 6, 9, 0, 0, 0)
#' roll_hampel(x, 3)


roll_hampel <- function(
  x,
  width = 1L,
  by = 1L,
  align = c("center", "left", "right"),
  na.rm = FALSE
) {

  if ( !is.numeric(x) ) stop("'x' must be numeric.")
  if ( !is.numeric(width) ) stop("'width' must be numeric.")
  if ( !is.numeric(by) ) stop("'by' must be numeric.")
  if ( !is.character(align) ) stop("'align' must be character.")
  if ( !is.logical(na.rm) ) stop("'na.rm' must be logical.")

  result <- .roll_hampel_cpp(
    x,
    as.integer(width),
    as.integer(by),
    as.character(match.arg(align)),
    as.logical(na.rm)
  )

  return(result)
}

#' @title Roll MAD
#'
#' @description Apply a moving-window Median Absolute Deviation function to a numeric vector.
#'
#' @details
#'
#' For every index in the incoming vector \code{x}, a value is returned that
#' is the Median Absolute Deviation (MAD) of all values in \code{x} that fall within a window of width
#' \code{width}.
#'
#' The \code{align} parameter determines the alignment of the return value
#' within the window. Thus:
#'
#' \itemize{
#'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
#'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
#'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
#' }
#'
#' For large vectors, the\code{by} parameter can be used to force the window
#' to jump ahead \code{by} indices for the next calculation. Indices that are
#' skipped over will be assigned \code{NA} values so that the return vector still has
#' the same length as the incoming vector. This can dramatically speed up
#' calculations for high resolution time series data.
#'
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by Integer shift to use when sliding the window to the next location
#' @param align Character position of the return value within the window. One of:
#' \code{"left" | "center" | "right"}.
#' @param na.rm Logical specifying whether \code{NA} values should be removed
#' before the calculations within each window.
#'
#' @return Numeric vector of the same length as \code{x}.
#'
#' @examples
#' library(MazamaRollUtils)
#'
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

  if ( !is.numeric(x) ) stop("'x' must be numeric.")
  if ( !is.numeric(width) ) stop("'width' must be numeric.")
  if ( !is.numeric(by) ) stop("'by' must be numeric.")
  if ( !is.character(align) ) stop("'align' must be character.")
  if ( !is.logical(na.rm) ) stop("'na.rm' must be logical.")

  result <- .roll_MAD_cpp(
    x,
    as.integer(width),
    as.integer(by),
    as.character(match.arg(align)),
    as.logical(na.rm)
  )

  return(result)
}

#' @title Roll Max
#'
#' @description Apply a moving-window maximum function to a numeric vector.
#'
#' @details
#'
#' For every index in the incoming vector \code{x}, a value is returned that
#' is the maximum of all values in \code{x} that fall within a window of width
#' \code{width}.
#'
#' The \code{align} parameter determines the alignment of the return value
#' within the window. Thus:
#'
#' \itemize{
#'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
#'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
#'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
#' }
#'
#' For large vectors, the\code{by} parameter can be used to force the window
#' to jump ahead \code{by} indices for the next calculation. Indices that are
#' skipped over will be assigned \code{NA} values so that the return vector still has
#' the same length as the incoming vector. This can dramatically speed up
#' calculations for high resolution time series data.
#'
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by Integer shift to use when sliding the window to the next location
#' @param align Character position of the return value within the window. One of:
#' \code{"left" | "center" | "right"}.
#' @param na.rm Logical specifying whether \code{NA} values should be removed
#' before the calculations within each window.
#'
#' @return Numeric vector of the same length as \code{x}.
#'
#' @examples
#' library(MazamaRollUtils)
#'
#' # Example air quality time series
#' t <- example_pm25$datetime
#' x <- example_pm25$pm25
#'
#' plot(t, x, pch = 16, cex = 0.5)
#' lines(t, roll_max(x, width = 12), col = 'red')
#' lines(t, roll_min(x, width = 12), col = 'deepskyblue')
#' title("12-hr Rolling Max and Min")
#'
#' plot(t, x, pch = 16, cex = 0.5)
#' points(t, roll_max(x, width = 12, na.rm = TRUE),
#'        pch = 16, col = 'red')
#' points(t, roll_max(x, width = 12, na.rm = FALSE),
#'        pch = 16, col = adjustcolor('black', 0.4))
#' legend("topright", pch = c(1, 16),
#'        col = c("red", adjustcolor("black", 0.4)),
#'        legend = c("na.rm = TRUE", "na.rm = FALSE"))
#' title("12-hr Rolling max with/out na.rm")
#'

roll_max <- function(
  x,
  width = 1L,
  by = 1L,
  align = c("center", "left", "right"),
  na.rm = FALSE
) {

  if ( !is.numeric(x) ) stop("'x' must be numeric.")
  if ( !is.numeric(width) ) stop("'width' must be numeric.")
  if ( !is.numeric(by) ) stop("'by' must be numeric.")
  if ( !is.character(align) ) stop("'align' must be character.")
  if ( !is.logical(na.rm) ) stop("'na.rm' must be logical.")

  result <- .roll_max_cpp(
    x,
    as.integer(width),
    as.integer(by),
    as.character(match.arg(align)),
    as.logical(na.rm)
  )

  return(result)
}

#' @title Roll Mean
#'
#' @description Apply a moving-window mean function to a numeric vector.
#'
#' @details
#'
#' For every index in the incoming vector \code{x}, a value is returned that
#' is the mean of all values in \code{x} that fall within a window of width
#' \code{width}.
#'
#' The \code{align} parameter determines the alignment of the return value
#' within the window. Thus:
#'
#' \itemize{
#'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
#'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
#'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
#' }
#'
#' For large vectors, the\code{by} parameter can be used to force the window
#' to jump ahead \code{by} indices for the next calculation. Indices that are
#' skipped over will be assigned \code{NA} values so that the return vector still has
#' the same length as the incoming vector. This can dramatically speed up
#' calculations for high resolution time series data.
#'
#' The \code{roll_mean()} function supports an additional \code{weights}
#' argument that can be used to calculate a "weighted moving average" --
#' a convolution of the incoming data with the \emph{kernel} (weighting function)
#' provided in \code{weights}.
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by Integer shift by which the window is moved each iteration.
#' @param align Character position of the return value within the window. One of:
#' \code{"left" | "center" | "right"}.
#' @param na.rm Logical specifying whether \code{NA} values should be removed
#' before the calculations within each window.
#' @param weights Numeric vector of size \code{width} specifying each window
#' index weight. If \code{NULL}, unit weights are used.
#'
#' @return Numeric vector of the same length as \code{x}.
#'
#' @examples
#' library(MazamaRollUtils)
#'
#' # Example air quality time series
#' t <- example_pm25$datetime
#' x <- example_pm25$pm25
#'
#' plot(t, x, pch = 16, cex = 0.5)
#' lines(t, roll_mean(x, width = 3), col = "goldenrod")
#' lines(t, roll_mean(x, width = 23), col = "purple")
#' legend("topright", lty = c(1, 1),
#'        col = c("goldenrod", "purple"),
#'        legend = c("3-hr mean", "12-hr mean"))
#' title("3- and 23-hr Rolling mean")

roll_mean <- function(
  x,
  width = 1L,
  by = 1L,
  align = c("center", "left", "right"),
  na.rm = FALSE,
  weights = NULL
) {

  if ( !is.numeric(x) ) stop("'x' must be numeric.")
  if ( !is.numeric(width) ) stop("'width' must be numeric.")
  if ( !is.numeric(by) ) stop("'by' must be numeric.")
  if ( !is.character(align) ) stop("'align' must be character.")
  if ( !is.logical(na.rm) ) stop("'na.rm' must be logical.")
  if ( !is.null(weights) ) {
    if ( !is.numeric(weights) ) stop("'weights' must be NULL or numeric.")
  }

  result <- .roll_mean_cpp(
    x,
    as.integer(width),
    as.integer(by),
    as.character(match.arg(align)),
    as.logical(na.rm),
    weights
  )

  return(result)
}


#' @title Roll Median
#'
#' @description Apply a moving-window median function to a numeric vector.
#'
#' @details
#'
#' For every index in the incoming vector \code{x}, a value is returned that
#' is the median of all values in \code{x} that fall within a window of width
#' \code{width}.
#'
#' The \code{align} parameter determines the alignment of the return value
#' within the window. Thus:
#'
#' \itemize{
#'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
#'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
#'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
#' }
#'
#' For large vectors, the\code{by} parameter can be used to force the window
#' to jump ahead \code{by} indices for the next calculation. Indices that are
#' skipped over will be assigned \code{NA} values so that the return vector still has
#' the same length as the incoming vector. This can dramatically speed up
#' calculations for high resolution time series data.
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by Integer shift by which the window is moved each iteration.
#' @param align Character position of the return value within the window. One of:
#' \code{"left" | "center" | "right"}.
#' @param na.rm Logical specifying whether \code{NA} values should be removed
#' before the calculations within each window.
#'
#' @return Numeric vector of the same length as \code{x}.
#'
#' @examples
#' library(MazamaRollUtils)
#'
#' # Example air quality time series
#' t <- example_pm25$datetime
#' x <- example_pm25$pm25
#'
#' plot(t, x, pch = 16, cex = 0.5)
#' lines(t, roll_median(x, width = 3), col = "goldenrod")
#' lines(t, roll_median(x, width = 23), col = "purple")
#' legend("topright", lty = c(1, 1),
#'        col = c("goldenrod", "purple"),
#'        legend = c("3-hr median", "12-hr median"))
#' title("3- and 23-hr Rolling median")

roll_median <- function(
  x,
  width = 1L,
  by = 1L,
  align = c("center", "left", "right"),
  na.rm = FALSE
) {

  if ( !is.numeric(x) ) stop("'x' must be numeric.")
  if ( !is.numeric(width) ) stop("'width' must be numeric.")
  if ( !is.numeric(by) ) stop("'by' must be numeric.")
  if ( !is.character(align) ) stop("'align' must be character.")
  if ( !is.logical(na.rm) ) stop("'na.rm' must be logical.")

  result <- .roll_median_cpp(
    x,
    as.integer(width),
    as.integer(by),
    as.character(match.arg(align)),
    as.logical(na.rm)
  )

  return(result)
}

#' @title Roll Min
#'
#' @description Apply a moving-window minimum function to a numeric vector.
#'
#' @details
#'
#' For every index in the incoming vector \code{x}, a value is returned that
#' is the minimum of all values in \code{x} that fall within a window of width
#' \code{width}.
#'
#' The \code{align} parameter determines the alignment of the return value
#' within the window. Thus:
#'
#' \itemize{
#'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
#'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
#'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
#' }
#'
#' For large vectors, the\code{by} parameter can be used to force the window
#' to jump ahead \code{by} indices for the next calculation. Indices that are
#' skipped over will be assigned \code{NA} values so that the return vector still has
#' the same length as the incoming vector. This can dramatically speed up
#' calculations for high resolution time series data.
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by Integer shift by which the window is moved each iteration.
#' @param align Character position of the return value within the window. One of:
#' \code{"left" | "center" | "right"}.
#' @param na.rm Logical specifying whether \code{NA} values should be removed
#' before the calculations within each window.
#'
#' @return Numeric vector of the same length as \code{x}.
#'
#' @examples
#' library(MazamaRollUtils)
#'
#' # Example air quality time series
#' t <- example_pm25$datetime
#' x <- example_pm25$pm25
#'
#' plot(t, x, pch = 16, cex = 0.5)
#' lines(t, roll_max(x, width = 12), col = 'red')
#' lines(t, roll_min(x, width = 12), col = 'deepskyblue')
#' title("12-hr Rolling Max and Min")
#'
#' plot(t, x, pch = 16, cex = 0.5)
#' points(t, roll_min(x, width = 12, na.rm = TRUE),
#'        pch = 16, col = 'deepskyblue')
#' points(t, roll_min(x, width = 12, na.rm = FALSE),
#'        pch = 16, col = adjustcolor('black', 0.4))
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

  if ( !is.numeric(x) ) stop("'x' must be numeric.")
  if ( !is.numeric(width) ) stop("'width' must be numeric.")
  if ( !is.numeric(by) ) stop("'by' must be numeric.")
  if ( !is.character(align) ) stop("'align' must be character.")
  if ( !is.logical(na.rm) ) stop("'na.rm' must be logical.")

  result <- .roll_min_cpp(
    x,
    as.integer(width),
    as.integer(by),
    as.character(match.arg(align)),
    as.logical(na.rm)
  )

  return(result)
}

#' @title Roll Product
#'
#' @description Apply a moving-window product function to a numeric vector.
#'
#' @details
#'
#' For every index in the incoming vector \code{x}, a value is returned that
#' is the product of all values in \code{x} that fall within a window of width
#' \code{width}.
#'
#' The \code{align} parameter determines the alignment of the return value
#' within the window. Thus:
#'
#' \itemize{
#'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
#'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
#'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
#' }
#'
#' For large vectors, the\code{by} parameter can be used to force the window
#' to jump ahead \code{by} indices for the next calculation. Indices that are
#' skipped over will be assigned \code{NA} values so that the return vector still has
#' the same length as the incoming vector. This can dramatically speed up
#' calculations for high resolution time series data.
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by Integer shift by which the window is moved each iteration.
#' @param align Character position of the return value within the window. One of:
#' \code{"left" | "center" | "right"}.
#' @param na.rm Logical specifying whether \code{NA} values should be removed
#' before the calculations within each window.
#'
#' @return Numeric vector of the same length as \code{x}.
#'
#' @examples
#' library(MazamaRollUtils)
#'
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

  if ( !is.numeric(x) ) stop("'x' must be numeric.")
  if ( !is.numeric(width) ) stop("'width' must be numeric.")
  if ( !is.numeric(by) ) stop("'by' must be numeric.")
  if ( !is.character(align) ) stop("'align' must be character.")
  if ( !is.logical(na.rm) ) stop("'na.rm' must be logical.")

  result <- .roll_prod_cpp(
    x,
    as.integer(width),
    as.integer(by),
    as.character(match.arg(align)),
    as.logical(na.rm)
  )

  return(result)
}

#' @title Roll Standard Deviation
#'
#' @description Apply a moving-window standard deviation function to a
#' numeric vector.
#'
#' @details
#'
#' For every index in the incoming vector \code{x}, a value is returned that
#' is the standard deviation of all values in \code{x} that fall within a window of width
#' \code{width}.
#'
#' The \code{align} parameter determines the alignment of the return value
#' within the window. Thus:
#'
#' \itemize{
#'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
#'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
#'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
#' }
#'
#' For large vectors, the\code{by} parameter can be used to force the window
#' to jump ahead \code{by} indices for the next calculation. Indices that are
#' skipped over will be assigned \code{NA} values so that the return vector still has
#' the same length as the incoming vector. This can dramatically speed up
#' calculations for high resolution time series data.
#'
#' @note No \code{na.rm} argument is provided as interpretation of the results
#' is not at all clear.
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by Integer shift by which the window is moved each iteration.
#' @param align Character position of the return value within the window. One of:
#' \code{"left" | "center" | "right"}.
#'
#' @return Numeric vector of the same length as \code{x}.
#'
#' @examples
#' library(MazamaRollUtils)
#'
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

  if ( !is.numeric(x) ) stop("'x' must be numeric.")
  if ( !is.numeric(width) ) stop("'width' must be numeric.")
  if ( !is.numeric(by) ) stop("'by' must be numeric.")
  if ( !is.character(align) ) stop("'align' must be character.")

  result <- .roll_sd_cpp(
    x,
    as.integer(width),
    as.integer(by),
    as.character(match.arg(align)),
    as.logical(FALSE)
  )

  return(result)
}

#' @title Roll Sum
#'
#' @description Apply a moving-window sum to a numeric vector.
#'
#' @details
#'
#' For every index in the incoming vector \code{x}, a value is returned that
#' is the sum of all values in \code{x} that fall within a window of width
#' \code{width}.
#'
#' The \code{align} parameter determines the alignment of the return value
#' within the window. Thus:
#'
#' \itemize{
#'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
#'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
#'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
#' }
#'
#' For large vectors, the\code{by} parameter can be used to force the window
#' to jump ahead \code{by} indices for the next calculation. Indices that are
#' skipped over will be assigned \code{NA} values so that the return vector still has
#' the same length as the incoming vector. This can dramatically speed up
#' calculations for high resolution time series data.
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by Integer shift by which the window is moved each iteration.
#' @param align Character position of the return value within the window. One of:
#' \code{"left" | "center" | "right"}.
#' @param na.rm Logical specifying whether \code{NA} values should be removed
#' before the calculations within each window.
#'
#' @return Numeric vector of the same length as \code{x}.
#'
#' @examples
#' library(MazamaRollUtils)
#'
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

  if ( !is.numeric(x) ) stop("'x' must be numeric.")
  if ( !is.numeric(width) ) stop("'width' must be numeric.")
  if ( !is.numeric(by) ) stop("'by' must be numeric.")
  if ( !is.character(align) ) stop("'align' must be character.")
  if ( !is.logical(na.rm) ) stop("'na.rm' must be logical.")

  result <- .roll_sum_cpp(
    x,
    as.integer(width),
    as.integer(by),
    as.character(match.arg(align)),
    as.logical(na.rm)
  )

  return(result)
}

#' @title Roll Variance
#'
#' @description Apply a moving-window variance function to a numeric vector.
#'
#' @details
#'
#' For every index in the incoming vector \code{x}, a value is returned that
#' is the variance of all values in \code{x} that fall within a window of width
#' \code{width}.
#'
#' The \code{align} parameter determines the alignment of the return value
#' within the window. Thus:
#'
#' \itemize{
#'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
#'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
#'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
#' }
#'
#' For large vectors, the\code{by} parameter can be used to force the window
#' to jump ahead \code{by} indices for the next calculation. Indices that are
#' skipped over will be assigned \code{NA} values so that the return vector still has
#' the same length as the incoming vector. This can dramatically speed up
#' calculations for high resolution time series data.
#'
#' @note No \code{na.rm} argument is provided as interpretation of the results
#' is not at all clear.
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by Integer shift by which the window is moved each iteration.
#' @param align Character position of the return value within the window. One of:
#' \code{"left" | "center" | "right"}.
#'
#' @return Numeric vector of the same length as \code{x}.
#'
#' @examples
#' library(MazamaRollUtils)
#'
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

  if ( !is.numeric(x) ) stop("'x' must be numeric.")
  if ( !is.numeric(width) ) stop("'width' must be numeric.")
  if ( !is.numeric(by) ) stop("'by' must be numeric.")
  if ( !is.character(align) ) stop("'align' must be character.")

  result <- .roll_var_cpp(
    x,
    as.integer(width),
    as.integer(by),
    as.character(match.arg(align)),
    as.logical(FALSE)
  )

  return(result)
}

