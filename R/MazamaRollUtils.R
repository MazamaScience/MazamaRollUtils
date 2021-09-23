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
#' @param align Character position of the return value within the window --
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
#' lines(t, roll_max(x, width = 12), col = 'salmon')
#' lines(t, roll_min(x, width = 12), col = 'light blue')

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
#' @param by An integer to shift the window by.
#' @param align Character position of the return value within the window --
#' \code{"left" | "center" | "right"}.
#' @param na.rm Logical specifying whether \code{NA} values should be removed
#' before the calculations within each window.
#' @param weights A numeric vector of size \code{width} specifying each window
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
#' @param by An integer to shift the window by.
#' @param align Character position of the return value within the window --
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
#' @param by An integer to shift the window by.
#' @param align Character position of the return value within the window --
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
#' lines(t, roll_max(x, width = 24), col = 'salmon')
#' lines(t, roll_min(x, width = 24), col = 'light blue')

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
#' @param by An integer to shift the window by.
#' @param align Character position of the return value within the window --
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
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by An integer to shift the window by.
#' @param align Character position of the return value within the window --
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
#' roll_sd(x, width = 5)[1:10]

roll_sd <- function(
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

  result <- .roll_sd_cpp(
    x,
    as.integer(width),
    as.integer(by),
    as.character(match.arg(align)),
    as.logical(na.rm)
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
#' @param by An integer to shift the window by.
#' @param align Character position of the return value within the window --
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
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param by An integer to shift the window by.
#' @param align Character position of the return value within the window --
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
#' roll_var(x, width = 5)[1:10]

roll_var <- function(
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

  result <- .roll_var_cpp(
    x,
    as.integer(width),
    as.integer(by),
    as.character(match.arg(align)),
    as.logical(na.rm)
  )

  return(result)
}

