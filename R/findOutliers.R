#' Outlier detection with a rolling Hampel filter
#'
#' A wrapper around [roll_hampel()] that identifies outliers using either
#' a fixed threshold or a threshold derived from the input data.
#'
#' @details
#'
#' The \code{thresholdMin} level is similar to a sigma value for normally
#' distributed data. Hampel filter values above 6 indicate a data value that is
#' extremely unlikely to be part of a normal distribution  (~ 1/500 million) and
#' therefore very likely to be an outlier. By choosing a relatively large value
#' for \code{thresholdMin} we make it less likely that we will generate false
#' positives. False positives can include high frequency environmental noise.
#'
#' With the default setting of \code{fixedThreshold = TRUE} any value above the
#' threshold is considered an outlier and the \code{selectivity} is ignored.
#'
#' The \code{selectivity} is a value between 0 and 1 and is used to generate an
#' appropriate threshold for outlier detection based on the statistics of the
#' incoming data. A lower value for \code{selectivity} will result in more
#' outliers while a value closer to 1.0 will result in fewer. If
#' \code{fixedThreshold=TRUE}, \code{selectivity} may have a value of \code{NA}.
#'
#' When the user specifies \code{fixedThreshold=FALSE}, the \code{thresholdMin}
#' and \code{selectivity} parameters work like squelch and volume on a CB radio:
#' \code{thresholdMin} sets a noise threshold below which you don't want anything
#' returned while \code{selectivity} adjusts the number of points defined as
#' outliers by setting a new threshold defined by the maximum value of
#' \code{roll_hampel} multiplied by \code{selectivity}.
#'
#' \code{width}, the window width, is a parameter that is passed to
#' \code{roll_hampel()}.
#'
#' @note This function is copied from the \pkg{seismicRoll} package.
#'
#' @param x Numeric vector.
#' @param width Integer width of the rolling window.
#' @param thresholdMin Numeric threshold for outlier detection
#' @param selectivity Value between 0 and 1 used in determining outliers, or
#' \code{NA} if \code{fixedThreshold=TRUE}.
#' @param fixedThreshold Logical specifying whether outlier detection uses
#' \code{selectivity}  (see Details).
#'
#' @return A vector of indices associated with outliers in the incoming data \code{x}.
#'
#' @seealso \code{\link{roll_hampel}}
#' @examples
#' # Noisy sinusoid with outliers
#' a <- jitter(sin(0.1*seq(1e4)),amount=0.2)
#' indices <- sample(seq(1e4),20)
#' a[indices] <- a[indices]*10
#'
#' # Outlier detection should identify many of these altered indices
#' sort(indices)
#' o_indices <- findOutliers(a)
#' o_indices
#'
#' plot(a)
#' points(o_indices, a[o_indices], pch = 16, cex = 0.8, col = 'red')
#' title("Outlier detection using a Hampel filter")

findOutliers <- function(
    x,
    width = 25,
    thresholdMin = 7,
    selectivity = NA,
    fixedThreshold = TRUE
) {

  if (!is.logical(fixedThreshold)) {
    warning(
      sprintf(
        "Argument 'fixedThreshold' must be logical; got %s. Using TRUE.",
        deparse(fixedThreshold)
      )
    )
    fixedThreshold <- TRUE
  }

  h <- roll_hampel(x, width)

  # If 50%+ of values in a window are identical, h can become Inf.
  # In that case, replace Inf with NA.
  h[is.infinite(h)] <- NA

  if (all(is.na(h))) {
    stop(
      "roll_hampel() returned all NA values; this can occur when 50% or more ",
      "of values in every rolling window are identical."
    )
  }

  maxH <- max(h, na.rm = TRUE)

  if (maxH < thresholdMin) {
    return(integer(0))
  } else {
    if (fixedThreshold) {
      return(which(h > thresholdMin))
    } else {
      return(which(h > maxH * selectivity))
    }
  }

}
