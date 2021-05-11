#' Roll Median
#'
#' @param x A numeric vector
#' @param n An integer window size #'
#' @param weights A numeric vector of n-length specifying each \code{n}th weight.
#' @param by Integer to shift window by
#' @param align Signed Integer window alignment, -1 (left) | 0 (center) | 1 (right)
#'
#' @return numeric vector of length(x)
#' @export
#'
#' @examples
roll_median <- function(x, n = 5L, weights = NULL, by = 1L, align = 0L) {
  .Call(`_MazamaRollUtils_roll_median`, x, n, weights, by, align)
}

#' Roll Mean
#'
#' @param x A numeric vector
#' @param n An integer window size #'
#' @param weights A numeric vector of n-length specifying each \code{n}th weight.
#' @param by Integer to shift window by
#' @param align Signed Integer window alignment, -1 (left) | 0 (center) | 1 (right)
#'
#' @return numeric vector of length(x)
#' @export
#'
#' @examples
roll_mean <- function(x, n = 5L, weights = NULL, by = 1L, align = 0L) {
  .Call(`_MazamaRollUtils_roll_mean`, x, n, weights, by, align)
}

#' Roll Variance
#'
#' @param x A numeric vector
#' @param n An integer window size#'
#' @param weights A numeric vector of n-length specifying each \code{n}th weight.
#' @param by Integer to shift window by
#' @param align Signed Integer window alignment, -1 (left) | 0 (center) | 1 (right)
#'
#' @return numeric vector of length(x)
#' @export
#'
#' @examples
roll_var <- function(x, n = 5L, weights = NULL, by = 1L, align = 0L) {
  .Call(`_MazamaRollUtils_roll_var`, x, n, weights, by, align)
}

#' Roll Standard Deviation
#'
#' @param x A numeric vector
#' @param n An integer window size #'
#' @param weights A numeric vector of n-length specifying each \code{n}th weight.
#' @param by Integer to shift window by
#' @param align Signed Integer window alignment, -1 (left) | 0 (center) | 1 (right)
#'
#' @return numeric vector of length(x)
#' @export
#'
#' @examples
roll_sd <- function(x, n = 5L, weights = NULL, by = 1L, align = 0L) {
  .Call(`_MazamaRollUtils_roll_sd`, x, n, weights, by, align)
}

#' Roll Hampel Detection
#'
#' @param x A numeric vector
#' @param n An integer window size
#' @param weights A numeric vector of n-length specifying each \code{n}th weight.
#' @param by Integer to shift window by
#' @param align Signed Integer window alignment, -1 (left) | 0 (center) | 1 (right)
#'
#' @return numeric vector of length(x)
#' @export
#'
#' @examples
roll_hampel <- function(x, n = 5L, weights = NULL, by = 1L, align = 0L) {
  .Call(`_MazamaRollUtils_roll_hampel`, x, n, weights, by, align)
}

#' Roll Maximum
#'
#' @param x A numeric vector
#' @param n An integer window size#'
#' @param weights A numeric vector of n-length specifying each \code{n}th weight.
#' @param by Integer to shift window by
#' @param align Signed Integer window alignment, -1 (left) | 0 (center) | 1 (right)
#'
#' @return numeric vector of length(x)
#' @export
#'
#' @examples
roll_max <- function(x, n = 5L, weights = NULL, by = 1L, align = 0L) {
  .Call(`_MazamaRollUtils_roll_max`, x, n, weights, by, align)
}

#' Roll Minimum
#'
#' @param x A numeric vector
#' @param n An integer window size#'
#' @param weights A numeric vector of n-length specifying each \code{n}th weight.
#' @param by Integer to shift window by
#' @param align Signed Integer window alignment, -1 (left) | 0 (center) | 1 (right)
#'
#' @return numeric vector of length(x)
#' @export
#'
#' @examples
roll_min <- function(x, n = 5L, weights = NULL, by = 1L, align = 0L) {
  .Call(`_MazamaRollUtils_roll_min`, x, n, weights, by, align)
}
