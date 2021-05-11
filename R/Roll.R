#' Roll Median
#'
#' @param x A numeric vector
#' @param n An integer window size
#' @param by Integer to shift window by
#' @param align Signed Integer window alignment, -1 (left) | 0 (center) | 1 (right)
#'
#' @return numeric vector of length(X)
#' @export
#'
#' @examples
roll_median <- function(x, n = 5L, by = 1L, align = 0L) {
  .Call(`_MazamaRollUtils_roll_median`, x, n, by, align)
}

#' Roll Mean
#'
#' @param x A numeric vector
#' @param n An integer window size
#' @param by Integer to shift window by
#' @param align Signed Integer window alignment, -1 (left) | 0 (center) | 1 (right)
#'
#' @return numeric vector of length(X)
#' @export
#'
#' @examples
roll_mean <- function(x, n = 5L, by = 1L, align = 0L) {
  .Call(`_MazamaRollUtils_roll_mean`, x, n, by, align)
}

#' Roll Variance
#'
#' @param x A numeric vector
#' @param n An integer window size
#' @param by Integer to shift window by
#' @param align Signed Integer window alignment, -1 (left) | 0 (center) | 1 (right)
#'
#' @return numeric vector of length(X)
#' @export
#'
#' @examples
roll_var <- function(x, n = 5L, by = 1L, align = 0L) {
}

#' Roll Standard Deviation
#'
#' @param x A numeric vector
#' @param n An integer window size
#' @param by Integer to shift window by
#' @param align Signed Integer window alignment, -1 (left) | 0 (center) | 1 (right)
#'
#' @return numeric vector of length(X)
#' @export
#'
#' @examples
roll_sd <- function(x, n = 5L, by = 1L, align = 0L) {
  .Call(`_MazamaRollUtils_roll_sd`, x, n, by, align)
}

#' Roll Hampel Detection
#'
#' @param x A numeric vector
#' @param n An integer window size
#' @param by Integer to shift window by
#' @param align Signed Integer window alignment, -1 (left) | 0 (center) | 1 (right)
#'
#' @return numeric vector of length(X)
#' @export
#'
#' @examples
roll_hampel <- function(x, n = 5L, by = 1L, align = 0L) {
  .Call(`_MazamaRollUtils_roll_hampel`, x, n, by, align)
}

#' Roll Maximum
#'
#' @param x A numeric vector
#' @param n An integer window size
#' @param by Integer to shift window by
#' @param align Signed Integer window alignment, -1 (left) | 0 (center) | 1 (right)
#'
#' @return numeric vector of length(X)
#' @export
#'
#' @examples
roll_max <- function(x, n = 5L, by = 1L, align = 0L) {
  .Call(`_MazamaRollUtils_roll_max`, x, n, by, align)
}

#' Roll Minimum
#'
#' @param x A numeric vector
#' @param n An integer window size
#' @param by Integer to shift window by
#' @param align Signed Integer window alignment, -1 (left) | 0 (center) | 1 (right)
#'
#' @return numeric vector of length(X)
#' @export
#'
#' @examples
roll_min <- function(x, n = 5L, by = 1L, align = 0L) {
  .Call(`_MazamaRollUtils_roll_min`, x, n, by, align)
}
