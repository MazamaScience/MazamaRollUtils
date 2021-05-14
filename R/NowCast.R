#' Calculate NowCast
#'
#' As described
#' \href{https://usepa.servicenowservices.com/airnow?id=kb_article_view&sys_id=fed0037b1b62545040a1a7dbe54bcbd4}{here}.
#'
#' @param x A numeric vector.
#' @param n An integer window length.
#' @param threshold A weight factor threshold.
#'
#' @return A numeric vector of length(x)
#' @export
#'
#' @examples
#' data("airquality")
#'
#' nowcast(airquality$Ozone)
nowcast <- function(x, n = 12, threshold = 0.5) {

  if (n > length(x)) {
    stop("Window size `n` is larger than length(`x`).")
  }

  # Use `zoo's` rollapply generic to apply algorithm to each window of n by 1
  zoo::rollapply(
    x,
    width = n,
    by = 1,
    FUN = function(x) {

      # Get the weight factor
      weight_factor <- 1 - (max(x) - min(x)) / max(x)

      # Correct the weight factor - i.e. if weight < threshold --> threshold
      corrected_weight_factor <- ifelse(
        weight_factor < threshold,
        threshold,
        weight_factor
      )

      # power hour
      powers <- seq(0, n - 1, 1)

      # Compute NowCast
      nc <- sum(x * corrected_weight_factor ** powers, na.rm = TRUE) /
          sum(corrected_weight_factor ** powers, na.rm = TRUE)

      return(nc)

    }
  )

}
