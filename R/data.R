#' Example timeseries dataset
#'
#' @docType data
#' @format A data frame with 2 columns:
#' \describe{
#'   \item{datetime}{POSIXct timestamp of the observation}
#'   \item{pm25}{PM2.5 concentration (µg/m³)}
#' }
#'
#' @description
#' The `example_pm25` dataset provides example timeseries data for practicing
#' and code examples. This dataset represents hourly air quality measurements.
#'
#' The dataset was generated on 2021-09-22 by running:
#'
#' \preformatted{
#' library(AirSensor)
#'
#' example_pm25 <- example_sensor$data
#' names(example_pm25) <- c("datetime", "pm25")
#'
#' save(example_pm25, file = "data/example_pm25.rda")
#' }
"example_pm25"
