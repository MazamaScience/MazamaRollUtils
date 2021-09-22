#' @title Example timeseries dataset
#' @format A dataframe with columns "datetime" and "pm25".
#' @description The \code{example_pm25_data} dataset provides example timeseries
#' data for practicing and code examples. This data represents hourly air quality
#' measurements.
#'
#' This dataset was was generated on 2021-09-22 by running:
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

