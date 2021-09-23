[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/MazamaTimeSeries)](https://cran.r-project.org/package=MazamaTimeSeries)
[![Downloads](http://cranlogs.r-pkg.org/badges/MazamaTimeSeries)](https://cran.r-project.org/package=MazamaTimeSeries)
[![Build Status](https://travis-ci.org/MazamaScience/MazamaTimeSeries.svg?branch=master)](https://travis-ci.org/MazamaScience/MazamaTimeSeries)


# MazamaRollUtils

```
A suite of utility functions for calculating rolling mins, means, 
maxes and other functions.
```

## Background

Analasys of time series data often involves applying "rolling" functions to calculate,
_e.g._ a "moving average". These functions are straightforward to write in any language
and it makes sense to have C++ versions of common rolling functions available
to R as they dramatically speed up calculations. Several packages exist that 
provide some version of this functionality:

* [zoo](https://cran.r-project.org/web/packages/zoo/index.html) -- 
core R package with a specific data model
* [seismicRoll](https://cran.r-project.org/web/packages/seismicRoll/index.html) -- rolling functions focused on seismology
* [RcppRoll](https://cran.r-project.org/web/packages/RcppRoll/index.html) --
rolling functions for basic statistics

Our goal in creating a new package of C++ rolling functions is to build up a
suite of functions useful in environmental time series analysis. We want these
functions to be available in a neutral environment with no underlying data model. 
The functions are as straightforward to use as is reasonably possible with a 
target audience of data analysts at any level of R expertise.

## Installation

Install from CRAN with:

```
install.packages('MazamaRollUtils')
```

Install the latest version from GitHub with:

```
devtools::install_github("MazamaScience/MazamaRollUtils")
```

## Examples

```
library(MazamaRollUtils)

# Example air quality time series
t <- example_pm25$datetime
x <- example_pm25$pm25

plot(t, x)
lines(t, roll_max(x, width = 12), col = 'salmon')
lines(t, roll_min(x, width = 12), col = 'light blue')
```
----

This project is supported by Mazama Science.

