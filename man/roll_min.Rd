% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MazamaRollUtils.R
\name{roll_min}
\alias{roll_min}
\title{Roll Min}
\usage{
roll_min(
  x,
  width = 1L,
  by = 1L,
  align = c("center", "left", "right"),
  na.rm = FALSE
)
}
\arguments{
\item{x}{Numeric vector.}

\item{width}{Integer width of the rolling window.}

\item{by}{Integer shift by which the window is moved each iteration.}

\item{align}{Character position of the return value within the window. One of:
\code{"left" | "center" | "right"}.}

\item{na.rm}{Logical specifying whether \code{NA} values should be removed
before the calculations within each window.}
}
\value{
Numeric vector of the same length as \code{x}.
}
\description{
Apply a moving-window minimum function to a numeric vector.
}
\details{
For every index in the incoming vector \code{x}, a value is returned that
is the minimum of all values in \code{x} that fall within a window of width
\code{width}.

The \code{align} parameter determines the alignment of the return value
within the window. Thus:

\itemize{
  \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
  \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
  \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
}

For large vectors, the\code{by} parameter can be used to force the window
to jump ahead \code{by} indices for the next calculation. Indices that are
skipped over will be assigned \code{NA} values so that the return vector still has
the same length as the incoming vector. This can dramatically speed up
calculations for high resolution time series data.
}
\examples{
library(MazamaRollUtils)

# Example air quality time series
t <- example_pm25$datetime
x <- example_pm25$pm25

plot(t, x, pch = 16, cex = 0.5)
lines(t, roll_max(x, width = 12), col = 'red')
lines(t, roll_min(x, width = 12), col = 'deepskyblue')
title("12-hr Rolling Max and Min")

plot(t, x, pch = 16, cex = 0.5)
points(t, roll_min(x, width = 12, na.rm = TRUE),
       pch = 16, col = 'deepskyblue')
points(t, roll_min(x, width = 12, na.rm = FALSE),
       pch = 16, col = adjustcolor('black', 0.4))
legend("topright", pch = c(16, 16),
       col = c("deepskyblue", adjustcolor("black", 0.4)),
       legend = c("na.rm = TRUE", "na.rm = FALSE"))
title("12-hr Rolling min with/out na.rm")
}
