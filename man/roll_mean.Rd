% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MazamaRollUtils.R
\name{roll_mean}
\alias{roll_mean}
\title{Roll Mean}
\usage{
roll_mean(
  x,
  width = 1L,
  by = 1L,
  align = c("center", "left", "right"),
  na.rm = FALSE,
  weights = NULL
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

\item{weights}{Numeric vector of size \code{width} specifying each window
index weight. If \code{NULL}, unit weights are used.}
}
\value{
Numeric vector of the same length as \code{x}.
}
\description{
Apply a moving-window mean function to a numeric vector.
}
\details{
For every index in the incoming vector \code{x}, a value is returned that
is the mean of all values in \code{x} that fall within a window of width
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

The \code{roll_mean()} function supports an additional \code{weights}
argument that can be used to calculate a "weighted moving average" --
a convolution of the incoming data with the \emph{kernel} (weighting function)
provided in \code{weights}.
}
\examples{
library(MazamaRollUtils)

# Example air quality time series
t <- example_pm25$datetime
x <- example_pm25$pm25

plot(t, x, pch = 16, cex = 0.5)
lines(t, roll_mean(x, width = 3), col = "goldenrod")
lines(t, roll_mean(x, width = 23), col = "purple")
legend("topright", lty = c(1, 1),
       col = c("goldenrod", "purple"),
       legend = c("3-hr mean", "12-hr mean"))
title("3- and 23-hr Rolling mean")
}
