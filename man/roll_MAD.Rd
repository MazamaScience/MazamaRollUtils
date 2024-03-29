% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MazamaRollUtils.R
\name{roll_MAD}
\alias{roll_MAD}
\title{Roll MAD}
\usage{
roll_MAD(
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

\item{by}{Integer shift to use when sliding the window to the next location}

\item{align}{Character position of the return value within the window. One of:
\code{"left" | "center" | "right"}.}

\item{na.rm}{Logical specifying whether \code{NA} values should be removed
before the calculations within each window.}
}
\value{
Numeric vector of the same length as \code{x}.
}
\description{
Apply a moving-window Median Absolute Deviation function to a numeric vector.
}
\details{
For every index in the incoming vector \code{x}, a value is returned that
is the Median Absolute Deviation (MAD) of all values in \code{x} that fall within a window of width
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

# Wikipedia example
x <- c(0, 0, 0, 1, 1, 2, 2, 4, 6, 9, 0, 0, 0)
roll_MAD(x, 3)
roll_MAD(x, 5)
roll_MAD(x, 7)
}
