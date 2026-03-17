# Roll Max

Apply a moving-window maximum function to a numeric vector.

## Usage

``` r
roll_max(
  x,
  width = 1L,
  by = 1L,
  align = c("center", "left", "right"),
  na.rm = FALSE
)
```

## Arguments

- x:

  Numeric vector.

- width:

  Integer width of the rolling window.

- by:

  Integer shift by which the window is moved each iteration.

- align:

  Character position of the return value within the window. One of:
  `"left" | "center" | "right"`.

- na.rm:

  Logical specifying whether `NA` values should be removed before the
  calculations within each window.

## Value

Numeric vector of the same length as `x`.

## Details

For every index in the incoming vector `x`, a value is returned that is
the maximum of all values in `x` that fall within a window of width
`width`.

The `align` parameter determines the alignment of the return value
within the window. Thus:

- `align = "left" [*------]` will cause the returned vector to have
  width - 1 `NA` values at the right end.

- `align = "center" [---*---]` will cause the returned vector to have
  `NA` values at either end as needed for centered alignment.

- `align = "right" [------*]` will cause the returned vector to have
  width - 1 `NA` values at the left end.

For large vectors, the `by` parameter can be used to force the window to
jump ahead `by` indices for the next calculation. Indices that are
skipped over will be assigned `NA` values so that the return vector
still has the same length as the incoming vector. This can dramatically
speed up calculations for high resolution time series data.

## Examples

``` r
# Example air quality time series
t <- example_pm25$datetime
x <- example_pm25$pm25

plot(t, x, pch = 16, cex = 0.5)
lines(t, roll_max(x, width = 12), col = "red")
lines(t, roll_min(x, width = 12), col = "deepskyblue")
title("12-hr Rolling Max and Min")


plot(t, x, pch = 16, cex = 0.5)
points(t, roll_max(x, width = 12, na.rm = TRUE),
       pch = 16, col = "red")
points(t, roll_max(x, width = 12, na.rm = FALSE),
       pch = 16, col = adjustcolor("black", 0.4))
legend("topright", pch = c(1, 16),
       col = c("red", adjustcolor("black", 0.4)),
       legend = c("na.rm = TRUE", "na.rm = FALSE"))
title("12-hr Rolling max with/out na.rm")
```
