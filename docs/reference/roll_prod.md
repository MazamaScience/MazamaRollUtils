# Roll Product

Apply a moving-window product function to a numeric vector.

## Usage

``` r
roll_prod(
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
the product of all values in `x` that fall within a window of width
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

x[1:10]
#>  [1] 13.032808  9.208778  9.413230 10.089091 10.075755  8.240968  9.507955
#>  [8] 10.704980 11.232667  9.767381
roll_prod(x, width = 5)[1:10]
#>  [1]        NA        NA 114844.00  72618.71  74977.97  85266.97  94931.79
#>  [8]  92026.35 118631.23 136870.47
```
