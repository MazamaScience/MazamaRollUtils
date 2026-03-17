# Roll MAD

Apply a moving-window Median Absolute Deviation function to a numeric
vector.

## Usage

``` r
roll_MAD(
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
the Median Absolute Deviation (MAD) of all values in `x` that fall
within a window of width `width`.

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
# Wikipedia example
x <- c(0, 0, 0, 1, 1, 2, 2, 4, 6, 9, 0, 0, 0)
roll_MAD(x, 3)
#>  [1] NA  0  0  0  0  0  0  2  2  3  0  0 NA
roll_MAD(x, 5)
#>  [1] NA NA  0  1  1  1  1  2  2  4  0 NA NA
roll_MAD(x, 7)
#>  [1] NA NA NA  1  1  1  1  2  2  2 NA NA NA
```
