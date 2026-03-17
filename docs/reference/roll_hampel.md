# Roll Hampel

Apply a moving-window Hampel function to a numeric vector.

## Usage

``` r
roll_hampel(
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

The Hampel filter is a robust outlier detector using Median Absolute
Deviation (MAD).

For every index in the incoming vector `x`, a value is returned that is
the Hampel function of all values in `x` that fall within a window of
width `width`.

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
x <- c(0, 0, 0, 1, 1, 2, 2, 4, 6, 9, 0, 0, 0)
roll_hampel(x, 3)
#>  [1]        NA 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
#>  [8] 0.0000000 0.0000000 0.6744908 0.0000000 0.0000000        NA
```
