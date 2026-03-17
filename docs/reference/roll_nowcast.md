# Roll NowCast

Apply the EPA NowCast algorithm to a numeric vector of hourly
particulate matter measurements.

## Usage

``` r
roll_nowcast(x)
```

## Arguments

- x:

  Numeric vector of hourly PM measurements.

## Value

Numeric vector of the same length as `x`.

## Details

The EPA NowCast is a weighted average designed to emphasize more recent
hourly PM values while still using up to the previous 12 hours of data.
The weighting depends on how much concentrations vary within the window:
rapidly changing conditions place more weight on the most recent hours,
while stable conditions allow older hours to contribute more evenly.

For every index in the incoming vector x, a value is returned that is
the NowCast associated with that hour.

This calculation is always right-aligned:

- `[-----------*]` where `*` marks the hour receiving the NowCast value.

Early values use all available data back to the beginning of the series,
so the first 11 positions may be calculated from fewer than 12 hours.

Missing values are allowed, but at least 2 valid values must be present
in the most recent 3 hours or the result for that index will be `NA`.

Returned values are rounded to one decimal place.

## Examples

``` r
x <- c(10, 12, 11, 13, 15, 18, 20, 25, 30, 28, 26, 24, 22)
roll_nowcast(x)
#>  [1]   NA 11.1 11.1 11.8 13.1 15.5 17.9 21.5 25.7 26.9 26.4 25.2 23.6
```
