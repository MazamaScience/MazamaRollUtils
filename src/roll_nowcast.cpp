#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>

/* ----- Internal Helpers ----- */

// Compute a single NowCast value from up to 12 hourly measurements.
//
// Uses values in x[start] through x[end], in chronological order.
// Missing values may be NA or NaN.
//
// Returns NA_REAL when:
//   - fewer than 2 valid values exist in the most recent 3 hours
//   - no valid values exist in the window
//   - the final result is not numeric
static double nowcastWindow(Rcpp::NumericVector const& x, int start, int end) {

  // Reverse to most-recent-first order
  std::vector<double> values;
  values.reserve(end - start + 1);

  for (int i = end; i >= start; --i) {
    double value = x[i];
    if (Rcpp::NumericVector::is_na(value) || std::isnan(value)) {
      values.push_back(NA_REAL);
    } else {
      values.push_back(value);
    }
  }

  const int n = values.size();
  if (n == 0) {
    return NA_REAL;
  }

  // Require at least 2 valid values in most recent 3 hours
  int recent_valid = 0;
  for (int i = 0; i < std::min(3, n); ++i) {
    if (!std::isnan(values[i])) {
      recent_valid += 1;
    }
  }

  if (recent_valid < 2) {
    return NA_REAL;
  }

  // Compute min and max among valid values
  bool found_valid = false;
  double min_value = 0.0;
  double max_value = 0.0;

  for (int i = 0; i < n; ++i) {
    if (!std::isnan(values[i])) {
      if (!found_valid) {
        min_value = values[i];
        max_value = values[i];
        found_valid = true;
      } else {
        if (values[i] < min_value) min_value = values[i];
        if (values[i] > max_value) max_value = values[i];
      }
    }
  }

  if (!found_valid) {
    return NA_REAL;
  }

  // EPA NowCast scaling
  double scaled_rate = (max_value - min_value) / max_value;

  double weight_factor = 1.0 - scaled_rate;
  if (weight_factor < 0.5) {
    weight_factor = 0.5;
  }

  // Weighted average
  double weighted_sum = 0.0;
  double weight_sum = 0.0;

  for (int i = 0; i < n; ++i) {
    if (!std::isnan(values[i])) {
      double w = std::pow(weight_factor, i);
      weighted_sum += values[i] * w;
      weight_sum += w;
    }
  }

  if (weight_sum == 0.0) {
    return NA_REAL;
  }

  double result = weighted_sum / weight_sum;

  result = std::round(result * 10.0) / 10.0;

  if (std::isnan(result) || std::isinf(result)) {
    return NA_REAL;
  }

  return result;
}


/* ----- Exported Function ----- */

// Rolling EPA NowCast
//
// Calculates a right-aligned NowCast value at every index using up to the
// most recent 12 hourly values.
//
// Early values use all available data back to the start of the series.
//
// Missing values are allowed, but at least 2 valid values must be present
// in the most recent 3 hours for a value to be returned.
//
// @param x Numeric vector of hourly PM values.
//
// @returns Numeric vector of the same length as 'x', with NA where a NowCast
//   value cannot be calculated.
//
// [[Rcpp::export(".roll_nowcast_cpp")]]
Rcpp::NumericVector roll_nowcast_cpp(Rcpp::NumericVector x) {

  const int length = x.size();
  Rcpp::NumericVector out(length, NA_REAL);

  for (int i = 0; i < length; ++i) {
    int start = std::max(0, i - 11);
    out[i] = nowcastWindow(x, start, i);
  }

  return out;
}
