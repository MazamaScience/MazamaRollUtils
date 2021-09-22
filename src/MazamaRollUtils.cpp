#include <Rcpp.h>
#include <stdlib.h>

/* ----- Roll Class ----- */

class Roll {

public:

  // Initialize Roller
  void init(
      Rcpp::NumericVector x,
      int width,
      int by,
      Rcpp::String const& align,
      Rcpp::LogicalVector na_rm,
      Rcpp::Nullable<Rcpp::NumericVector> weights
  ) {

    if (width < 1) {
      Rcpp::stop("Window 'width' must be 1 or larger");
    }
    if (width > x.size()) {
      Rcpp::stop("Window 'width' cannot be larger than 'x'");
    }
    if (by < 1) {
      Rcpp::stop("Increment 'by' must be 1 or larger");
    }
    if (by > x.size()) {
      Rcpp::stop("Increment 'by' cannot be larger than 'x'");
    }

    // Initialize private vars
    x_ = x;
    width_ = width;
    by_ = by;
    na_rm_ = na_rm;
    weights_ = Rcpp::rep(1.0, width_);

    if (na_rm_[0]) {
      Rprintf("'na_rm_' evaluates as TRUE");
    } else {
      Rprintf("'na_rm_' evaluates as FALSE");
    }

    // Default weights
    if (!weights.isNull()) {
      // See:  https://stackoverflow.com/questions/43388698/rcpp-how-can-i-get-the-size-of-a-rcppnullable-numericvector
      Rcpp::NumericVector w(weights.get());
      if (w.size() != width_) {
        Rcpp::stop("'weights' must be either NULL or a vector of the same length as the window 'width'");
      }
      for (int i = 0; i < width_; ++i) {
        if (w[i] < 0) {
          Rcpp::stop("All 'weights' must be positive values or zero. Negative weights are not supported.");
        };
      }
      // See:  https://en.cppreference.com/w/cpp/algorithm/accumulate
      double weights_sum = std::accumulate(w.begin(), w.end(), (double)0);
      if ( weights_sum == 0 ) {
        Rcpp::stop("All 'weights' are zero. Please include non-zero values in 'weights'.");
      }
      double normalization = (double)width_ / weights_sum;
      for (int i = 0; i < width_; ++i) {
        weights_[i] = w[i] * normalization;
      }
    }

    // Additional private vars
    length_ = x.size();
    half_width_ = width / 2;   // truncated division rounds down

    // Initialize start and end
    if (align == "left") {
      align_code_ = -1;
      start_ = 0;
      end_ = length_ - (width - 1);
    } else if (align == "center") {
      align_code_ = 0;
      start_ = half_width_;
      end_ = length_ - half_width_;
    } else if (align == "right") {
      align_code_ = 1;
      start_ = width - 1;
      end_ = length_;
    } else {
      Rcpp::stop("Window alignment 'align' must be either 'left', 'center' or 'right'");
    }

  }

  // // Rolling Hampel
  // Rcpp::NumericVector hampel() {
  //   Rcpp::NumericVector out(length_, NA_REAL);
  //   for (int i = start_; i < end_; i += by_) {
  //     out[i] = windowHampel(i);
  //   }
  //   return out;
  // }

  // Rolling Maximum
  Rcpp::NumericVector max() {
    Rcpp::NumericVector out(length_, NA_REAL);
    for (int i = start_; i < end_; i += by_) {
      out[i] = windowMax(i);
    }
    return out;
  }

  // Rolling Mean
  Rcpp::NumericVector mean() {
    Rcpp::NumericVector out(length_, NA_REAL);
    for (int i = start_; i < end_; i += by_) {
      out[i] = windowMean(i);
    }
    return out;
  }

  // Rolling Median
  Rcpp::NumericVector median() {
    Rcpp::NumericVector out(length_, NA_REAL);
    for (int i = start_; i < end_; i += by_) {
      out[i] = windowMedian(i);
    }
    return out;
  }

  // Rolling Minimum
  Rcpp::NumericVector min() {
    Rcpp::NumericVector out(length_, NA_REAL);
    for (int i = start_; i < end_; i += by_) {
      out[i] = windowMin(i);
    }
    return out;
  }

  // Rolling Product
  Rcpp::NumericVector prod() {
    Rcpp::NumericVector out(length_, NA_REAL);
    for (int i = start_; i < end_; i += by_) {
      out[i] = windowProd(i);
    }
    return out;
  }

  // Rolling Standard Deviation
  Rcpp::NumericVector sd() {
    Rcpp::NumericVector out(length_, NA_REAL);
    for (int i = start_; i < end_; i += by_) {
      out[i] = sqrt(windowVar(i));
    }
    return out;
  }

  // Rolling Sum
  Rcpp::NumericVector sum() {
    Rcpp::NumericVector out(length_, NA_REAL);
    for (int i = start_; i < end_; i += by_) {
      out[i] = windowSum(i);
    }
    return out;
  }

  // Rolling Variance
  Rcpp::NumericVector var() {
    Rcpp::NumericVector out(length_, NA_REAL);
    for (int i = start_; i < end_; i += by_) {
      out[i] = windowVar(i);
    }
    return out;
  }

private:

  Rcpp::NumericVector x_;        // data
  int width_;                    // window width
  int by_;                       // increment
  int align_code_;               // alignment
  Rcpp::LogicalVector na_rm_;    // NA removal
  Rcpp::NumericVector weights_;  // window weights
  int length_;                   // data length
  int half_width_;               // window half-width
  int start_;                    // start index
  int end_;                      // end index

  // // Window Hampel
  // double windowHampel(const int &index) {
  //   const double kappa = 1.4826;
  //   double mediawidth_i = windowMedian(index);
  //   Rcpp::NumericVector tmp(width_, NA_REAL);
  //   for (int i = 0; i < width_; ++i) { // MAD
  //     int s = index - half_width_ + i;
  //     tmp[i] = std::fabs(x_[s] - mediawidth_i);
  //   }
  //   std::nth_element(tmp.begin(), tmp.begin() + half_width_, tmp.end());
  //   double mad = tmp[half_width_];
  //   return std::fabs(x_[index] - mediawidth_i) / (kappa * mad);
  // }

  // Window Maximum
  double windowMax(const int &index) {
    double max = x_[index];
    for (int i = 0; i < width_; ++i) {
      int s;
      switch (align_code_) {
      case -1:
        s = index + i;
        break;
      case 0:
        s = index - half_width_ + i;
        break;
      case 1:
        s = index - (width_ - 1) + i;
        break;
      }
      if ( s < 0 ) {
        return NA_REAL;
      } else if (ISNAN(x_[s])) {
        return NA_REAL;
      } else {
        if (x_[s] > max) max = x_[s];
      }
    }
    return max;
  }

  // Window Mean
  double windowMean(const int &index) {
    double mean = 0;
    for (int i = 0; i < width_; ++i) {
      int s;
      switch (align_code_) {
      case -1:
        s = index + i;
        break;
      case 0:
        s = index - half_width_ + i;
        break;
      case 1:
        s = index - (width_ - 1) + i;
        break;
      }
      if ( s < 0 ) {
        return NA_REAL;
      } else if (ISNAN(x_[s])) {
        return NA_REAL;
      } else {
        mean += x_[s] * weights_[i];
      }
    }
    mean /= (double)width_;
    return mean;
  }

  // Window Median
  double windowMedian(const int &index) {
    Rcpp::NumericVector tmp(width_, NA_REAL);
    for (int i = 0; i < width_; ++i) {
      int s;
      switch (align_code_) {
      case -1:
        s = index + i;
        break;
      case 0:
        s = index - half_width_ + i;
        break;
      case 1:
        s = index - (width_ - 1) + i;
        break;
      }
      if ( s < 0 ) {
        return NA_REAL;
      } else if (ISNAN(x_[s])) {
        return NA_REAL;
      } else {
        tmp[i] = x_[s];
      }
    }
    std::nth_element(tmp.begin(), tmp.begin() + half_width_, tmp.end());
    return tmp[half_width_];
  }

  // Window Minimum
  double windowMin(const int &index) {
    double min = x_[index];
    for (int i = 0; i < width_; ++i) {
      int s;
      switch (align_code_) {
      case -1:
        s = index + i;
        break;
      case 0:
        s = index - half_width_ + i;
        break;
      case 1:
        s = index - (width_ - 1) + i;
        break;
      }
      if ( s < 0 ) {
        return NA_REAL;
      } else if (ISNAN(x_[s])) {
        return NA_REAL;
      } else {
        if (x_[s] < min) min = x_[s];
      }
    }
    return min;
  }

  // Window Product
  double windowProd(const int &index) {
    double prod = 1;
    for (int i = 0; i < width_; ++i) {
      int s;
      switch (align_code_) {
      case -1:
        s = index + i;
        break;
      case 0:
        s = index - half_width_ + i;
        break;
      case 1:
        s = index - (width_ - 1) + i;
        break;
      }
      if ( s < 0 ) {
        return NA_REAL;
      } else if (ISNAN(x_[s])) {
        return NA_REAL;
      } else {
        prod *= x_[s];
      }
    }
    return prod;
  }

  // Window Sum
  double windowSum(const int &index) {
    double sum = 0;
    for (int i = 0; i < width_; ++i) {
      int s;
      switch (align_code_) {
      case -1:
        s = index + i;
        break;
      case 0:
        s = index - half_width_ + i;
        break;
      case 1:
        s = index - (width_ - 1) + i;
        break;
      }
      if ( s < 0 ) {
        return NA_REAL;
      } else if (ISNAN(x_[s])) {
        return NA_REAL;
      } else {
        sum += x_[s];
      }
    }
    return sum;
  }

  // Window Variance
  double windowVar(const int &index) {
    double var = 0;
    double mean = windowMean(index);
    for (int i = 0; i < width_; ++i) {
      int s;
      switch (align_code_) {
      case -1:
        s = index + i;
        break;
      case 0:
        s = index - half_width_ + i;
        break;
      case 1:
        s = index - (width_ - 1) + i;
        break;
      }
      if ( s < 0 ) {
        return NA_REAL;
      } else if (ISNAN(x_[s])) {
        return NA_REAL;
      } else {
        var += (x_[s] - mean) * (x_[s] - mean);
      }
    }
    var /= ((double)width_ - (double)1);
    return var;
  }

};

/* ----- Rcpp Exported ----- */

// //' @title Roll Hampel
// //'
// //' @description Apply a moving-window hampel value function to a numeric
// //' vector.
// //'
// //' @details Each window of \code{n}-length is applied \code{weight} and then
// //' slid/shifted/rolled \code{by} a positive integer amount about the window's
// //' \code{align}-ment index.
// //'
// //' The \code{align} parameter determines the alignment of the return value
// //' within the window. Thus:
// //'
// //' \itemize{
// //'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
// //'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
// //'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
// //' }
// //'
// //' @param x Numeric vector.
// //' @param width Integer width of the rolling window.
// //' @param by An integer to shift the window by.
// //' @param align A signed integer representing the position of the return value within the window.
// //' \code{-1(left) | 0(center) | 1(right)}.
// //'
// //' @return Numeric vector of the same length as \code{x}.
// //'
// //' @examples
// //' # load airquality
// //' data("airquality")
// //'
// //' # calculate moving hampel value of next 3 measurements
// //' roll_mean(airquality$Temp, width = 3, align = 1)
// // [[Rcpp::export]]
// Rcpp::NumericVector roll_hampel(
//     Rcpp::NumericVector x,
//     unsigned int width = 5,
//     int by = 1,
//     int align = 0
// ) {
//   Roll roll;
//   Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue;
//   roll.init(x, width, by, align, na_rm, weights);
//   return roll.hampel();
// }

//' @title Roll Max
//'
//' @description Apply a moving-window maximum function to a numeric vector.
//'
//' @details
//'
//' For every index in the incoming vector \code{x}, a value is returned that
//' is the maximum of all values in \code{x} that fall within a window of width
//' \code{width}.
//'
//' The \code{align} parameter determines the alignment of the return value
//' within the window. Thus:
//'
//' \itemize{
//'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
//'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
//'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
//' }
//'
//' For large vectors, the\code{by} parameter can be used to force the window
//' to jump ahead \code{by} indices for the next calculation. Indices that are
//' skipped over will be assigned \code{NA} values so that the return vector still has
//' the same length as the incoming vector. This can dramatically speed up
//' calculations for high resolution time series data.
//'
//'
//' @param x Numeric vector.
//' @param width Integer width of the rolling window.
//' @param by Integer shift to use when sliding the window to the next location
//' @param align Character position of the return value within the window --
//' \code{"left" | "center" | "right"}.
//' @param na_rm Logical specifying whether \code{NA} values should be removed
//' before the calculations within each window.
//'
//' @return Numeric vector of the same length as \code{x}.
//'
//' @examples
//' library(MazamaRollUtils)
//'
//' # R default air quality time series
//' t <- datasets::airquality$Temp
//'
//' plot(t)
//' lines(roll_max(t, width = 5), col = 'salmon')
// [[Rcpp::export]]
Rcpp::NumericVector roll_max(
    Rcpp::NumericVector x,
    unsigned int width = 5,
    int by = 1,
    Rcpp::String const& align = "center",
    Rcpp::LogicalVector na_rm = Rcpp::LogicalVector::create(0)
) {
  Roll roll;
  Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue;
  roll.init(x, width, by, align, na_rm, weights);
  return roll.max();
}

//' @title Roll Mean
//'
//' @description Apply a moving-window mean function to a numeric vector.
//'
//' @details
//'
//' For every index in the incoming vector \code{x}, a value is returned that
//' is the mean of all values in \code{x} that fall within a window of width
//' \code{width}.
//'
//' The \code{align} parameter determines the alignment of the return value
//' within the window. Thus:
//'
//' \itemize{
//'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
//'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
//'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
//' }
//'
//' For large vectors, the\code{by} parameter can be used to force the window
//' to jump ahead \code{by} indices for the next calculation. Indices that are
//' skipped over will be assigned \code{NA} values so that the return vector still has
//' the same length as the incoming vector. This can dramatically speed up
//' calculations for high resolution time series data.
//'
//' The \code{roll_mean()} function supports an additional \code{weights}
//' argument that can be used to calculate a "weighted moving average" --
//' a convolution of the incoming data with the \emph{kernel} (weighting function)
//' provided in \code{weights}.
//'
//' @param x Numeric vector.
//' @param width Integer width of the rolling window.
//' @param by An integer to shift the window by.
//' @param align Character position of the return value within the window --
//' \code{"left" | "center" | "right"}.
//' @param na_rm Logical specifying whether \code{NA} values should be removed
//' before the calculations within each window.
//' @param weights A numeric vector of size \code{width} specifying each window
//' index weight. If \code{NULL}, unit weights are used.
//'
//' @return Numeric vector of the same length as \code{x}.
//'
//' @examples
//' library(MazamaRollUtils)
//'
//' # R default air quality time series
//' t <- datasets::airquality$Temp
//'
//' plot(t)
//' lines(roll_mean(t, width = 5), col = 'salmon')
// [[Rcpp::export]]
Rcpp::NumericVector roll_mean(
    Rcpp::NumericVector x,
    unsigned int width = 5,
    int by = 1,
    Rcpp::String const& align = "center",
    Rcpp::LogicalVector na_rm = Rcpp::LogicalVector::create(0),
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue
) {
  Roll roll;
  roll.init(x, width, by, align, na_rm, weights);
  return roll.mean();
}

//' @title Roll Median
//'
//' @description Apply a moving-window median function to a numeric vector.
//'
//' @details
//'
//' For every index in the incoming vector \code{x}, a value is returned that
//' is the median of all values in \code{x} that fall within a window of width
//' \code{width}.
//'
//' The \code{align} parameter determines the alignment of the return value
//' within the window. Thus:
//'
//' \itemize{
//'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
//'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
//'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
//' }
//'
//' For large vectors, the\code{by} parameter can be used to force the window
//' to jump ahead \code{by} indices for the next calculation. Indices that are
//' skipped over will be assigned \code{NA} values so that the return vector still has
//' the same length as the incoming vector. This can dramatically speed up
//' calculations for high resolution time series data.
//'
//' @param x Numeric vector.
//' @param width Integer width of the rolling window.
//' @param by An integer to shift the window by.
//' @param align Character position of the return value within the window --
//' \code{"left" | "center" | "right"}.
//' @param na_rm Logical specifying whether \code{NA} values should be removed
//' before the calculations within each window.
//'
//' @return Numeric vector of the same length as \code{x}.
//'
//' @examples
//' library(MazamaRollUtils)
//'
//' # R default air quality time series
//' t <- datasets::airquality$Temp
//'
//' plot(t)
//' lines(roll_median(t, width = 5), col = 'salmon')
// [[Rcpp::export]]
Rcpp::NumericVector roll_median (
    Rcpp::NumericVector x,
    unsigned int width = 5,
    int by = 1,
    Rcpp::String const& align = "center",
    Rcpp::LogicalVector na_rm = Rcpp::LogicalVector::create(0)
) {
  Roll roll;
  Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue;
  roll.init(x, width, by, align, na_rm, weights);
  return roll.median();
}

//' @title Roll Min
//'
//' @description Apply a moving-window minimum function to a numeric vector.
//'
//' @details
//'
//' For every index in the incoming vector \code{x}, a value is returned that
//' is the minimum of all values in \code{x} that fall within a window of width
//' \code{width}.
//'
//' The \code{align} parameter determines the alignment of the return value
//' within the window. Thus:
//'
//' \itemize{
//'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
//'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
//'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
//' }
//'
//' For large vectors, the\code{by} parameter can be used to force the window
//' to jump ahead \code{by} indices for the next calculation. Indices that are
//' skipped over will be assigned \code{NA} values so that the return vector still has
//' the same length as the incoming vector. This can dramatically speed up
//' calculations for high resolution time series data.
//'
//' @param x Numeric vector.
//' @param width Integer width of the rolling window.
//' @param by An integer to shift the window by.
//' @param align Character position of the return value within the window --
//' \code{"left" | "center" | "right"}.
//' @param na_rm Logical specifying whether \code{NA} values should be removed
//' before the calculations within each window.
//'
//' @return Numeric vector of the same length as \code{x}.
//'
//' @examples
//' library(MazamaRollUtils)
//'
//' # R default air quality time series
//' t <- datasets::airquality$Temp
//'
//' plot(t)
//' lines(roll_min(t, width = 5), col = 'salmon')
// [[Rcpp::export]]
Rcpp::NumericVector roll_min(
    Rcpp::NumericVector x,
    unsigned int width = 5,
    int by = 1,
    Rcpp::String const& align = "center",
    Rcpp::LogicalVector na_rm = Rcpp::LogicalVector::create(0)
) {
  Roll roll;
  Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue;
  roll.init(x, width, by, align, na_rm, weights);
  return roll.min();
}

//' @title Roll Product
//'
//' @description Apply a moving-window product function to a numeric vector.
//'
//' @details
//'
//' For every index in the incoming vector \code{x}, a value is returned that
//' is the product of all values in \code{x} that fall within a window of width
//' \code{width}.
//'
//' The \code{align} parameter determines the alignment of the return value
//' within the window. Thus:
//'
//' \itemize{
//'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
//'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
//'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
//' }
//'
//' For large vectors, the\code{by} parameter can be used to force the window
//' to jump ahead \code{by} indices for the next calculation. Indices that are
//' skipped over will be assigned \code{NA} values so that the return vector still has
//' the same length as the incoming vector. This can dramatically speed up
//' calculations for high resolution time series data.
//'
//' @param x Numeric vector.
//' @param width Integer width of the rolling window.
//' @param by An integer to shift the window by.
//' @param align Character position of the return value within the window --
//' \code{"left" | "center" | "right"}.
//' @param na_rm Logical specifying whether \code{NA} values should be removed
//' before the calculations within each window.
//'
//' @return Numeric vector of the same length as \code{x}.
//'
//' @examples
//' library(MazamaRollUtils)
//'
//' # R default air quality time series
//' t <- datasets::airquality$Temp
//'
//' t[1:10]
//' roll_prod(t, width = 5)[1:10]
// [[Rcpp::export]]
Rcpp::NumericVector roll_prod(
    Rcpp::NumericVector x,
    unsigned int width = 5,
    int by = 1,
    Rcpp::String const& align = "center",
    Rcpp::LogicalVector na_rm = Rcpp::LogicalVector::create(0)
) {
  Roll roll;
  Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue;
  roll.init(x, width, by, align, na_rm, weights);
  return roll.prod();
}

//' @title Roll Standard Deviation
//'
//' @description Apply a moving-window standard deviation function to a
//' numeric vector.
//'
//' @details
//'
//' For every index in the incoming vector \code{x}, a value is returned that
//' is the standard deviation of all values in \code{x} that fall within a window of width
//' \code{width}.
//'
//' The \code{align} parameter determines the alignment of the return value
//' within the window. Thus:
//'
//' \itemize{
//'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
//'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
//'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
//' }
//'
//' For large vectors, the\code{by} parameter can be used to force the window
//' to jump ahead \code{by} indices for the next calculation. Indices that are
//' skipped over will be assigned \code{NA} values so that the return vector still has
//' the same length as the incoming vector. This can dramatically speed up
//' calculations for high resolution time series data.
//'
//' @param x Numeric vector.
//' @param width Integer width of the rolling window.
//' @param by An integer to shift the window by.
//' @param align Character position of the return value within the window --
//' \code{"left" | "center" | "right"}.
//' @param na_rm Logical specifying whether \code{NA} values should be removed
//' before the calculations within each window.
//'
//' @return Numeric vector of the same length as \code{x}.
//'
//' @examples
//' library(MazamaRollUtils)
//'
//' # R default air quality time series
//' t <- datasets::airquality$Temp
//'
//' t[1:10]
//' roll_sd(t, width = 5)[1:10]
// [[Rcpp::export]]
Rcpp::NumericVector roll_sd(
    Rcpp::NumericVector x,
    unsigned int width = 5,
    int by = 1,
    Rcpp::String const& align = "center",
    Rcpp::LogicalVector na_rm = Rcpp::LogicalVector::create(0)
) {
  Roll roll;
  Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue;
  roll.init(x, width, by, align, na_rm, weights);
  return roll.sd();
}

//' @title Roll Sum
//'
//' @description Apply a moving-window sum to a numeric vector.
//'
//' @details
//'
//' For every index in the incoming vector \code{x}, a value is returned that
//' is the sum of all values in \code{x} that fall within a window of width
//' \code{width}.
//'
//' The \code{align} parameter determines the alignment of the return value
//' within the window. Thus:
//'
//' \itemize{
//'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
//'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
//'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
//' }
//'
//' For large vectors, the\code{by} parameter can be used to force the window
//' to jump ahead \code{by} indices for the next calculation. Indices that are
//' skipped over will be assigned \code{NA} values so that the return vector still has
//' the same length as the incoming vector. This can dramatically speed up
//' calculations for high resolution time series data.
//'
//' @param x Numeric vector.
//' @param width Integer width of the rolling window.
//' @param by An integer to shift the window by.
//' @param align Character position of the return value within the window --
//' \code{"left" | "center" | "right"}.
//' @param na_rm Logical specifying whether \code{NA} values should be removed
//' before the calculations within each window.
//'
//' @return Numeric vector of the same length as \code{x}.
//'
//' @examples
//' library(MazamaRollUtils)
//'
//' # R default air quality time series
//' t <- datasets::airquality$Temp
//'
//' t[1:10]
//' roll_sum(t, width = 5)[1:10]
// [[Rcpp::export]]
Rcpp::NumericVector roll_sum(
    Rcpp::NumericVector x,
    unsigned int width = 5,
    int by = 1,
    Rcpp::String const& align = "center",
    Rcpp::LogicalVector na_rm = Rcpp::LogicalVector::create(0)
) {
  Roll roll;
  Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue;
  roll.init(x, width, by, align, na_rm, weights);
  return roll.sum();
}

//' @title Roll Variance
//'
//' @description Apply a moving-window variance function to a numeric vector.
//'
//' @details
//'
//' For every index in the incoming vector \code{x}, a value is returned that
//' is the variance of all values in \code{x} that fall within a window of width
//' \code{width}.
//'
//' The \code{align} parameter determines the alignment of the return value
//' within the window. Thus:
//'
//' \itemize{
//'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
//'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
//'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
//' }
//'
//' For large vectors, the\code{by} parameter can be used to force the window
//' to jump ahead \code{by} indices for the next calculation. Indices that are
//' skipped over will be assigned \code{NA} values so that the return vector still has
//' the same length as the incoming vector. This can dramatically speed up
//' calculations for high resolution time series data.
//'
//' @param x Numeric vector.
//' @param width Integer width of the rolling window.
//' @param by An integer to shift the window by.
//' @param align Character position of the return value within the window --
//' \code{"left" | "center" | "right"}.
//' @param na_rm Logical specifying whether \code{NA} values should be removed
//' before the calculations within each window.
//'
//' @return Numeric vector of the same length as \code{x}.
//'
//' @examples
//' library(MazamaRollUtils)
//'
//' # R default air quality time series
//' t <- datasets::airquality$Temp
//'
//' t[1:10]
//' roll_var(t, width = 5)[1:10]
// [[Rcpp::export]]
Rcpp::NumericVector roll_var(
    Rcpp::NumericVector x,
    unsigned int width = 5,
    int by = 1,
    Rcpp::String const& align = "center",
    Rcpp::LogicalVector na_rm = Rcpp::LogicalVector::create(0)
) {
  Roll roll;
  Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue;
  roll.init(x, width, by, align, na_rm, weights);
  return roll.var();
}






