#include <Rcpp.h>
#include <stdlib.h>

/* ----- Roll Class ----- */

class Roll {

public:

  // Initialize Roller
  void init(
      Rcpp::NumericVector x,
      int width,
      Rcpp::Nullable<Rcpp::NumericVector> weights,
      int by,
      int align
  ) {

    // Checks
    if (width > x.size()) {
      Rcpp::stop("Window 'n' cannot be larger than 'x'");
    }
    if (by > x.size()) {
      Rcpp::stop("Increment 'by' cannot be larger than 'x'");
    }
    if (pow(align, 2) > 1) {
      Rcpp::stop("Window align must be either -1 (left), 0 (center), or +1 (right)");
    }

    // Default weights
    if (weights.isNull()) {
      weights_ = Rcpp::rep(1, width);
    } else {
      // See:  https://stackoverflow.com/questions/43388698/rcpp-how-can-i-get-the-size-of-a-rcppnullable-numericvector
      if (weights.isNotNull()) {
        Rcpp::NumericVector w(weights.get());
        if (w.size() != width) {
          Rcpp::stop("'weights' must be either NULL or a vector of the same length as window size 'n'");
        }
      }
      weights_ = weights;
    }

    // Initialize private vars
    x_ = x;
    width_ = width;
    by_ = by;
    align_ = align;

    // Additional private vars
    length_ = x.size();
    half_width_ = width / 2;   // truncated division rounds down

    // Initialize start and end
    switch (align) {
      case -1:
        start_ = 0;
        end_ = length_ - (width - 1);
        break;
      case 0:
        start_ = half_width_;
        end_ = length_ - half_width_;
        break;
      case 1:
        start_ = width - 1;
        end_ = length_;
        break;
    }

  }

  // Rolling Hampel
  Rcpp::NumericVector hampel() {
    Rcpp::NumericVector out(length_, NA_REAL);
    for (int i = start_; i < end_; i += by_) {
      out[i] = windowHampel(i);
    }
    return out;
  }

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
  Rcpp::NumericVector weights_;  // window weights
  int by_;                       // increment
  int align_;                    // alignment
  int length_;                   // data length
  int half_width_;               // window half-width
  int start_;                    // start index
  int end_;                      // end index

  // Window Hampel
  double windowHampel(const int &index) {
    const double kappa = 1.4826;
    double mediawidth_i = windowMedian(index);
    Rcpp::NumericVector tmp(width_, NA_REAL);
    for (int i = 0; i < width_; ++i) { // MAD
      int s = index - half_width_ + i;
      tmp[i] = std::fabs(x_[s] - mediawidth_i) * weights_[i];
    }
    std::nth_element(tmp.begin(), tmp.begin() + half_width_, tmp.end());
    double mad = tmp[half_width_];
    return std::fabs(x_[index] - mediawidth_i) / (kappa * mad);
  }

  // Window Maximum
  double windowMax(const int &index) {
    double max = x_[index];
    for (int i = 0; i < width_; ++i) {
      int s;
      switch (align_) {
        case -1:
          s = index + i;
          break;
        case 0:
          s = index - half_width_ + i;
          break;
        case 1:
          s = index - i;
          break;
      }
      if (ISNAN(x_[s])) {
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
      switch (align_) {
        case -1:
          s = index + i;
          break;
        case 0:
          s = index - half_width_ + i;
          break;
        case 1:
          s = index - i;
          break;
      }
      mean += x_[s] * weights_[i];
    }
    mean /= width_;
    return mean;
  }

  // Window Median
  double windowMedian(const int &index) {
    Rcpp::NumericVector tmp(width_, NA_REAL);
    for (int i = 0; i < width_; ++i) {
      int s;
      switch (align_) {
      case -1:
        s = index + i;
        break;
      case 0:
        s = index - half_width_ + i;
        break;
      case 1:
        s = index - i;
        break;
      }
      if (ISNAN(x_[s])) {
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
      switch (align_) {
        case -1:
          s = index + i;
          break;
        case 0:
          s = index - half_width_ + i;
          break;
        case 1:
          s = index - i;
          break;
      }
      if (ISNAN(x_[s])) {
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
      switch (align_) {
        case -1:
          s = index + i;
          break;
        case 0:
          s = index - half_width_ + i;
          break;
        case 1:
          s = index - i;
          break;
      }
      prod *= x_[s] * weights_[i];
    }
    return prod;
  }

  // Window Sum
  double windowSum(const int &index) {
    double sum = 0;
    for (int i = 0; i < width_; ++i) {
      int s;
      switch (align_) {
        case -1:
          s = index + i;
          break;
        case 0:
          s = index - half_width_ + i;
          break;
        case 1:
          s = index - i;
          break;
      }
      sum += x_[s] * weights_[i];
    }
    return sum;
  }

  // Window Variance
  double windowVar(const int &index) {
    double var = 0;
    double mean = windowMean(index);
    for (int i = 0; i < width_; ++i) {
      int s;
        switch (align_) {
        case -1:
          s = index + i;
          break;
        case 0:
          s = index - half_width_ + i;
          break;
        case 1:
          s = index - i;
          break;
      }
      var += (x_[s] - mean) * (x_[s] - mean);
    }
    var /= width_ - 1;
    return var;
  }

};

/* ----- Rcpp Exported ----- */

//' @title Roll Hampel
//'
//' @description Apply a moving-window hampel value function to a numeric
//' vector.
//'
//' @details Each window of \code{n}-length is applied \code{weight} and then
//' slid/shifted/rolled \code{by} a positive integer amount about the window's
//' \code{align}-ment index.
//'
//' The \code{align} parameter determines the alignment of the current index
//' within the window. Thus:
//'
//' \itemize{
//'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
//'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
//'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
//' }
//'
//' @param x Numeric vector.
//' @param width Integer width of the rolling window.
//' @param weights A numeric vector of size \code{n} specifying each window
//' index weight. If \code{NULL}, the unit weight is used.
//' @param by An integer to shift the window by.
//' @param align A signed integer representing the windows alignment.
//' \code{-1(left)|0(center)|1(right)}.
//'
//' @return numeric vector of length(x)
//'
//' @examples
//' # load airquality
//' data("airquality")
//'
//' # calculate moving hampel value of next 3 measurements
//' roll_mean(airquality$Temp, width = 3, align = 1)
// [[Rcpp::export]]
Rcpp::NumericVector roll_hampel(
    Rcpp::NumericVector x,
    unsigned int width = 5,
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, width, weights, by, align);
  return roll.hampel();
}

//' @title Roll Max
//'
//' @description Apply a moving-window maximum function to a numeric vector.
//'
//' @details Each window of \code{n}-length is applied \code{weight} and then
//' slid/shifted/rolled \code{by} a positive integer amount about the window's
//' \code{align}-ment index.
//'
//' The \code{align} parameter determines the alignment of the current index
//' within the window. Thus:
//'
//' \itemize{
//'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
//'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
//'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
//' }
//'
//' @param x Numeric vector.
//' @param width Integer width of the rolling window.
//' @param weights \emph{Not used in \code{roll_meax()}}.
//' @param by Integer shift to use when sliding the window to the next location
//' @param align Signed integer representing the window alignment.
//' \code{-1(left)|0(center)|1(right)}.
//'
//' @return numeric vector of length(x)
//'
//' @examples
//' # Load package air quality data
//' data("airquality")
//'
//' # Calculate moving maximum of adjacent measurements
//' roll_mean(airquality$Temp, width = 3)
// [[Rcpp::export]]
Rcpp::NumericVector roll_max(
    Rcpp::NumericVector x,
    unsigned int width = 5,
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, width, weights, by, align);
  return roll.max();
}

//' @title Roll Mean
//'
//' @description Apply a moving-window mean function to a numeric vector.
//'
//' @details Each window of \code{n}-length is applied \code{weight} and then
//' slid/shifted/rolled \code{by} a positive integer amount about the window's
//' \code{align}-ment index.
//'
//' The \code{align} parameter determines the alignment of the current index
//' within the window. Thus:
//'
//' \itemize{
//'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
//'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
//'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
//' }
//'
//' @param x Numeric vector.
//' @param width Integer width of the rolling window.
//' @param weights A numeric vector of size \code{n} specifying each window
//' index weight. If \code{NULL}, the unit weight is used.
//' @param by An integer to shift the window by.
//' @param align A signed integer representing the windows alignment.
//' \code{-1(left)|0(center)|1(right)}.
//'
//' @return numeric vector of length(x)
//'
//' @examples
//' # load airquality
//' data("airquality")
//'
//' # calculate moving average of last 6 measurements
//' roll_mean(airquality$Temp, width = 6, align = -1)
// [[Rcpp::export]]
Rcpp::NumericVector roll_mean(
    Rcpp::NumericVector x,
    unsigned int width = 5,
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, width, weights, by, align);
  return roll.mean();
}

//' @title Roll Median
//'
//' @description Apply a moving-window median function to a numeric vector.
//'
//' @details Each window of \code{n}-length is applied \code{weights} and then
//' slid/shifted/rolled \code{by} a positive integer amount about the window's
//' \code{align}-ment index.
//'
//' The \code{align} parameter determines the alignment of the current index
//' within the window. Thus:
//'
//' \itemize{
//'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
//'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
//'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
//' }
//'
//' @param x Numeric vector.
//' @param width Integer width of the rolling window.
//' @param weights \emph{Not used in \code{roll_median()}}.
//' @param by An integer to shift the window by.
//' @param align A signed integer representing the windows alignment.
//' \code{-1(left)|0(center)|1(right)}.
//'
//' @return numeric vector of length(x)
//'
//' @examples
//' # load airquality
//' data("airquality")
//'
//' # calculate moving median of adjacent measurements
//' roll_mean(airquality$Temp, width = 3)
// [[Rcpp::export]]
Rcpp::NumericVector roll_median (
    Rcpp::NumericVector x,
    unsigned int width = 5,
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, width, weights, by, align);
  return roll.median();
}

//' @title Roll Min
//'
//' @description Apply a moving-window minimum function to a numeric vector.
//'
//' @details Each window of \code{n}-length is applied \code{weight} and then
//' slid/shifted/rolled \code{by} a positive integer amount about the window's
//' \code{align}-ment index.
//'
//' The \code{align} parameter determines the alignment of the current index
//' within the window. Thus:
//'
//' \itemize{
//'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
//'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
//'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
//' }
//'
//' @param x Numeric vector.
//' @param width Integer width of the rolling window.
//' @param weights \emph{Not used in \code{roll_median()}}.
//' @param by An integer to shift the window by.
//' @param align A signed integer representing the windows alignment.
//' \code{-1(left)|0(center)|1(right)}.
//'
//' @return numeric vector of length(x)
//'
//' @examples
//' # load airquality
//' data("airquality")
//'
//' # calculate moving minimum of last 24 measurements
//' roll_min(airquality$Temp, width = 24, align = -1)
// [[Rcpp::export]]
Rcpp::NumericVector roll_min(
    Rcpp::NumericVector x,
    unsigned int width = 5,
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, width, weights, by, align);
  return roll.min();
}

//' @title Roll Product
//'
//' @description Apply a moving-window product function to a numeric vector.
//'
//' @details Each window of \code{n}-length is applied \code{weight} and then
//' slid/shifted/rolled \code{by} a positive integer amount about the window's
//' \code{align}-ment index.
//'
//' The \code{align} parameter determines the alignment of the current index
//' within the window. Thus:
//'
//' \itemize{
//'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
//'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
//'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
//' }
//'
//' @param x Numeric vector.
//' @param width Integer width of the rolling window.
//' @param weights A numeric vector of size \code{n} specifying each window
//' index weight. If \code{NULL}, the unit weight is used.
//' @param by An integer to shift the window by.
//' @param align A signed integer representing the windows alignment.
//' \code{-1(left)|0(center)|1(right)}.
//'
//' @return numeric vector of length(x)
//'
//' @examples
//' # load airquality
//' data("airquality")
//'
//' # calculate moving product of 12 measurements
//' roll_prod(airquality$Temp, width = 12)
// [[Rcpp::export]]
Rcpp::NumericVector roll_prod(
    Rcpp::NumericVector x,
    unsigned int width = 5,
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, width, weights, by, align);
  return roll.prod();
}

//' @title Roll Standard Deviation
//'
//' @description Apply a moving-window standard deviation function to a
//' numeric vector.
//'
//' @details Each window of \code{n}-length is applied \code{weight} and then
//' slid/shifted/rolled \code{by} a positive integer amount about the window's
//' \code{align}-ment index.
//'
//' The \code{align} parameter determines the alignment of the current index
//' within the window. Thus:
//'
//' \itemize{
//'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
//'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
//'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
//' }
//'
//' @param x Numeric vector.
//' @param width Integer width of the rolling window.
//' @param weights \emph{Not used in \code{roll_sd()}}.
//' @param by An integer to shift the window by.
//' @param align A signed integer representing the windows alignment.
//' \code{-1(left)|0(center)|1(right)}.
//'
//' @return numeric vector of length(x)
//'
//' @examples
//' # load airquality
//' data("airquality")
//'
//' # calculate moving standard deviation of adjacent measurements
//' roll_mean(airquality$Temp, width = 3)
// [[Rcpp::export]]
Rcpp::NumericVector roll_sd(
    Rcpp::NumericVector x,
    unsigned int width = 5,
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, width, weights, by, align);
  return roll.sd();
}

//' @title Roll Sum
//'
//' @description Apply a moving-window sum to a numeric vector.
//'
//' @details Each window of \code{n}-length is applied \code{weight} and then
//' slid/shifted/rolled \code{by} a positive integer amount about the window's
//' \code{align}-ment index.
//'
//' The \code{align} parameter determines the alignment of the current index
//' within the window. Thus:
//'
//' \itemize{
//'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
//'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
//'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
//' }
//'
//' @param x Numeric vector.
//' @param width Integer width of the rolling window.
//' @param weights A numeric vector of size \code{n} specifying each window
//' index weight. If \code{NULL}, the unit weight is used.
//' @param by An integer to shift the window by.
//' @param align A signed integer representing the windows alignment.
//' \code{-1(left)|0(center)|1(right)}.
//'
//' @return numeric vector of length(x)
//'
//' @examples
//' # load airquality
//' data("airquality")
//'
//' # calculate moving sum of last 3 measurements
//' roll_sum(airquality$Temp, width = 3, align = -1)
// [[Rcpp::export]]
Rcpp::NumericVector roll_sum(
    Rcpp::NumericVector x,
    unsigned int width = 5,
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, width, weights, by, align);
  return roll.sum();
}

//' @title Roll Variance
//'
//' @description Apply a moving-window variance function to a numeric vector.
//'
//' @details Each window of \code{n}-length is applied \code{weight} and then
//' slid/shifted/rolled \code{by} a positive integer amount about the window's
//' \code{align}-ment index.
//'
//' The \code{align} parameter determines the alignment of the current index
//' within the window. Thus:
//'
//' \itemize{
//'   \item{\code{align = -1 [*------]} will cause the returned vector to have width-1 \code{NA} values at the right end.}
//'   \item{\code{align = 0  [---*---]} will cause the returned vector to have width/2 \code{NA} values at either end.}
//'   \item{\code{align = 1  [------*]} will cause the returned vector to have width-1 \code{NA} values at the left end.}
//' }
//'
//' @param x Numeric vector.
//' @param width Integer width of the rolling window.
//' @param weights \emph{Not used in \code{roll_var()}}.
//' @param by An integer to shift the window by.
//' @param align A signed integer representing the windows alignment.
//' \code{-1(left)|0(center)|1(right)}.
//'
//' @return numeric vector of length(x)
//'
//' @examples
//' # load airquality
//' data("airquality")
//'
//' # calculate moving variance of adjacent measurements
//' roll_mean(airquality$Temp, width = 3)
// [[Rcpp::export]]
Rcpp::NumericVector roll_var(
    Rcpp::NumericVector x,
    unsigned int width = 5,
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, width, weights, by, align);
  return roll.var();
}






