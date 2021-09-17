#include <Rcpp.h>
#include <stdlib.h>

/* ----- Roll Class ----- */

class Roll {

public:

  // Initialize Roller
  void init(
      Rcpp::NumericVector x,
      int n,
      Rcpp::Nullable<Rcpp::NumericVector> weights,
      int by,
      int align
  ) {

    // Checks
    if (n > x.size()) {
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
      weights_ = Rcpp::rep(1, n);
    } else {
      // See:  https://stackoverflow.com/questions/43388698/rcpp-how-can-i-get-the-size-of-a-rcppnullable-numericvector
      if (weights.isNotNull()) {
        Rcpp::NumericVector w(weights.get());
        if (w.size() != n) {
          Rcpp::stop("'weights' must be either NULL or a vector of the same length as window size 'n'");
        }
      }
      weights_ = weights;
    }

    // init private vars
    x_ = x;
    n_ = n;
    by_ = by;
    align_ = align;
    kwin = n / 2;

  }

  // Roll Mean
  Rcpp::NumericVector mean() {
    size_t len = x_.size();
    Rcpp::NumericVector out(len, NA_REAL);
    int start = 0;
    int end = len;
    switch (align_) {
      case -1:
        start = 0;
        end = len - (n_ - 1);
        break;
      case 0:
        start = kwin;
        end = len - kwin;
        break;
      case 1:
        start = n_ - 1;
        end = len;
        break;
    }
    for (int i = start; i < end; i += by_) {
      out[i] = windowMean(i);
    }
    return out;
  }

  // Roll Median
  Rcpp::NumericVector median() {
    int len = x_.size();
    Rcpp::NumericVector out(len, NA_REAL);
    for (int i = kwin; i < len - kwin; i += by_) {
      out[i] = windowMedian(i);
    }
    return out;
  }

  // Roll Variance
  Rcpp::NumericVector var() {
    int len = x_.size();
    Rcpp::NumericVector out(len, NA_REAL);
    int start = 0;
    int end = len;
    switch (align_) {
      case -1:
        start = 0;
        end = len - (n_ - 1);
        break;
      case 0:
        start = kwin;
        end = len - kwin;
        break;
      case 1:
        start = n_ - 1;
        end = len;
        break;
    }
    for (int i = start; i < end; i += by_) {
      out[i] = windowVar(i);
    }
    return out;
  }

  // Roll Standard Deviation
  Rcpp::NumericVector sd() {
    int len = x_.size();
    Rcpp::NumericVector out(len, NA_REAL);
    int start = 0;
    int end = len;
    switch (align_) {
      case -1:
        start = 0;
        end = len - (n_ - 1);
        break;
      case 0:
        start = kwin;
        end = len - kwin;
        break;
      case 1:
        start = n_ - 1;
        end = len;
        break;
    }
    for (int i = start; i < end; i += by_) {
      out[i] = sqrt(windowVar(i));
    }
    return out;
  }

  // Roll Hampel
  Rcpp::NumericVector hampel() {
    int len = x_.size();
    Rcpp::NumericVector out(len, NA_REAL);
    for (int i = kwin; i < len - kwin; i += by_) {
      out[i] = windowHampel(i);
    }
    return out;
  }

  // Roll Maxima
  Rcpp::NumericVector max() {
    int len = x_.size();
    Rcpp::NumericVector out(len, NA_REAL);
    for (int i = kwin; i < len - kwin; i += by_) {
      out[i] = windowMax(i);
    }
    return out;
  }

  // Roll Minima
  Rcpp::NumericVector min() {
    int len = x_.size();
    Rcpp::NumericVector out(len, NA_REAL);
    for (int i = kwin; i < len - kwin; i += by_) {
      out[i] = windowMin(i);
    }
    return out;
  }

  Rcpp::NumericVector sum() {
    int len = x_.size();
    Rcpp::NumericVector out(len, NA_REAL);
    for (int i = kwin; i < len - kwin; i += by_) {
      out[i] = windowSum(i);
    }
    return out;
  }

  Rcpp::NumericVector prod() {
    int len = x_.size();
    Rcpp::NumericVector out(len, NA_REAL);
    for (int i = kwin; i < len - kwin; i += by_) {
      out[i] = windowProd(i);
    }
    return out;
  }

private:

  Rcpp::NumericVector x_; // Input Data
  int align_; // Alignment
  int n_; // Window Size
  int kwin; // Half-window Size
  int by_; // Increment by
  Rcpp::NumericVector weights_;

  // Window Mean
  double windowMean(const int &index) {
    double mean = 0;
    for (size_t i = 0; i < n_; ++i) {
      int s;
      switch (align_) {
        case -1:
          s = index + i;
          break;
        case 0:
          s = index - kwin + i;
          break;
        case 1:
          s = index - i;
          break;
      }
      mean += x_[s] * weights_[i];
    }
    mean /= n_;
    return mean;
  }

  // Window Median
  double windowMedian(const int &index) {
    Rcpp::NumericVector tmp(n_, NA_REAL);
    for (int i = 0; i < n_; ++i) {
      tmp[i] = x_[index - kwin + i] * weights_[i];
    }
    std::nth_element(tmp.begin(), tmp.begin() + kwin, tmp.end());
    return tmp[kwin];
  }

  // Window Variance
  double windowVar(const int &index) {
    double var = 0;
    double mean = windowMean(index);
    for (int i = 0; i < n_; ++i) {
      int s;
      switch (align_) {
        case -1:
          s = index + i;
          break;
        case 0:
          s = index - kwin + i;
          break;
        case 1:
          s = index - i;
          break;
      }
      var += (x_[s] - mean) * (x_[s] - mean) * weights_[i];
    }
    var /= n_ - 1;
    return var;
  }

  // Window Hampel
  double windowHampel(const int &index) {
    const double kappa = 1.4826;
    double median_i = windowMedian(index);
    Rcpp::NumericVector tmp(n_, NA_REAL);
    for (int i = 0; i < n_; ++i) { // MAD
      size_t s = index - kwin + i;
      tmp[i] = std::fabs(x_[s] - median_i) * weights_[i];
    }
    std::nth_element(tmp.begin(), tmp.begin() + kwin, tmp.end());
    double mad = tmp[kwin];
    return std::fabs(x_[index] - median_i) / (kappa * mad);
  }

  // Window Maxima
  double windowMax(const int &index) {
    double max = x_[index];
    for (int i = 0; i < n_; ++i) {
      int s;
      switch (align_) {
      case -1:
        s = index + i;
        break;
      case 0:
        s = index - kwin + i;
        break;
      case 1:
        s = index - i;
        break;
      }
      if (x_[s] * weights_[i] > max) max = x_[s] * weights_[i];
    }
    return max;
  }

  // Window Minima
  double windowMin(const int &index) {
    double min = x_[index];
    for (int i = 0; i < n_; ++i) {
      int s;
      switch (align_) {
      case -1:
        s = index + i;
        break;
      case 0:
        s = index - kwin + i;
        break;
      case 1:
        s = index - i;
        break;
      }
      if (x_[s] < min) min = x_[s];
    }
    return min;
  }

  double windowSum(const int &index) {
    double sum = 0;
    for (size_t i = 0; i < n_; ++i) {
      int s;
      switch (align_) {
      case -1:
        s = index + i;
        break;
      case 0:
        s = index - kwin + i;
        break;
      case 1:
        s = index - i;
        break;
      }
      sum += x_[s] * weights_[i];
    }

    return sum;
  }

  double windowProd(const int &index) {
    double prod = 1;
    for (size_t i = 0; i < n_; ++i) {
      int s;
      switch (align_) {
      case -1:
        s = index + i;
        break;
      case 0:
        s = index - kwin + i;
        break;
      case 1:
        s = index - i;
        break;
      }
      prod *= x_[s] * weights_[i];
    }

    return prod;
  }

};

/* ----- Rcpp Exported ----- */

//' @title Roll Median
//'
//' @description Apply a moving-window median function to a numeric vector.
//'
//' @details Each window of \code{n}-length is applied \code{weights} and then
//' slid/shifted/rolled \code{by} a positive integer amount about the window's
//' \code{align}-ment index.
//'
//' @param x A numeric vector.
//' @param n An integer window length.
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
//' # calculate moving median of adjacent measurements
//' roll_mean(airquality$Temp, n = 3)
// [[Rcpp::export]]
Rcpp::NumericVector roll_median (
    Rcpp::NumericVector x,
    unsigned int n = 5,
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, n, weights, by, align);
  return roll.median();
}

//' @title Roll Mean
//'
//' @description Apply a moving-window mean function to a numeric vector.
//'
//' @details Each window of \code{n}-length is applied \code{weight} and then
//' slid/shifted/rolled \code{by} a positive integer amount about the window's
//' \code{align}-ment index.
//'
//' @param x A numeric vector.
//' @param n An integer window length.
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
//' roll_mean(airquality$Temp, n = 6, align = -1)
// [[Rcpp::export]]
Rcpp::NumericVector roll_mean(
    Rcpp::NumericVector x,
    unsigned int n = 5,
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, n, weights, by, align);
  return roll.mean();
}

//' @title Roll Variance
//'
//' @description Apply a moving-window variance function to a numeric vector.
//'
//' @details Each window of \code{n}-length is applied \code{weight} and then
//' slid/shifted/rolled \code{by} a positive integer amount about the window's
//' \code{align}-ment index.
//'
//' @param x A numeric vector.
//' @param n An integer window length.
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
//' # calculate moving variance of adjacent measurements
//' roll_mean(airquality$Temp, n = 3)
// [[Rcpp::export]]
Rcpp::NumericVector roll_var(
    Rcpp::NumericVector x,
    unsigned int n = 5,
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, n, weights, by, align);
  return roll.var();
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
//' @param x A numeric vector.
//' @param n An integer window length.
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
//' # calculate moving standard deviation of adjacent measurements
//' roll_mean(airquality$Temp, n = 3)
// [[Rcpp::export]]
Rcpp::NumericVector roll_sd(
    Rcpp::NumericVector x,
    unsigned int n = 5,
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, n, weights, by, align);
  return roll.sd();
}

//' @title Roll Hampel
//'
//' @description Apply a moving-window hampel value function to a numeric
//' vector.
//'
//' @details Each window of \code{n}-length is applied \code{weight} and then
//' slid/shifted/rolled \code{by} a positive integer amount about the window's
//' \code{align}-ment index.
//'
//' @param x A numeric vector.
//' @param n An integer window length.
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
//' roll_mean(airquality$Temp, n = 3, align = 1)
// [[Rcpp::export]]
Rcpp::NumericVector roll_hampel(
    Rcpp::NumericVector x,
    unsigned int n = 5,
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, n, weights, by, align);
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
//'   \item{\code{align = -1 [*------]} will cause the returned vector to have n-1 \code{NA} values at the right end.}
//'   \item{\code{align = 0 [---*---]} will cause the returned vector to have (n-1)/2 \code{NA} values at either end.}
//'   \item{\code{align = 1 [------*]} will cause the returned vector to have n-1 \code{NA} values at the left end.}
//' }
//'
//' @param x Numeric vector.
//' @param n Integer window size.
//' @param weights Numeric vector of length \code{n} specifying each window
//' index weight. If \code{NULL}, the unit weight is used.
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
//' roll_mean(airquality$Temp, n = 3)
// [[Rcpp::export]]
Rcpp::NumericVector roll_max(
    Rcpp::NumericVector x,
    unsigned int n = 5,
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, n, weights, by, align);
  return roll.max();
}

//' @title Roll Min
//'
//' @description Apply a moving-window minimum function to a numeric vector.
//'
//' @details Each window of \code{n}-length is applied \code{weight} and then
//' slid/shifted/rolled \code{by} a positive integer amount about the window's
//' \code{align}-ment index.
//'
//' @param x A numeric vector.
//' @param n An integer window length.
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
//' # calculate moving minimum of last 24 measurements
//' roll_min(airquality$Temp, n = 24, align = -1)
// [[Rcpp::export]]
Rcpp::NumericVector roll_min(
    Rcpp::NumericVector x,
    unsigned int n = 5,
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, n, weights, by, align);
  return roll.min();
}

//' @title Roll Sum
//'
//' @description Apply a moving-window sum to a numeric vector.
//'
//' @details Each window of \code{n}-length is applied \code{weight} and then
//' slid/shifted/rolled \code{by} a positive integer amount about the window's
//' \code{align}-ment index.
//'
//' @param x A numeric vector.
//' @param n An integer window length.
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
//' roll_sum(airquality$Temp, n = 3, align = -1)
// [[Rcpp::export]]
Rcpp::NumericVector roll_sum(
    Rcpp::NumericVector x,
    unsigned int n = 5,
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, n, weights, by, align);
  return roll.sum();
}

//' @title Roll Product
//'
//' @description Apply a moving-window product function to a numeric vector.
//'
//' @details Each window of \code{n}-length is applied \code{weight} and then
//' slid/shifted/rolled \code{by} a positive integer amount about the window's
//' \code{align}-ment index.
//'
//' @param x A numeric vector.
//' @param n An integer window length.
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
//' roll_prod(airquality$Temp, n = 12)
// [[Rcpp::export]]
Rcpp::NumericVector roll_prod(
    Rcpp::NumericVector x,
    unsigned int n = 5,
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, n, weights, by, align);
  return roll.prod();
}






