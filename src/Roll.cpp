#include <Rcpp.h>
#include <stdlib.h>

/* ----- Roll Class ----- */

class Roll {

public:

  // Initialize Roller
  void init(
      Rcpp::NumericVector data,
      int windowSize,
      Rcpp::Nullable<Rcpp::NumericVector> weights,
      int increment,
      int alignment
  ) {

    // Checks
    if (windowSize > data.size()) {
      Rcpp::stop("\nWindow 'n' cannot be larger than 'x'");
    }
    if (increment > data.size()) {
      Rcpp::stop("\nIncrement 'by' cannot be larger than 'x'\n");
    }
    if (pow(alignment, 2) > 1) {
      Rcpp::stop("\nWindow alignment must be either -1 (left), 0 (center), or +1 (right)");
    }
    if (increment > windowSize) {
      Rcpp::warning("\nWarning: Increment 'by' is larger than the window 'n'\n");
    }

    // handle defaults
    if (weights.isNull()) {
      weight = Rcpp::rep(1, windowSize);
    } else {
      weight = weights;
    }

    // init private vars
    X = data;
    nwin = windowSize;
    inc = increment;
    align = alignment;
    kwin = windowSize / 2;

  }

  // Roll Mean
  Rcpp::NumericVector mean() {
    size_t len = X.size();
    Rcpp::NumericVector out(len, NA_REAL);
    int start = 0;
    int end = len;
    switch (align) {
      case -1:
        start = 0;
        end = len - (nwin - 1);
        break;
      case 0:
        start = kwin;
        end = len - kwin;
        break;
      case 1:
        start = nwin - 1;
        end = len;
        break;
    }
    for (int i = start; i < end; i += inc) {
      out[i] = windowMean(i);
    }
    return out;
  }

  // Roll Median
  Rcpp::NumericVector median() {
    int len = X.size();
    Rcpp::NumericVector out(len, NA_REAL);
    for (int i = kwin; i < len - kwin; i += inc) {
      out[i] = windowMedian(i);
    }
    return out;
  }

  // Roll Variance
  Rcpp::NumericVector var() {
    int len = X.size();
    Rcpp::NumericVector out(len, NA_REAL);
    int start = 0;
    int end = len;
    switch (align) {
      case -1:
        start = 0;
        end = len - (nwin - 1);
        break;
      case 0:
        start = kwin;
        end = len - kwin;
        break;
      case 1:
        start = nwin - 1;
        end = len;
        break;
    }
    for (int i = start; i < end; i += inc) {
      out[i] = windowVar(i);
    }
    return out;
  }

  // Roll Standard Deviation
  Rcpp::NumericVector sd() {
    int len = X.size();
    Rcpp::NumericVector out(len, NA_REAL);
    int start = 0;
    int end = len;
    switch (align) {
      case -1:
        start = 0;
        end = len - (nwin - 1);
        break;
      case 0:
        start = kwin;
        end = len - kwin;
        break;
      case 1:
        start = nwin - 1;
        end = len;
        break;
    }
    for (int i = start; i < end; i += inc) {
      out[i] = sqrt(windowVar(i));
    }
    return out;
  }

  // Roll Hampel
  Rcpp::NumericVector hampel() {
    int len = X.size();
    Rcpp::NumericVector out(len, NA_REAL);
    for (int i = kwin; i < len - kwin; i += inc) {
      out[i] = windowHampel(i);
    }
    return out;
  }

  // Roll Maxima
  Rcpp::NumericVector max() {
    int len = X.size();
    Rcpp::NumericVector out(len, NA_REAL);
    for (int i = kwin; i < len - kwin; i += inc) {
      out[i] = windowMax(i);
    }
    return out;
  }

  // Roll Minima
  Rcpp::NumericVector min() {
    int len = X.size();
    Rcpp::NumericVector out(len, NA_REAL);
    for (int i = kwin; i < len - kwin; i += inc) {
      out[i] = windowMin(i);
    }
    return out;
  }

  Rcpp::NumericVector sum() {
    int len = X.size();
    Rcpp::NumericVector out(len, NA_REAL);
    for (int i = kwin; i < len - kwin; i += inc) {
      out[i] = windowSum(i);
    }
    return out;
  }

  Rcpp::NumericVector prod() {
    int len = X.size();
    Rcpp::NumericVector out(len, NA_REAL);
    for (int i = kwin; i < len - kwin; i += inc) {
      out[i] = windowProd(i);
    }
    return out;
  }

private:

  Rcpp::NumericVector X; // Input Data
  int align; // Alignment
  int nwin; // Window Size
  int kwin; // Half-window Size
  int inc; // Increment by
  Rcpp::NumericVector weight;

  // Window Mean
  double windowMean(const int &index) {
    double mean = 0;
    for (size_t i = 0; i < nwin; ++i) {
      int s;
      switch (align) {
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
      mean += X[s] * weight[i];
    }
    mean /= nwin;
    return mean;
  }

  // Window Median
  double windowMedian(const int &index) {
    Rcpp::NumericVector tmp(nwin, NA_REAL);
    for (int i = 0; i < nwin; ++i) {
      tmp[i] = X[index - kwin + i] * weight[i];
    }
    std::nth_element(tmp.begin(), tmp.begin() + kwin, tmp.end());
    return tmp[kwin];
  }

  // Window Variance
  double windowVar(const int &index) {
    double var = 0;
    double mean = windowMean(index);
    for (int i = 0; i < nwin; ++i) {
      int s;
      switch (align) {
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
      var += (X[s] - mean) * (X[s] - mean) * weight[i];
    }
    var /= nwin - 1;
    return var;
  }

  // Window Hampel
  double windowHampel(const int &index) {
    const double kappa = 1.4826;
    double median_i = windowMedian(index);
    Rcpp::NumericVector tmp(nwin, NA_REAL);
    for (int i = 0; i < nwin; ++i) { // MAD
      size_t s = index - kwin + i;
      tmp[i] = std::fabs(X[s] - median_i) * weight[i];
    }
    std::nth_element(tmp.begin(), tmp.begin() + kwin, tmp.end());
    double mad = tmp[kwin];
    return std::fabs(X[index] - median_i) / (kappa * mad);
  }

  // Window Maxima
  double windowMax(const int &index) {
    double max = X[index];
    for (int i = 0; i < nwin; ++i) {
      int s;
      switch (align) {
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
      if (X[s] * weight[i] > max) max = X[s] * weight[i];
    }
    return max;
  }

  // Window Minima
  double windowMin(const int &index) {
    double min = X[index];
    for (int i = 0; i < nwin; ++i) {
      int s;
      switch (align) {
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
      if (X[s] < min) min = X[s];
    }
    return min;
  }

  double windowSum(const int &index) {
    double sum = 0;
    for (size_t i = 0; i < nwin; ++i) {
      int s;
      switch (align) {
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
      sum += X[s] * weight[i];
    }

    return sum;
  }

  double windowProd(const int &index) {
    double prod = 1;
    for (size_t i = 0; i < nwin; ++i) {
      int s;
      switch (align) {
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
      prod *= X[s] * weight[i];
    }

    return prod;
  }

};

/* ----- Rcpp Exported ----- */

//' @title Roll Median
//'
//' @description
//'
//' @param x A numeric vector.
//' @param n An integer window length.
//' @param weights A numeric vector of n-length specifying each \code{n}th
//' weight. If `NULL`, unit weights are used.
//' @param by An integer to shift the window by.
//' @param align A signed integer representing the windows alignment.
// '-1 (left) | 0 (center) | 1 (right).
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

//' Roll Mean
//'
//' @param x A numeric vector.
//' @param n An integer window length.
//' @param weights A numeric vector of n-length specifying each \code{n}th
//' weight. If `NULL`, unit weights are used.
//' @param by An integer to shift the window by.
//' @param align A signed integer representing the windows alignment.
// '-1 (left) | 0 (center) | 1 (right).
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

//' Roll Variance
//'
//' @param x A numeric vector.
//' @param n An integer window length.
//' @param weights A numeric vector of n-length specifying each \code{n}th
//' weight. If `NULL`, unit weights are used.
//' @param by An integer to shift the window by.
//' @param align A signed integer representing the windows alignment.
// '-1 (left) | 0 (center) | 1 (right).
//'
//' @return numeric vector of length(x)
//'
//' @examples
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

//' Roll Standard Deviation
//'
//' @param x A numeric vector.
//' @param n An integer window length.
//' @param weights A numeric vector of n-length specifying each \code{n}th
//' weight. If `NULL`, unit weights are used.
//' @param by An integer to shift the window by.
//' @param align A signed integer representing the windows alignment.
// '-1 (left) | 0 (center) | 1 (right).
//'
//' @return numeric vector of length(x)
//'
//' @examples
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

//' Roll Hampel
//'
//' @param x A numeric vector.
//' @param n An integer window length.
//' @param weights A numeric vector of n-length specifying each \code{n}th
//' weight. If `NULL`, unit weights are used.
//' @param by An integer to shift the window by.
//' @param align A signed integer representing the windows alignment.
// '-1 (left) | 0 (center) | 1 (right).
//'
//' @return numeric vector of length(x)
//'
//' @examples
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

//' Roll Max
//'
//' @param x A numeric vector.
//' @param n An integer window length.
//' @param weights A numeric vector of n-length specifying each \code{n}th
//' weight. If `NULL`, unit weights are used.
//' @param by An integer to shift the window by.
//' @param align A signed integer representing the windows alignment.
// '-1 (left) | 0 (center) | 1 (right).
//'
//' @return numeric vector of length(x)
//'
//' @examples
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

//' Roll Min
//'
//' @param x A numeric vector.
//' @param n An integer window length.
//' @param weights A numeric vector of n-length specifying each \code{n}th
//' weight. If `NULL`, unit weights are used.
//' @param by An integer to shift the window by.
//' @param align A signed integer representing the windows alignment.
// '-1 (left) | 0 (center) | 1 (right).
//'
//' @return numeric vector of length(x)
//'
//' @examples
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

//' Roll Sum
//'
//' @param x A numeric vector.
//' @param n An integer window length.
//' @param weights A numeric vector of n-length specifying each \code{n}th
//' weight. If `NULL`, unit weights are used.
//' @param by An integer to shift the window by.
//' @param align A signed integer representing the windows alignment.
// '-1 (left) | 0 (center) | 1 (right).
//'
//' @return numeric vector of length(x)
//'
//' @examples
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

//' Roll Product
//'
//' @param x A numeric vector.
//' @param n An integer window length.
//' @param weights A numeric vector of n-length specifying each \code{n}th
//' weight. If `NULL`, unit weights are used.
//' @param by An integer to shift the window by.
//' @param align A signed integer representing the windows alignment.
// '-1 (left) | 0 (center) | 1 (right).
//'
//' @return numeric vector of length(x)
//'
//' @examples
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






