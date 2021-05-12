#include <Rcpp.h>
#include <stdlib.h>

/* ----- Roll Class ----- */

class Roll {

public:

  // Initialize Roller
  void init(
      Rcpp::NumericVector data,
      int windowSize,
      Rcpp::NumericVector weights,
      int increment,
      int alignment
  ) {
    X = data;
    nwin = windowSize;
    inc = increment;
    align = alignment;
    kwin = windowSize / 2;
    weight = weights;
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

// Roll Median
// [[Rcpp::export]]
Rcpp::NumericVector roll_median (
    Rcpp::NumericVector x,
    unsigned int n = 5,
    Rcpp::NumericVector weight = 1,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, n, weight, by, align);
  return roll.median();
}

// Roll Mean
// [[Rcpp::export]]
Rcpp::NumericVector roll_mean(
    Rcpp::NumericVector x,
    unsigned int n = 5,
    Rcpp::NumericVector weight = 1,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, n, weight, by, align);
  return roll.mean();
}

// Roll Variance
// [[Rcpp::export]]
Rcpp::NumericVector roll_var(
    Rcpp::NumericVector x,
    unsigned int n = 5,
    Rcpp::NumericVector weight = 1,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, n, weight, by, align);
  return roll.var();
}

// Roll Standard Deviation
// [[Rcpp::export]]
Rcpp::NumericVector roll_sd(
    Rcpp::NumericVector x,
    unsigned int n = 5,
    Rcpp::NumericVector weight = 1,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, n, weight, by, align);
  return roll.sd();
}

// Roll Hampel
// [[Rcpp::export]]
Rcpp::NumericVector roll_hampel(
    Rcpp::NumericVector x,
    unsigned int n = 5,
    Rcpp::NumericVector weight = 1,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, n, weight, by, align);
  return roll.hampel();
}

// Roll Max
// [[Rcpp::export]]
Rcpp::NumericVector roll_max(
    Rcpp::NumericVector x,
    unsigned int n = 5,
    Rcpp::NumericVector weight = 1,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, n, weight, by, align);
  return roll.max();
}

// Roll Min
// [[Rcpp::export]]
Rcpp::NumericVector roll_min(
    Rcpp::NumericVector x,
    unsigned int n = 5,
    Rcpp::NumericVector weight = 1,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, n, weight, by, align);
  return roll.min();
}

// Roll Sum
// [[Rcpp::export]]
Rcpp::NumericVector roll_sum(
    Rcpp::NumericVector x,
    unsigned int n = 5,
    Rcpp::NumericVector weight = 1,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, n, weight, by, align);
  return roll.sum();
}

// Roll Product
// [[Rcpp::export]]
Rcpp::NumericVector roll_prod(
    Rcpp::NumericVector x,
    unsigned int n = 5,
    Rcpp::NumericVector weight = 1,
    int by = 1,
    int align = 0
) {
  Roll roll;
  roll.init(x, n, weight, by, align);
  return roll.prod();
}

// PM2.5 NowCast
// Rcpp::NumericVector nowcast(Rcpp::NumericVector x) {
//
//   Roll roll;
//
//   Rcpp::NumericVector min, max;
//
//   Rcpp::NumericVector tmp;
//
//   roll.init(x, 12, tmp, 1, 0);
//
//   min = roll.min();
//   max = roll.max();
//
//
//   return -1;
// }






