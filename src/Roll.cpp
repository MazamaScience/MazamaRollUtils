#include <Rcpp.h>
#include <stdlib.h>

/* ----- Roll Class ----- */

class Roll {

public:

  // Initialize Roller
  void init(Rcpp::NumericVector data, int windowSize, int increment, int alignment) {
    X = data;
    nwin = windowSize;
    align = alignment;
    cenwin = windowSize / 2;
    inc = increment;
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
        start = cenwin;
        end = len - cenwin;
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
    for (int i = cenwin; i < len - cenwin; i += inc) {
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
        start = cenwin;
        end = len - cenwin;
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
        start = cenwin;
        end = len - cenwin;
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
    // For the valid region, calculate the result
    for (int i = cenwin; i < len - cenwin; i += inc) {
      out[i] = windowHampel(i);
    }
    return out;
  }

  // Roll Maxima
  Rcpp::NumericVector max() {
    int len = X.size();
    Rcpp::NumericVector out(len, NA_REAL);
    // For the valid region, calculate the result
    for (int i = cenwin; i < len - cenwin; i += inc) {
      out[i] = windowMax(i);
    }
    return out;
  }

  // Roll Minima
  Rcpp::NumericVector min() {
    int len = X.size();
    Rcpp::NumericVector out(len, NA_REAL);
    // For the valid region, calculate the result
    for (int i = cenwin; i < len - cenwin; i += inc) {
      out[i] = windowMin(i);
    }
    return out;
  }

private:

  Rcpp::NumericVector X; // Input Data
  int align; // Alignment
  int nwin; // Window Size
  unsigned long cenwin; // Half-window Size
  int inc; // Increment by

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
          s = index - cenwin + i;
          break;
        case 1:
          s = index - i;
          break;
      }
      mean += X[s];
    }
    mean /= nwin;
    return mean;
  }

  // Window Median
  double windowMedian(const int &index) {
    Rcpp::NumericVector tmp(nwin, NA_REAL);
    for (int i = 0; i < nwin; ++i) {
      tmp[i] = X[index - cenwin + i];
    }
    std::nth_element(tmp.begin(), tmp.begin() + cenwin, tmp.end());
    return tmp[cenwin];
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
          s = index - cenwin + i;
          break;
        case 1:
          s = index - i;
          break;
      }
      var += (X[s] - mean) * (X[s] - mean);
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
      size_t s = index - cenwin + i;
      tmp[i] = std::fabs(X[s] - median_i);
    }
    std::nth_element(tmp.begin(), tmp.begin() + cenwin, tmp.end());
    double mad = tmp[cenwin];
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
        s = index - cenwin + i;
        break;
      case 1:
        s = index - i;
        break;
      }
      if (X[s] > max) max = X[s];
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
        s = index - cenwin + i;
        break;
      case 1:
        s = index - i;
        break;
      }
      if (X[s] < min) min = X[s];
    }
    return min;
  }
};

/* ----- Rcpp Exported ----- */

// Roll Median
// [[Rcpp::export]]
Rcpp::NumericVector roll_median (Rcpp::NumericVector x, unsigned int n = 5, int by = 1, int align = 0) {
  Roll roll;
  roll.init(x, n, by, align);
  return roll.median();
}

// Roll Mean
// [[Rcpp::export]]
Rcpp::NumericVector roll_mean (Rcpp::NumericVector x, unsigned int n = 5, int by = 1, int align = 0) {
  Roll roll;
  roll.init(x, n, by, align);
  return roll.mean();
}

// Roll Variance
// [[Rcpp::export]]
Rcpp::NumericVector roll_var (Rcpp::NumericVector x, unsigned int n = 5, int by = 1, int align = 0) {
  Roll roll;
  roll.init(x, n, by, align);
  return roll.var();
}

// Roll Standard Deviation
// [[Rcpp::export]]
Rcpp::NumericVector roll_sd (Rcpp::NumericVector x, unsigned int n = 5, int by = 1, int align = 0) {
  Roll roll;
  roll.init(x, n, by, align);
  return roll.sd();
}

// Roll Hampel
// [[Rcpp::export]]
Rcpp::NumericVector roll_hampel (Rcpp::NumericVector x, unsigned int n = 5, int by = 1, int align = 0) {
  Roll roll;
  roll.init(x, n, by, align);
  return roll.hampel();
}

// Roll Max
// [[Rcpp::export]]
Rcpp::NumericVector roll_max (Rcpp::NumericVector x, unsigned int n = 5, int by = 1, int align = 0) {
  Roll roll;
  roll.init(x, n, by, align);
  return roll.max();
}

// Roll Min
// [[Rcpp::export]]
Rcpp::NumericVector roll_min (Rcpp::NumericVector x, unsigned int n = 5, int by = 1, int align = 0) {
  Roll roll;
  roll.init(x, n, by, align);
  return roll.min();
}





