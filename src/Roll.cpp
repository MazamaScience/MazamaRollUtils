
#include <Rcpp.h>
#include <stdlib.h>
#include <cmath>

using namespace Rcpp;

/* ----- Roll Class ----- */

class Roll {

public:

  // Initialize Roller
  void init(NumericVector data, int windowSize, int alignment) {
    X = data;
    win = windowSize;
    align = alignment;
    halfWin = windowSize / 2;
  }

  // Roll Mean
  NumericVector mean() {
    size_t len = X.size();
    NumericVector out(len, NA_REAL);
    int start = 0;
    int end = len;
    switch (align) {
      case -1:
        start = 0;
        end = len - (win - 1);
        break;
      case 0:
        start = halfWin;
        end = len - halfWin;
        break;
      case 1:
        start = win - 1;
        end = len;
        break;
    }
    for (int ind = start; ind < end; ++ind) {
      out[ind] = windowMean(ind);
    }
    return out;
  }

  // Roll Median
  NumericVector median() {
    int len = X.size();
    NumericVector out(len, NA_REAL);
    for (int ind = halfWin; ind < len - halfWin; ++ind) {
      out[ind] = windowMedian(ind);
    }
    return out;
  }

  // Roll Variance
  NumericVector var() {
    int len = X.size();
    NumericVector out(len, NA_REAL);
    int start = 0;
    int end = len;
    switch (align) {
      case -1:
        start = 0;
        end = len - (win - 1);
        break;
      case 0:
        start = halfWin;
        end = len - halfWin;
        break;
      case 1:
        start = win - 1;
        end = len;
        break;
    }
    for (int i = start; i < end; ++i) {
      out[i] = windowVar(i);
    }
    return out;
  }

  // Roll Standard Deviation
  NumericVector sd() {
    int len = X.size();
    NumericVector out(len, NA_REAL);
    int start = 0;
    int end = len;
    switch (align) {
      case -1:
        start = 0;
        end = len - (win - 1);
        break;
      case 0:
        start = halfWin;
        end = len - halfWin;
        break;
      case 1:
        start = win - 1;
        end = len;
        break;
    }
    for (int i = start; i < end; ++i) {
      out[i] = sqrt(windowVar(i));
    }
    return out;
  }

  // Roll Hampel
  NumericVector hampel() {
    int len = X.size();
    NumericVector out(len, NA_REAL);
    // For the valid region, calculate the result
    for( int i = halfWin; i < len - halfWin; ++i ) {
      out[i] = windowHampel(i);
    }
    return out;
  }

private:

  NumericVector X; // Input Data
  int align; // Alignment
  int win; // Window Size
  unsigned long halfWin; // Half-window Size

  // Window Mean
  double windowMean(const int &index) {
    double mean = 0;
    for (size_t i = 0; i < win; ++i) {
      int s;
      switch (align) {
        case -1:
          s = index + i;
          break;
        case 0:
          s = index - halfWin + i;
          break;
        case 1:
          s = index - i;
          break;
      }
      mean += X[s];
    }
    mean /= win;
    return mean;
  }

  // Window Median
  double windowMedian(const int &index) {
    NumericVector tmp(win, NA_REAL);
    for (int i = 0; i < win; i++) {
      tmp[i] = X[index - halfWin + i];
    }
    std::sort(tmp.begin(), tmp.end());
    return win % 2 ? tmp[halfWin]:(tmp[halfWin - 1] + tmp[halfWin]) / 2;
  }

  // Window Variance
  double windowVar(const int &index) {
    double var = 0;
    double mean = windowMean(index);
    for (int i = 0; i < win; ++i) {
      int s;
      switch (align) {
        case -1:
          s = index + i;
          break;
        case 0:
          s = index - halfWin + i;
          break;
        case 1:
          s = index - i;
          break;
      }
      var += (X[s] - mean) * (X[s] - mean);
    }
    var /= win - 1;
    return var;
  }

  // Window Hampel
  double windowHampel(const int &index) {
    const double kappa = 1.4826;
    double median_i = windowMedian(index);
    NumericVector tmp(win, NA_REAL);
    for (int i = 0; i < win; ++i) { // MAD
      size_t s = index - halfWin + i;
      tmp[i] = std::fabs(X[s] - median_i);
    }
    std::sort(tmp.begin(), tmp.end());
    double mad =  win % 2 ? tmp[halfWin]:(tmp[halfWin - 1] + tmp[halfWin]) / 2;
    return std::fabs(X[index] - median_i) / (kappa * mad);
  }

};

/* ----- Rcpp Exported ----- */

// Roll Median
// [[Rcpp::export]]
NumericVector roll_median (NumericVector x, unsigned int windowSize = 5, int align = 0) {
  Roll roll;
  roll.init(x, windowSize, align);
  return roll.median();
}

// Roll Mean
// [[Rcpp::export]]
NumericVector roll_mean (NumericVector x, unsigned int windowSize = 5, int align = 0) {
  Roll roll;
  roll.init(x, windowSize, align);
  return roll.mean();
}

// Roll Variance
// [[Rcpp::export]]
NumericVector roll_var (NumericVector x, unsigned int windowSize = 5, int align = 0) {
  Roll roll;
  roll.init(x, windowSize, align);
  return roll.var();
}

// Roll Standard Deviation
// [[Rcpp::export]]
NumericVector roll_sd (NumericVector x, unsigned int windowSize = 5, int align = 0) {
  Roll roll;
  roll.init(x, windowSize, align);
  return roll.sd();
}

// Roll Hampel
// [[Rcpp::export]]
NumericVector roll_hampel (NumericVector x, unsigned int windowSize = 5, double threshold = 1, int align = 0) {
  Roll roll;
  roll.init(x, windowSize, align);
  return roll.hampel();
}




