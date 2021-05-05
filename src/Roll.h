// Roll.h
#ifndef Roll_h
#define Roll_h

#include <Rcpp.h>
#include <stdlib.h>
#include <cmath>


using namespace Rcpp;

class Roll {

public:

  NumericVector X;

  int align;
  int N;
  int K;

  void initRoll(NumericVector data, int windowSize, int alignment) {
    X = data;
    N = windowSize;
    align = alignment;
    K = windowSize / 2;
  }

  double windowMean(const int &index) {

    // Calculate the window average
    double mean = 0;

    for (size_t i = 0; i < N; ++i) {

      if (align == -1) { // index at left edge of window
        mean += X[index + i];
      } else if (align == 0) { // index at center of window
        mean += X[index - K + i];
      } else { // index at right edge of window
        mean += X[index - i];
      }

    }

    mean /= N;

    return mean;

  }

  double windowMedian(const int &index) {

    // Calculate the median value
    NumericVector out(N, NA_REAL);

    for( int i=0; i < N; i++ ) {

      out[i] = X[index - K + i];

    }

    std::sort(out.begin(), out.end());

    return N % 2 ? out[K] : (out[K - 1 ] + out[K]) / 2;


  }

  double windowVar(const int &index) {

    double var = 0;

    double mean = windowMean(index);

    for (int i = 0; i < N; ++i) {

      if (align == -1) { // index at left edge of window
        var += (X[index + i] - mean) * (X[index + i] - mean);
      } else if (align == 0) { // index at center of window
        var += (X[index - K + i] - mean) * (X[index - K + i] - mean);
      } else { // index at right edge of window
        var += (X[index - i] - mean) * (X[index - i] - mean);
      }

    }

    var /= N - 1;

    return var;

  }


  NumericVector rollMean() {

    size_t len = X.size();

    // Initialize the output vector with NA
    NumericVector out(len, NA_REAL);


    // Get the start and endpoints
    int start = 0;
    int end = X.size();

    if (align == -1) {

      // "left" aligned means the index is at the left edge of the window
      start = 0;
      end = len - (N - 1);

    } else if (align == 0) {

      // "center" aligned
      start = K;
      end = len - K;

    } else {

      // "right" aligned means the index is at the right edge of the window
      start = N - 1;
      end = len;

    }

    // For the valid region, calculate the result
    for( int ind = start; ind < end; ++ind ) {

      out[ind] = windowMean(ind);

    }

    return out;

  }

  NumericVector rollMedian() {

    int len = X.size();

    // Initialize the output vector with NA's
    NumericVector out(len, NA_REAL);

    // For the valid region, calculate the result
    for( int ind = K; ind < len - K; ++ind ) {

      out[ind] = windowMedian(ind);

    }

    return out;

  }

  NumericVector rollVar() {

  int len = X.size();

    // Initialize the output vector with NA's
    NumericVector out(len, NA_REAL);

    // Get the start and endpoints
    int start = 0;
    int end = len;

    if (align == -1) {
      // "left" aligned means the index is at the left edge of the window
      start = 0;
      end = len - (N - 1);
    } else if (align == 0) {
      // "center" aligned
      start = K;
      end = len - K;
    } else {
      // "right" aligned means the index is at the right edge of the window
      start = N - 1;
      end = len;
    }

    // For the valid region, calculate the result
    for( int i = start; i < end; ++i) {

      out[i] = windowVar(i);

    }

    return out;

  }

};

#endif
