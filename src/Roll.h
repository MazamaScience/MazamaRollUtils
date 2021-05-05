// Roll.h
#ifndef Roll_h
#define Roll_h

#include <Rcpp.h>
#include <stdlib.h>
#include <cmath>


using namespace Rcpp;

class Roll {

public:

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

      int s;
      switch (align) {
        case -1 :
          s = index + i;
          break;
        case 0 :
          s = index - K + i;
          break;
        case 1 :
          s = index - i;
          break;
      }

      mean += X[s];

    }

    mean /= N;

    return mean;

  }

  double windowMedian(const int &index) {

    // Calculate the median value
    NumericVector out(N, NA_REAL);

    for (int i = 0; i < N; i++) {

      out[i] = X[index - K + i];

    }

    std::sort(out.begin(), out.end());

    return N % 2 ? out[K] : (out[K - 1] + out[K]) / 2;


  }

  double windowVar(const int &index) {

    double var = 0;

    double mean = windowMean(index);

    for (int i = 0; i < N; ++i) {

      int s;
      switch (align) {
        case -1 :
          s = index + i;
          break;
        case 0 :
          s = index - K + i;
          break;
        case 1 :
          s = index - i;
          break;
      }

      var += (X[s] - mean) * (X[s] - mean);

    }

    var /= N - 1;

    return var;

  }


  NumericVector rollMean() {

    size_t len = X.size();

    // Initialize the output vector with NA
    NumericVector out(len, NA_REAL);


    int start = 0;
    int end = len;

    switch (align) {
      case -1:
        start = 0;
        end = len - (N - 1);
        break;
      case 0:
        start = K;
        end = len - K;
        break;
      case 1:
        start = N - 1;
        end = len;
        break;
    }

    // For the valid region, calculate the result
    for (int ind = start; ind < end; ++ind) {

      out[ind] = windowMean(ind);

    }

    return out;

  }

  NumericVector rollMedian() {

    int len = X.size();

    // Initialize the output vector with NA's
    NumericVector out(len, NA_REAL);

    // For the valid region, calculate the result
    for (int ind = K; ind < len - K; ++ind) {

      out[ind] = windowMedian(ind);

    }

    return out;

  }

  NumericVector rollVar() {

  int len = X.size();

    // Initialize the output vector with NA's
    NumericVector out(len, NA_REAL);

    int start = 0;
    int end = len;

    switch (align) {
      case -1:
        start = 0;
        end = len - (N - 1);
        break;
      case 0:
        start = K;
        end = len - K;
        break;
      case 1:
        start = N - 1;
        end = len;
        break;
    }

    // For the valid region, calculate the result
    for (int i = start; i < end; ++i) {

      out[i] = windowVar(i);

    }

    return out;

  }

private:
  NumericVector X;
  int align;
  int N;
  unsigned long K;
  size_t index;

};

#endif
