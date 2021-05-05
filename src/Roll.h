// Roll.h
#ifndef Roll_h
#define Roll_h

#include <Rcpp.h>
#include <stdlib.h>
#include <cmath>


using namespace Rcpp;

class Roll {

public:

  NumericVector x;

  int align;
  int win;

  void  initRoll(NumericVector data, int windowSize, int alignment) {
    x = data;
    win = windowSize;
    align = alignment;
  }

  double windowMean(const int &index) {

    // Calculate the window average
    double mean = 0;

    for (size_t i = 0; i < win; ++i) {

      if (align == -1) { // index at left edge of window
        mean += x[index + i];
      } else if (align == 0) { // index at center of window
        mean += x[index - (win / 2) + i];
      } else { // index at right edge of window
        mean += x[index - i];
      }

    }

    mean /= win;

    return mean;

  }

  double windowMedian(const int &index) {

    // Get the half-window width
    int k = win / 2;

    // Calculate the median value
    NumericVector out(win, NA_REAL);

    for( int i=0; i<win; i++ ) {

      out[i] = x[index - k + i];

    }

    std::sort(out.begin(), out.end());

    return win % 2 ? out[k] : (out[k - 1 ] + out[k]) / 2;


  }


  NumericVector rollMean() {

    size_t len = x.size();

    // Initialize the output vector with NA
    NumericVector out(len, NA_REAL);


    // Get the start and endpoints
    int start = 0;
    int end = x.size();

    if (align == -1) {

      // "left" aligned means the index is at the left edge of the window
      start = 0;
      end = len - (win - 1);

    } else if (align == 0) {

      // "center" aligned
      start = win / 2;
      end = len - win / 2;

    } else {

      // "right" aligned means the index is at the right edge of the window
      start = win - 1;
      end = len;

    }

    // For the valid region, calculate the result
    for( int ind = start; ind < end; ++ind ) {

      out[ind] = windowMean(ind);

    }

    return out;

  }

  NumericVector rollMedian() {

    int len = x.size();

    // Initialize the output vector with NA's
    NumericVector out(len, NA_REAL);

    // Get the half-window width
    int k = win / 2;

    // For the valid region, calculate the result
    for( int ind = k; ind < len - k; ++ind ) {

      out[ind] = windowMedian(ind);

    }

    return out;

  }

};

#endif
