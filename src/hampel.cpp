
#include <Rcpp.h>
#include <stdlib.h>
#include <cmath>
#include "Roll.h"

using namespace Rcpp;
// O(n) hampel filter solution of a subarray of size n

// Roll Median
// [[Rcpp::export]]
NumericVector rollMedian (NumericVector x, unsigned int windowSize = 5, double threshold = 1) {

  Roll ob;

  ob.initRoll(x, windowSize, 0);

  return ob.rollMedian();

}

// Roll Mean
// [[Rcpp::export]]
NumericVector rollMean (NumericVector x, unsigned int windowSize = 5) {
  Roll ob;

  ob.initRoll(x, windowSize, 0);

  return ob.rollMean();
}




