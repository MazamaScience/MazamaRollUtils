
#include <Rcpp.h>
#include <stdlib.h>
#include <cmath>
#include "Roll.h"

using namespace Rcpp;



// O(n) hampel filter solution of a subarray of size n

// [[Rcpp::export]]
RObject hampel (NumericVector x, unsigned int windowSize = 5, double threshold = 1)
{
    // debug test
    // x = {1,2,4,9,23,8,12,4,2};
    //

    Roll ob;

    // double out = sliding_window_mad(x, 5);

    return Rcpp::wrap(ob.rollMean(x, windowSize));

}




