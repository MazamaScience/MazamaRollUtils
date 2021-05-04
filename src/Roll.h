// Roll.h
#ifndef Roll_h
#define Roll_h

#include <Rcpp.h>
#include <stdlib.h>
#include <cmath>

using namespace Rcpp;

class Roll {

public:

  std::multiset <double> window;

  void windowInsert(double x) {
    window.insert(x);
  }

  void windowErase(double x) {
    window.erase(window.find(x));
  }

  double windowMedian() {

    size_t n = window.size();

    double a = *std::next(window.begin(), n / 2 - 1);
    double b = *std::next(window.begin(), n / 2);

    if (window.size() & 1) {
      return b;
    }
    return (a + b) * 0.5;

  }

  double windowMean() {

    size_t n = window.size();
    return 1.0 * std::accumulate(window.begin(), window.end(), 0LL) / n;
  }

  // Roll Median
  std::vector<double> rollMedian(NumericVector x, int windowSize) {

    std::vector<double> ans;

    window.clear();

    // initial condition
    for (size_t i = 0; i < windowSize; ++i) {
      windowInsert(x[i]);
    }

    for (size_t i = windowSize, j = 0; i < x.size(); ++i, ++j) {
      ans.push_back(windowMedian());
      windowErase(x[j]);
      windowInsert(x[i]);
    }
    ans.push_back(windowMedian());
    return ans;
  }

  // Roll Standard deviation (Welford's Algorithm)
  std::vector<double> rollMean(NumericVector x, int windowSize) {

    std::vector<double> ans;

    window.clear();

    // initial condition
    for (size_t i = 0; i < windowSize; ++i) {
      windowInsert(x[i]);
    }

    for (size_t i = windowSize, j = 0; i < x.size(); ++i, ++j) {
      ans.push_back(windowMean());
      windowErase(x[j]);
      windowInsert(x[i]);
    }
    ans.push_back(windowMean());

    return ans;
  }


};

#endif
