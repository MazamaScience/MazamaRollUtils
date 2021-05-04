
#include <Rcpp.h>
#include <stdlib.h>
#include <cmath>

using namespace Rcpp;

class HampelFilter {
public:

    std::multiset <double> arr;

    void insert(double x) {
        arr.insert(x);
    }

    void erase(double x) {
        arr.erase(arr.find(x));
    }

    double median() {

        int n = arr.size();

        double a = *std::next(arr.begin(), n/2 - 1);
        double b = *std::next(arr.begin(), n/2);

        if (arr.size() & 1) {
            return b;
        }
        return (a + b) * 0.5;

    }

    std::vector<double> medianSlidingWindow(NumericVector x, int windowSize) {

        std::vector<double> ans;

        arr.clear();

        for (int i = 0; i < windowSize; ++i) {
            insert(x[i]);
        }

        for (int i = windowSize, j = 0; i < x.size(); ++i, ++j) {
            ans.push_back(median());
            erase(x[j]);
            insert(x[i]);
        }
        ans.push_back(median());
        return ans;
    }


};

// O(n) hampel filter solution of a subarray of size n

// [[Rcpp::export]]
RObject hampel (NumericVector x, unsigned int windowSize = 5, double threshold = 1)
{
    // debug test
    x = {1,2,4,9,23,8,12,4,2};
    //

    HampelFilter ob;

    // double out = sliding_window_mad(x, 5);

    return Rcpp::wrap(ob.medianSlidingWindow(x, 3));

}


