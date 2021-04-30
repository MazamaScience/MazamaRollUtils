
#include <Rcpp.h>
#include <stdlib.h>
#include <cmath>

using namespace Rcpp;

// Median
// Use nth_element to minimize sorting req
double median (std::vector<double> v)
{
    size_t n = v.size() / 2;
    std::nth_element(v.begin(), v.begin() + n, v.end());
    return v[n];
}

double mad () {
    return -1;
}

std::vector<double> window (std::vector<double> v, int n, int k)
{
    std::vector<double> tmp;
    tmp.resize(n, 0);
    for (int i = 0; i < n; ++i)
    {
        tmp[i] = v[k + i];
    }
    return tmp;
}

// O(n) hampel filter solution of a subarray of size n

// [[Rcpp::export]]
RObject hampel (NumericVector x, int n)
{

    const double kappa = 1.4826;

    std::vector<double> vec;
    vec.resize(x.size() + n - 1, 0);

    // copy over the input NV to a std vec
    for (int i = n - 1; i < vec.size(); ++i)
    {
        vec[i] = x[i - n + 1];
    }


    std::vector<double> tmp;
    tmp.resize(n, 0);

    std::vector<double> out;
    out.resize(x.size(), NA_REAL);

    for (int step = 0; step < x.size(); ++step)
    {
        tmp = window(vec, n, step);
        out[step] = median(tmp);
    }


    return Rcpp::wrap(out);
}

