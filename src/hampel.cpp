
#include <Rcpp.h>
#include <stdlib.h>
#include <cmath>

using namespace Rcpp;


double HampelFilter(std::vector<double> x, unsigned int windowSize = 5, double threshold = 1)
{

    // half window for median num
    const unsigned medianNum = windowSize/2;
    const double kappa = 1.4826;

    double filteredVal = (x)[medianNum];

    std::vector<double> sortedVector, kVector;

    sortedVector.assign(x.begin(), x.end());
    std::sort(sortedVector.begin(), sortedVector.end());

    double medianVal = sortedVector[medianNum];

    for (auto &value : sortedVector)
    {
        kVector.emplace_back(std::fabs(value - medianVal));
    }

    std::sort(kVector.begin(), kVector.end());
    double kValue = kVector[medianNum] * kappa;

    if (std::fabs(filteredVal - medianVal) >= threshold * kValue)
    {
        filteredVal = medianVal;
    }

    return filteredVal;

}


// O(n) hampel filter solution of a subarray of size n
// [[Rcpp::export]]
RObject hampel (NumericVector x, unsigned int windowSize = 5, double threshold = 1)
{
    // debug test
    x = {1,2,4,9,23,8,12,4,2};

    // collector vector and output filtered
    std::vector<double> collector, filtered;

    // handle 0th position
    for (int i = collector.size(); i < windowSize / 2; ++i)
    {
        collector.emplace_back(0);
    }

    // use circular array
    unsigned int step = 0;
    while(step < x.size() - (windowSize / 2))
    {

        // Add input vector at step to back of collector
        collector.emplace_back(x[step]);
        if (collector.size() == windowSize)
        {
            filtered.emplace_back(HampelFilter(collector, windowSize, 1));
            collector.erase(collector.begin());
        }
        ++step;
    }

    for (auto &value : filtered)
    {
        // debug
        // std::cout << value;
    }

    return Rcpp::wrap(filtered);

}


