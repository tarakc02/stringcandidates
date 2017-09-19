#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::string substitution(std::string x) {
    double prob = 1.0 / x.size();
    NumericVector samps = runif(x.size());
    for ( int i = 0; i < x.size(); i++) {
        if (samps[i] <= prob) x.replace(i, 1, "X");
    }
    return x;
}
