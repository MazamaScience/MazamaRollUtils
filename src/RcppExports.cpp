// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// roll_median
Rcpp::NumericVector roll_median(Rcpp::NumericVector x, unsigned int n, Rcpp::NumericVector weights, int by, int align);
RcppExport SEXP _MazamaRollUtils_roll_median(SEXP xSEXP, SEXP nSEXP, SEXP weightsSEXP, SEXP bySEXP, SEXP alignSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type n(nSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< int >::type align(alignSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_median(x, n, weights, by, align));
    return rcpp_result_gen;
END_RCPP
}
// roll_mean
Rcpp::NumericVector roll_mean(Rcpp::NumericVector x, unsigned int n, Rcpp::NumericVector weights, int by, int align);
RcppExport SEXP _MazamaRollUtils_roll_mean(SEXP xSEXP, SEXP nSEXP, SEXP weightsSEXP, SEXP bySEXP, SEXP alignSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type n(nSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< int >::type align(alignSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_mean(x, n, weights, by, align));
    return rcpp_result_gen;
END_RCPP
}
// roll_var
Rcpp::NumericVector roll_var(Rcpp::NumericVector x, unsigned int n, Rcpp::NumericVector weights, int by, int align);
RcppExport SEXP _MazamaRollUtils_roll_var(SEXP xSEXP, SEXP nSEXP, SEXP weightsSEXP, SEXP bySEXP, SEXP alignSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type n(nSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< int >::type align(alignSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_var(x, n, weights, by, align));
    return rcpp_result_gen;
END_RCPP
}
// roll_sd
Rcpp::NumericVector roll_sd(Rcpp::NumericVector x, unsigned int n, Rcpp::NumericVector weights, int by, int align);
RcppExport SEXP _MazamaRollUtils_roll_sd(SEXP xSEXP, SEXP nSEXP, SEXP weightsSEXP, SEXP bySEXP, SEXP alignSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type n(nSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< int >::type align(alignSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_sd(x, n, weights, by, align));
    return rcpp_result_gen;
END_RCPP
}
// roll_hampel
Rcpp::NumericVector roll_hampel(Rcpp::NumericVector x, unsigned int n, Rcpp::NumericVector weights, int by, int align);
RcppExport SEXP _MazamaRollUtils_roll_hampel(SEXP xSEXP, SEXP nSEXP, SEXP weightsSEXP, SEXP bySEXP, SEXP alignSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type n(nSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< int >::type align(alignSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_hampel(x, n, weights, by, align));
    return rcpp_result_gen;
END_RCPP
}
// roll_max
Rcpp::NumericVector roll_max(Rcpp::NumericVector x, unsigned int n, Rcpp::NumericVector weights, int by, int align);
RcppExport SEXP _MazamaRollUtils_roll_max(SEXP xSEXP, SEXP nSEXP, SEXP weightsSEXP, SEXP bySEXP, SEXP alignSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type n(nSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< int >::type align(alignSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_max(x, n, weights, by, align));
    return rcpp_result_gen;
END_RCPP
}
// roll_min
Rcpp::NumericVector roll_min(Rcpp::NumericVector x, unsigned int n, Rcpp::NumericVector weights, int by, int align);
RcppExport SEXP _MazamaRollUtils_roll_min(SEXP xSEXP, SEXP nSEXP, SEXP weightsSEXP, SEXP bySEXP, SEXP alignSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type n(nSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< int >::type align(alignSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_min(x, n, weights, by, align));
    return rcpp_result_gen;
END_RCPP
}
// roll_sum
Rcpp::NumericVector roll_sum(Rcpp::NumericVector x, unsigned int n, Rcpp::NumericVector weights, int by, int align);
RcppExport SEXP _MazamaRollUtils_roll_sum(SEXP xSEXP, SEXP nSEXP, SEXP weightsSEXP, SEXP bySEXP, SEXP alignSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type n(nSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< int >::type align(alignSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_sum(x, n, weights, by, align));
    return rcpp_result_gen;
END_RCPP
}
// roll_prod
Rcpp::NumericVector roll_prod(Rcpp::NumericVector x, unsigned int n, Rcpp::NumericVector weights, int by, int align);
RcppExport SEXP _MazamaRollUtils_roll_prod(SEXP xSEXP, SEXP nSEXP, SEXP weightsSEXP, SEXP bySEXP, SEXP alignSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type n(nSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< int >::type align(alignSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_prod(x, n, weights, by, align));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_MazamaRollUtils_roll_median", (DL_FUNC) &_MazamaRollUtils_roll_median, 5},
    {"_MazamaRollUtils_roll_mean", (DL_FUNC) &_MazamaRollUtils_roll_mean, 5},
    {"_MazamaRollUtils_roll_var", (DL_FUNC) &_MazamaRollUtils_roll_var, 5},
    {"_MazamaRollUtils_roll_sd", (DL_FUNC) &_MazamaRollUtils_roll_sd, 5},
    {"_MazamaRollUtils_roll_hampel", (DL_FUNC) &_MazamaRollUtils_roll_hampel, 5},
    {"_MazamaRollUtils_roll_max", (DL_FUNC) &_MazamaRollUtils_roll_max, 5},
    {"_MazamaRollUtils_roll_min", (DL_FUNC) &_MazamaRollUtils_roll_min, 5},
    {"_MazamaRollUtils_roll_sum", (DL_FUNC) &_MazamaRollUtils_roll_sum, 5},
    {"_MazamaRollUtils_roll_prod", (DL_FUNC) &_MazamaRollUtils_roll_prod, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_MazamaRollUtils(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
