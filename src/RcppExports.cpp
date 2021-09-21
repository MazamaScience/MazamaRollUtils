// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// roll_max
Rcpp::NumericVector roll_max(Rcpp::NumericVector x, unsigned int width, int by, int align);
RcppExport SEXP _MazamaRollUtils_roll_max(SEXP xSEXP, SEXP widthSEXP, SEXP bySEXP, SEXP alignSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type width(widthSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< int >::type align(alignSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_max(x, width, by, align));
    return rcpp_result_gen;
END_RCPP
}
// roll_mean
Rcpp::NumericVector roll_mean(Rcpp::NumericVector x, unsigned int width, int by, int align, Rcpp::Nullable<Rcpp::NumericVector> weights);
RcppExport SEXP _MazamaRollUtils_roll_mean(SEXP xSEXP, SEXP widthSEXP, SEXP bySEXP, SEXP alignSEXP, SEXP weightsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type width(widthSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< int >::type align(alignSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable<Rcpp::NumericVector> >::type weights(weightsSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_mean(x, width, by, align, weights));
    return rcpp_result_gen;
END_RCPP
}
// roll_median
Rcpp::NumericVector roll_median(Rcpp::NumericVector x, unsigned int width, int by, int align);
RcppExport SEXP _MazamaRollUtils_roll_median(SEXP xSEXP, SEXP widthSEXP, SEXP bySEXP, SEXP alignSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type width(widthSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< int >::type align(alignSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_median(x, width, by, align));
    return rcpp_result_gen;
END_RCPP
}
// roll_min
Rcpp::NumericVector roll_min(Rcpp::NumericVector x, unsigned int width, int by, int align);
RcppExport SEXP _MazamaRollUtils_roll_min(SEXP xSEXP, SEXP widthSEXP, SEXP bySEXP, SEXP alignSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type width(widthSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< int >::type align(alignSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_min(x, width, by, align));
    return rcpp_result_gen;
END_RCPP
}
// roll_prod
Rcpp::NumericVector roll_prod(Rcpp::NumericVector x, unsigned int width, int by, int align);
RcppExport SEXP _MazamaRollUtils_roll_prod(SEXP xSEXP, SEXP widthSEXP, SEXP bySEXP, SEXP alignSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type width(widthSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< int >::type align(alignSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_prod(x, width, by, align));
    return rcpp_result_gen;
END_RCPP
}
// roll_sd
Rcpp::NumericVector roll_sd(Rcpp::NumericVector x, unsigned int width, int by, int align);
RcppExport SEXP _MazamaRollUtils_roll_sd(SEXP xSEXP, SEXP widthSEXP, SEXP bySEXP, SEXP alignSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type width(widthSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< int >::type align(alignSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_sd(x, width, by, align));
    return rcpp_result_gen;
END_RCPP
}
// roll_sum
Rcpp::NumericVector roll_sum(Rcpp::NumericVector x, unsigned int width, int by, int align);
RcppExport SEXP _MazamaRollUtils_roll_sum(SEXP xSEXP, SEXP widthSEXP, SEXP bySEXP, SEXP alignSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type width(widthSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< int >::type align(alignSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_sum(x, width, by, align));
    return rcpp_result_gen;
END_RCPP
}
// roll_var
Rcpp::NumericVector roll_var(Rcpp::NumericVector x, unsigned int width, int by, int align);
RcppExport SEXP _MazamaRollUtils_roll_var(SEXP xSEXP, SEXP widthSEXP, SEXP bySEXP, SEXP alignSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type width(widthSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    Rcpp::traits::input_parameter< int >::type align(alignSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_var(x, width, by, align));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_MazamaRollUtils_roll_max", (DL_FUNC) &_MazamaRollUtils_roll_max, 4},
    {"_MazamaRollUtils_roll_mean", (DL_FUNC) &_MazamaRollUtils_roll_mean, 5},
    {"_MazamaRollUtils_roll_median", (DL_FUNC) &_MazamaRollUtils_roll_median, 4},
    {"_MazamaRollUtils_roll_min", (DL_FUNC) &_MazamaRollUtils_roll_min, 4},
    {"_MazamaRollUtils_roll_prod", (DL_FUNC) &_MazamaRollUtils_roll_prod, 4},
    {"_MazamaRollUtils_roll_sd", (DL_FUNC) &_MazamaRollUtils_roll_sd, 4},
    {"_MazamaRollUtils_roll_sum", (DL_FUNC) &_MazamaRollUtils_roll_sum, 4},
    {"_MazamaRollUtils_roll_var", (DL_FUNC) &_MazamaRollUtils_roll_var, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_MazamaRollUtils(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
