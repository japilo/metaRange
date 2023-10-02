// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// calculate_suitability
NumericVector calculate_suitability(double vmax, double vopt, double vmin, NumericVector venv);
RcppExport SEXP _metaRange_calculate_suitability(SEXP vmaxSEXP, SEXP voptSEXP, SEXP vminSEXP, SEXP venvSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type vmax(vmaxSEXP);
    Rcpp::traits::input_parameter< double >::type vopt(voptSEXP);
    Rcpp::traits::input_parameter< double >::type vmin(vminSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type venv(venvSEXP);
    rcpp_result_gen = Rcpp::wrap(calculate_suitability(vmax, vopt, vmin, venv));
    return rcpp_result_gen;
END_RCPP
}
// dispersal_fixed_undirected
arma::mat dispersal_fixed_undirected(arma::mat abundance, arma::mat dispersal_kernel);
RcppExport SEXP _metaRange_dispersal_fixed_undirected(SEXP abundanceSEXP, SEXP dispersal_kernelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type abundance(abundanceSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type dispersal_kernel(dispersal_kernelSEXP);
    rcpp_result_gen = Rcpp::wrap(dispersal_fixed_undirected(abundance, dispersal_kernel));
    return rcpp_result_gen;
END_RCPP
}
// dispersal_fixed_directed
arma::mat dispersal_fixed_directed(arma::mat abundance, arma::mat suitability, arma::mat dispersal_kernel);
RcppExport SEXP _metaRange_dispersal_fixed_directed(SEXP abundanceSEXP, SEXP suitabilitySEXP, SEXP dispersal_kernelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type abundance(abundanceSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type suitability(suitabilitySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type dispersal_kernel(dispersal_kernelSEXP);
    rcpp_result_gen = Rcpp::wrap(dispersal_fixed_directed(abundance, suitability, dispersal_kernel));
    return rcpp_result_gen;
END_RCPP
}
// metabolic_scaling
NumericVector metabolic_scaling(double normalization_constant, double scaling_exponent, NumericVector mass, NumericVector temperature, double E, double k);
RcppExport SEXP _metaRange_metabolic_scaling(SEXP normalization_constantSEXP, SEXP scaling_exponentSEXP, SEXP massSEXP, SEXP temperatureSEXP, SEXP ESEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type normalization_constant(normalization_constantSEXP);
    Rcpp::traits::input_parameter< double >::type scaling_exponent(scaling_exponentSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mass(massSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type temperature(temperatureSEXP);
    Rcpp::traits::input_parameter< double >::type E(ESEXP);
    Rcpp::traits::input_parameter< double >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(metabolic_scaling(normalization_constant, scaling_exponent, mass, temperature, E, k));
    return rcpp_result_gen;
END_RCPP
}
// ricker_reproduction_model
NumericVector ricker_reproduction_model(NumericVector abundance, NumericVector reproduction_rate, NumericVector carrying_capacity);
RcppExport SEXP _metaRange_ricker_reproduction_model(SEXP abundanceSEXP, SEXP reproduction_rateSEXP, SEXP carrying_capacitySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type abundance(abundanceSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type reproduction_rate(reproduction_rateSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type carrying_capacity(carrying_capacitySEXP);
    rcpp_result_gen = Rcpp::wrap(ricker_reproduction_model(abundance, reproduction_rate, carrying_capacity));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_metaRange_calculate_suitability", (DL_FUNC) &_metaRange_calculate_suitability, 4},
    {"_metaRange_dispersal_fixed_undirected", (DL_FUNC) &_metaRange_dispersal_fixed_undirected, 2},
    {"_metaRange_dispersal_fixed_directed", (DL_FUNC) &_metaRange_dispersal_fixed_directed, 3},
    {"_metaRange_metabolic_scaling", (DL_FUNC) &_metaRange_metabolic_scaling, 6},
    {"_metaRange_ricker_reproduction_model", (DL_FUNC) &_metaRange_ricker_reproduction_model, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_metaRange(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
