// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// txt_to_df_cpp
void txt_to_df_cpp(std::string input_file, std::string output_file);
RcppExport SEXP _patentr_txt_to_df_cpp(SEXP input_fileSEXP, SEXP output_fileSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type input_file(input_fileSEXP);
    Rcpp::traits::input_parameter< std::string >::type output_file(output_fileSEXP);
    txt_to_df_cpp(input_file, output_file);
    return R_NilValue;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_patentr_txt_to_df_cpp", (DL_FUNC) &_patentr_txt_to_df_cpp, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_patentr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
