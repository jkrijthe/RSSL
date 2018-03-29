#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern void svmpredictd(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void svmtraind(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
SEXP _RSSL_harmonic_function_cpp(SEXP WSEXP, SEXP YSEXP);
SEXP _RSSL_svmlin_rcpp(SEXP XSEXP, SEXP ySEXP, SEXP lSEXP, SEXP algorithmSEXP, SEXP lambdaSEXP, SEXP lambda_uSEXP, SEXP max_switchSEXP, SEXP pos_fracSEXP, SEXP CpSEXP, SEXP CnSEXP, SEXP costsSEXP, SEXP verboseSEXP);
SEXP _RSSL_rowMax(SEXP XSEXP);
SEXP _RSSL_rowMax2(SEXP XSEXP);
SEXP _RSSL_which_rowMax(SEXP XSEXP);
SEXP _RSSL_which_rowMax2(SEXP XSEXP);
SEXP _RSSL_sort_matrix(SEXP XSEXP);
SEXP _RSSL_rowwise_addition(SEXP ASEXP, SEXP xSEXP);
SEXP _RSSL_factor_to_dummy_cpp(SEXP ySEXP, SEXP cSEXP);


static const R_CallMethodDef CallEntries[] = {
    {"_RSSL_harmonic_function_cpp", (DL_FUNC) &_RSSL_harmonic_function_cpp, 2},
    {"_RSSL_svmlin_rcpp", (DL_FUNC) &_RSSL_svmlin_rcpp, 12},
    {"_RSSL_rowMax", (DL_FUNC) &_RSSL_rowMax, 1},
    {"_RSSL_rowMax2", (DL_FUNC) &_RSSL_rowMax2, 1},
    {"_RSSL_which_rowMax", (DL_FUNC) &_RSSL_which_rowMax, 1},
    {"_RSSL_which_rowMax2", (DL_FUNC) &_RSSL_which_rowMax2, 1},
    {"_RSSL_sort_matrix", (DL_FUNC) &_RSSL_sort_matrix, 1},
    {"_RSSL_rowwise_addition", (DL_FUNC) &_RSSL_rowwise_addition, 2},
    {"_RSSL_factor_to_dummy_cpp", (DL_FUNC) &_RSSL_factor_to_dummy_cpp, 2},
    {NULL, NULL, 0}
};

static const R_CMethodDef CEntries[] = {
  {"svmpredictd", (DL_FUNC) &svmpredictd, 30},
  {"svmtraind",   (DL_FUNC) &svmtraind,   40},
  {NULL, NULL, 0}
};

void R_init_RSSL(DllInfo *dll) {
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
