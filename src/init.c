#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP aggregate_age_counts(SEXP, SEXP, SEXP);
extern SEXP split_interval_counts(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP reaggregate_interval_counts(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"aggregate_age_counts",  (DL_FUNC) &aggregate_age_counts, 3},
    {"split_interval_counts", (DL_FUNC) &split_interval_counts, 5},
    {"reaggregate_interval_counts", (DL_FUNC) &reaggregate_interval_counts, 6},
    {NULL, NULL, 0}
};

void R_init_ageutils(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
