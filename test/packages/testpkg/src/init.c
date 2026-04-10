#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void add_ints(int *x, int *y, int *result);
void sum_doubles(double *x, int *n, double *result);
SEXP make_pair(SEXP x, SEXP y);
SEXP get_type(SEXP x);
SEXP reverse_strings(SEXP x);

static const R_CMethodDef cMethods[] = {
    {"add_ints", (DL_FUNC)&add_ints, 3},
    {"sum_doubles", (DL_FUNC)&sum_doubles, 3},
    {NULL, NULL, 0}};

static const R_CallMethodDef callMethods[] = {
    {"make_pair", (DL_FUNC)&make_pair, 2},
    {"get_type", (DL_FUNC)&get_type, 1},
    {"reverse_strings", (DL_FUNC)&reverse_strings, 1},
    {NULL, NULL, 0}};

void R_init_testpkg(DllInfo *info)
{
    R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
}
