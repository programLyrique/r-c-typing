#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void testpkg_add_ints(int *x, int *y, int *result);
void testpkg_sum_doubles(double *x, int *n, double *result);
SEXP testpkg_make_pair(SEXP x, SEXP y);
SEXP testpkg_get_type(SEXP x);
SEXP testpkg_reverse_strings(SEXP x);

static const R_CMethodDef cMethods[] = {
    {"testpkg_add_ints", (DL_FUNC)&testpkg_add_ints, 3},
    {"testpkg_sum_doubles", (DL_FUNC)&testpkg_sum_doubles, 3},
    {NULL, NULL, 0}};

static const R_CallMethodDef callMethods[] = {
    {"testpkg_make_pair", (DL_FUNC)&testpkg_make_pair, 2},
    {"testpkg_get_type", (DL_FUNC)&testpkg_get_type, 1},
    {"testpkg_reverse_strings", (DL_FUNC)&testpkg_reverse_strings, 1},
    {NULL, NULL, 0}};

void R_init_testpkg(DllInfo *info)
{
    R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
}
