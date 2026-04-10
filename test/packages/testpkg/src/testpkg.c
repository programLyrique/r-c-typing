#include <R.h>
#include <Rinternals.h>

void add_ints(int *x, int *y, int *result)
{
    result[0] = x[0] + y[0];
}

void sum_doubles(double *x, int *n, double *result)
{
    double sum = 0.0;
    for (int i = 0; i < *n; i++)
    {
        sum += x[i];
    }
    result[0] = sum;
}

SEXP make_pair(SEXP x, SEXP y)
{
    return Rf_list2(x, y);
}

// SEXP get_type(SEXP x)
// {
//     return Rf_mkString(Rf_type2char(TYPEOF(x)));
// }

SEXP reverse_strings(SEXP x)
{
    if (TYPEOF(x) != STRSXP)
    {
        Rf_error("Expected a character vector");
    }

    R_xlen_t n = XLENGTH(x);
    SEXP out = PROTECT(Rf_allocVector(STRSXP, n));
    for (R_xlen_t i = 0; i < n; i++)
    {
        SET_STRING_ELT(out, i, STRING_ELT(x, n - 1 - i));
    }
    UNPROTECT(1);
    return out;
}
