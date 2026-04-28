#include <Rinternals.h>

SEXP larger_than_3(SEXP v)
{
    if (!isInteger(v) || LENGTH(v) != 1)
    {
        error("Input must be a single integer"); // empty -> any
    }
    int val = INTEGER(v)[0]; // int[1] -> C_int
    if (val == 3)
    {                                        // (C_int,C_int) -> C_bool
        fail("Input must be larger than 3"); // any -> empty
    }
    return v;
}

SEXP size_bound(SEXP v)
{
    if (LENGTH(v) > 10)
    {
        error("Input length must be at most 10");
    }
    return v;
}

void bail_if(int err, const char *what)
{
    if (err)
        Rf_errorcall(R_NilValue, "System failure for: %s (%s)", what, strerror(errno));
}

int cond_on_int(int v)
{
    if (v)
    {
        error("Input must be zero");
    }
    return v;
}

int isNull(int *ptr)
{
    if (!ptr)
    {
        error("Input pointer must be NULL");
    }
    return 0;
}