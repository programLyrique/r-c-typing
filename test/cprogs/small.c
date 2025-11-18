#include <Rinternals.h>

// This is just a test
SEXP small_f(SEXP x, int test) {
    //test = test + 1;
    int y = 1;
    double z;
    if (test > 0) {
        printf("test is positive\n");
    } else {
        printf("test is non-positive\n");
    }
    return x;
}