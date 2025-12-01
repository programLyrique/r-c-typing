#include <Rinternals.h>

SEXP test_while(SEXP n) {
    while(INTEGER(n)[0] > 0) {
        INTEGER(n)[0] = INTEGER(n)[0] - 1;
    }
    return n;
}