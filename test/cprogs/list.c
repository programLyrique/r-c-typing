#include <Rinternals.h>


// SEXP create_pos_list() {
//     return allocVector(VECSXP, 3);
// }

// SEXP create_list() {
//     int n = 3;
//     return allocVector(VECSXP, n);
// }

SEXP create_named_list() {
    const char *names[] = {"a", "b", ""};
    return mkNamed(VECSXP, names);
}