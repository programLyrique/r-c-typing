#include <Rinternals.h>

SEXP create_env() {
    return R_NewEnv(R_GlobalEnv, 0, 29);
}