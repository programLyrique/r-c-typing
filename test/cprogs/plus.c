#include <Rinternals.h>

// SEXP r_plus(SEXP a, SEXP b) {
//    // handle both ints and reals, whatever their sizes
//    // recycle the shorter vector
//     if (isInteger(a) && isInteger(b)) {
//          // both are integer vectors
//          R_xlen_t na = XLENGTH(a);
//          R_xlen_t nb = XLENGTH(b);
//          R_xlen_t nres;
//          if (na > nb) {
//               nres = na;
//          } else {
//               nres = nb;
//          }
//          SEXP result = PROTECT(allocVector(INTSXP, nres));
//          for (R_xlen_t i = 0; i < nres; i++) {
//               INTEGER(result)[i] = INTEGER(a)[i % na] + INTEGER(b)[i % nb];
//          }
//          UNPROTECT(1);
//          return result;
//     } else {
//          // at least one is a real vector
//          R_xlen_t na = XLENGTH(a);
//          R_xlen_t nb = XLENGTH(b);
//          R_xlen_t nres;
//         if (na > nb) {
//               nres = na;
//          } else {
//               nres = nb;
//          }
//          SEXP result = PROTECT(allocVector(REALSXP, nres));
//          for (R_xlen_t i = 0; i < nres; i++) {
//               double va = isInteger(a) ? INTEGER(a)[i % na] : REAL(a)[i % na];
//               double vb = isInteger(b) ? INTEGER(b)[i % nb] : REAL(b)[i % nb];
//               REAL(result)[i] = va + vb;
//          }
//          UNPROTECT(1);
//          return result;
//     }
// }

SEXP r_plus_scalar(SEXP a, SEXP b) {
   // int and real 
    if (isInteger(a) && isInteger(b)) {
         SEXP result = PROTECT(allocVector(INTSXP, 1));
         INTEGER(result)[0] = INTEGER(a)[0] + INTEGER(b)[0];
         UNPROTECT(1);
         return result;
    } else {
        //  double va = /*isInteger(a) ? INTEGER(a)[0] :*/ REAL(a)[0];
        //  double vb = /*isInteger(b) ? INTEGER(b)[0] :*/ REAL(b)[0];
         SEXP result = PROTECT(allocVector(REALSXP, 1));
         REAL(result)[0] = REAL(a)[0] + REAL(b)[0];
         UNPROTECT(1);
         return result;
    }
}

// SEXP incr_real(SEXP a) {
//     if (!isReal(a) || LENGTH(a) != 1) {
//         error("Input must be a single real"); // Ty.empty -> Ty.any
//     }
//     double val = REAL(a)[0]; // real[1] -> C_double
//     val = val + 1.0; // (C_double,C_double) -> C_double
//     SEXP result = PROTECT(allocVector(REALSXP, 1)); // REALSXP * 1 -> real[1] 
//     REAL(result)[0] = val;
//     UNPROTECT(1);
//     return result;
// }

// SEXP r_plus_scalar_real(SEXP a, SEXP b) {
//    // real only
//     SEXP result = PROTECT(allocVector(REALSXP, 1));
//     REAL(result)[0] = REAL(a)[0] + REAL(b)[0];
//     UNPROTECT(1);
//     return result;
// }

SEXP make_int_or_float(int test) {
    SEXP result;
    if (test == 1) {
        result = PROTECT(allocVector(INTSXP, 1)); 
        INTEGER(result)[0] = 42;
        UNPROTECT(1);
        return result;
    } else {
         result = PROTECT(allocVector(INTSXP, 2)); 
        INTEGER(result)[0] = 42;
        UNPROTECT(1);
        return result;
    }
}