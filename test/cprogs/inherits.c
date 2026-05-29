#include <Rinternals.h>

/* inherits(x, "myclass") as a guard is lowered to a type-case. In the
   then-branch x is refined to carry class myclass (other classes allowed:
   <myclass, ...>), observable in the returned type. */
SEXP C_inherits_guard(SEXP x) {
  if (inherits(x, "myclass")) {
    return x;
  }
  return R_NilValue;
}

/* Ternary form refines the same way. */
SEXP C_inherits_ternary(SEXP x) {
  return inherits(x, "myclass") ? x : R_NilValue;
}

/* Refinement also fires on a value with a fixed type (here a function result,
   with no inference variable to silently absorb the class constraint):
   Rf_GetOption1 returns any_sexp, the guard narrows the returned x to
   <myclass, ...>. */
SEXP C_inherits_fixed(SEXP s) {
  SEXP x = Rf_GetOption1(s);
  if (inherits(x, "myclass")) {
    return x;
  }
  return R_NilValue;
}

/* Negated guard with the common early-return idiom: after !inherits returns,
   the fall-through x is refined to <myclass, ...> (branches swapped). */
SEXP C_inherits_not_guard(SEXP x) {
  if (!inherits(x, "myclass"))
    return R_NilValue;
  return x;
}

/* Negated ternary refines the same way. */
SEXP C_inherits_not_ternary(SEXP x) {
  return !inherits(x, "myclass") ? R_NilValue : x;
}
