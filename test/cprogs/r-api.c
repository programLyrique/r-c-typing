/*
To check the types of some of the R API functions
*/

#include <Rinternals.h>

void check_interrupt_fn(void *dummy) {
  R_CheckUserInterrupt();
}

int pending_interrupt(void) {
  return !(R_ToplevelExec(check_interrupt_fn, NULL));
}


SEXP build_extptr(SEXP addr, SEXP tag, SEXP prot) {
  return R_MakeExternalPtr( INTEGER(addr), tag, prot);
}