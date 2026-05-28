#include <Rinternals.h>

/* Class attribute is specialised to v(chr). */
SEXP C_first_class(SEXP x) {
  return STRING_ELT(getAttrib(x, R_ClassSymbol), 0);
}

int C_n_classes(SEXP x) {
  return LENGTH(getAttrib(x, R_ClassSymbol));
}

/* Non-class attributes go through the generic any_sexp path. */
SEXP C_names(SEXP x) {
  return getAttrib(x, R_NamesSymbol);
}

SEXP C_dim(SEXP x) {
  return getAttrib(x, R_DimSymbol);
}

/* Refined branch: when v has a concrete class set, the result narrows to
   v[N](class-name singletons). test_class_fixture is declared in base.ty
   as [int <myclass>]; its class attribute should be v[1](chr("myclass"))
   and STRING_ELT preserves the singleton through its 'a parameter. 

TODO: remove the test fixture and also support setAttrib so that we can 
actually set-up classes.
*/
extern SEXP test_class_fixture;
SEXP C_refined_class(void) {
  return getAttrib(test_class_fixture, R_ClassSymbol);
}
int C_refined_class_length(void) {
  return LENGTH(getAttrib(test_class_fixture, R_ClassSymbol));
}
SEXP C_refined_class_first(void) {
  return STRING_ELT(getAttrib(test_class_fixture, R_ClassSymbol), 0);
}
