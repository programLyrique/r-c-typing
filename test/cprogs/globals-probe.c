// Probe how the checker handles file-scope (global) variables.
// No asserts — run typing_test.exe and inspect the output.

#include <Rinternals.h>

// --- extern-only (declaration, no definition) ---
extern SEXP ext_sexp;
extern int  ext_int;
extern const int ext_const_int;

// --- tentative definition (no initializer) ---
SEXP def_sexp;
int  def_int;

// --- definition with literal initializer ---
int def_int_42 = 42;
const int def_const_int_42 = 42;

// --- definition with non-literal initializer (R global) ---
SEXP def_sexp_nil = R_NilValue;

// --- static (internal linkage) ---
static SEXP stat_sexp;
static int  stat_int_7 = 7;

// --- array and struct globals ---
int def_arr[3] = {1, 2, 3};
struct point { int x; int y; };
struct point def_pt = {10, 20};

// ============ functions exercising each global ============

SEXP use_ext_sexp(void)         { return ext_sexp; }
int  use_ext_int(void)          { return ext_int; }
int  use_ext_const_int(void)    { return ext_const_int; }

SEXP use_def_sexp(void)         { return def_sexp; }
int  use_def_int(void)          { return def_int; }

int  use_def_int_42(void)       { return def_int_42; }
int  use_def_const_int_42(void) { return def_const_int_42; }

SEXP use_def_sexp_nil(void)     { return def_sexp_nil; }

SEXP use_stat_sexp(void)        { return stat_sexp; }
int  use_stat_int_7(void)       { return stat_int_7; }

int  use_def_arr(int i)         { return def_arr[i]; }
int  use_def_pt_x(void)         { return def_pt.x; }

// Two readers of the same global — consistency check
SEXP use_ext_sexp_A(void) { return ext_sexp; }
SEXP use_ext_sexp_B(void) { return ext_sexp; }

// Writer + reader — do they agree?
void set_def_int(int v) { def_int = v; }
int  get_def_int(void)  { return def_int; }
