/* The struct body is not visible here — it is only declared in attrib.c.
 * The runner must collect type declarations across all files before
 * registering globals so that "r_syms" gets the populated struct type
 * regardless of which file is processed first. */

struct r_globals_syms r_syms;

void init(void) {
  (void)r_syms;
}
