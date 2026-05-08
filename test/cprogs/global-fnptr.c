/* Regression test: file-scope function-pointer globals.
 *
 * Three shapes the parser previously dropped (or kept as immutable),
 * causing later assignments inside init functions to crash mlsem with
 * "Cannot assign to an immutable variable":
 *
 *   - void (*fp)(void);                      no return-pointer, no init
 *   - void (*fp_init)(void) = 0;             no return-pointer, with init
 *   - int* (*fp_ret_ptr)(void) = 0;          pointer-return, with init
 *
 * Each is targeted by a setter, and one setter assigns through a cast
 * argument the way vctrs' rlang vendor stub does. */

void (*fp)(void);
void (*fp_init)(void) = 0;
int* (*fp_ret_ptr)(void) = 0;

void set_fp(void (*p)(void))      { fp = p; }
void set_fp_init(void (*p)(void)) { fp_init = p; }
void set_fp_ret_ptr(int* (*p)(void)) { fp_ret_ptr = p; }
