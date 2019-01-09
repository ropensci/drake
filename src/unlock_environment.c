/* Adapted from:
 * https://gist.github.com/wch/3280369#file-unlockenvironment-r and
 * https://github.com/SurajGupta/r-source/blob/master/src/main/envir.c
 */

#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <Rversion.h>

#ifndef FRAME_LOCK_MASK
#define FRAME_LOCK_MASK (1<<14)
#endif

#ifndef FRAME_IS_LOCKED
#define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
#endif

#ifndef UNLOCK_FRAME
#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ FRAME_LOCK_MASK))
#endif

SEXP Cunlock_environment(SEXP envir) {
  UNLOCK_FRAME(envir);
  return R_NilValue;
}

static const R_CallMethodDef call_methods[] = {
  {"Cunlock_environment", (DL_FUNC) &Cunlock_environment, 1},
  {NULL, NULL, 0}
};

void R_init_drake(DllInfo *dll) {
  R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
  #if defined(R_VERSION) && R_VERSION >= R_Version(3, 4, 0)
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
  #endif
}
