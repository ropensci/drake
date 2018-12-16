/* Adapted from:
 * https://gist.github.com/wch/3280369#file-unlockenvironment-r and
 * https://github.com/SurajGupta/r-source/blob/master/src/main/envir.c
 */

#include <R.h>
#include <Rinternals.h>

#ifndef DRAKE_FRAME_LOCK_MASK
#define DRAKE_FRAME_LOCK_MASK (1<<14)
#endif

#ifndef DRAKE_FRAME_IS_LOCKED
#define DRAKE_FRAME_IS_LOCKED(e) (ENVFLAGS(e) & DRAKE_FRAME_LOCK_MASK)
#endif

#ifndef DRAKE_UNLOCK_FRAME
#define DRAKE_UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ DRAKE_FRAME_LOCK_MASK))
#endif

SEXP unlock_environment(SEXP envir) {
  DRAKE_UNLOCK_FRAME(envir);
  return R_NilValue;
}

static const R_CallMethodDef callMethods[] = {
  {"unlock_environment", (DL_FUNC) &unlock_environment, 1},
  {NULL, NULL, 0}
};

void R_init_drake(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE); 
}
