#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP c_any_infinite(SEXP);
extern SEXP c_get_threads(void);
extern SEXP c_set_threads(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"c_any_infinite", (DL_FUNC) &c_any_infinite, 1},
    {"c_get_threads",  (DL_FUNC) &c_get_threads,  0},
    {"c_set_threads",  (DL_FUNC) &c_set_threads,  1},
    {NULL, NULL, 0}
};

void R_init_svAssert(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
