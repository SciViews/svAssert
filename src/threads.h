#ifndef SVASSERT_THREADS_H_
#define SVASSERT_THREADS_H_

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Visibility.h>

extern int n_threads;

SEXP c_get_threads(void);
SEXP c_set_threads(SEXP);

#endif
