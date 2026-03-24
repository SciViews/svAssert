# include "threads.h"

int n_threads = 1;

SEXP c_get_threads(void) {
    return ScalarInteger(n_threads);
}

SEXP c_set_threads(SEXP n) {
    n_threads = asInteger(n);
    return ScalarInteger(n_threads);
}
