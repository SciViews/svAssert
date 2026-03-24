#include "any_finite.h"
#include "threads.h"

const R_xlen_t n_min_para = 1e6;

static Rboolean any_infinite_double(SEXP x) {
    const double *xp = REAL_RO(x);
    const R_xlen_t n = xlength(x);
    int found = 0;

    if (n_threads > 1 && n >= n_min_para){
        #pragma omp parallel for num_threads(n_threads) shared(found) schedule(static)
        for (R_xlen_t i = 0; i < n; i++) {
            int local_found;
            #pragma omp atomic read
            local_found = found;
            if (local_found) continue;
            if (xp[i] == R_PosInf || xp[i] == R_NegInf) {
               #pragma omp atomic write
               found = 1;
            }
        }
    } else {
        for (R_xlen_t i = 0; i < n; i++) {
            if (xp[i] == R_PosInf || xp[i] == R_NegInf) {
               found = 1;
               break;
            }
        }
    }
    
    return found ? TRUE : FALSE;
}

static Rboolean any_infinite_complex(SEXP x) {
    const Rcomplex *xp = COMPLEX_RO(x);
    const R_xlen_t n = xlength(x);
    int found = 0;

    if (n_threads > 1 && n >= n_min_para){
        #pragma omp parallel for num_threads(n_threads) shared(found) schedule(static)
        for (R_xlen_t i = 0; i < n; i++) {
            int local_found;
            #pragma omp atomic read
            local_found = found;
            if (local_found) continue;
            if (xp[i].r == R_PosInf || xp[i].r == R_NegInf ||
                xp[i].i == R_PosInf || xp[i].i == R_NegInf) {
                #pragma omp atomic write
                found = 1;
            }
        }
    } else {
        for (R_xlen_t i = 0; i < n; i++) {
            if (xp[i].r == R_PosInf || xp[i].r == R_NegInf ||
                xp[i].i == R_PosInf || xp[i].i == R_NegInf) {
               found = 1;
               break;
            }
        }
    }

    return found ? TRUE : FALSE;
}

static Rboolean any_infinite_list(SEXP x) {
    const R_xlen_t n = xlength(x);
    for (R_xlen_t i = 0; i < n; i++) {
        if (any_infinite(VECTOR_ELT(x, i)))
            return TRUE;
    }
    return FALSE;
}

Rboolean any_infinite(SEXP x) {
    switch (TYPEOF(x)) {
    case REALSXP:
        return any_infinite_double(x);
    case INTSXP:
    case LGLSXP:
        return FALSE;
    case CPLXSXP:
        return any_infinite_complex(x);
    case VECSXP:
        return any_infinite_list(x);
    default:
        error("any_infinite(): unsupported type %s", Rf_type2char(TYPEOF(x)));
    }
}

SEXP c_any_infinite(SEXP x) {
    return ScalarLogical(any_infinite(x));
}
