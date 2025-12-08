#include <Rinternals.h>

// Rboolean isVectorList(SEXP s)
// {
//     switch (TYPEOF(s)) {
//     case VECSXP:
//     case EXPRSXP:
// 	return TRUE;
//     default:
// 	return FALSE;
//     }
// }

RBoolean isInteger2(SEXP a) {
    switch (TYPEOF(a))
    {
    case INTSXP:
        return TRUE;
        break;
    
    default:
        error("Input must be an integer vector");
        break;
    }
}

RBoolean isInteger3(SEXP a) {
    if(TYPEOF(a) == INTSXP) {
        return TRUE;
    } else {
        error("Input must be an integer vector");
    }
}