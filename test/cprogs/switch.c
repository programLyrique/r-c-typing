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

RBoolean isInteger(SEXP a) {
    switch (TYPEOF(s))
    {
    case INTSXP:
        return TRUE;
        break;
    
    default:
        return FALSE;
        break;
    }
}