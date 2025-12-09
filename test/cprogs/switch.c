#include <Rinternals.h>


RBoolean isInteger2(SEXP a) {
    switch (TYPEOF(a))
    {
    case INTSXP:
        return TRUE;
        break;

    default:
        return FALSE;
        break;
    }
 }

RBoolean isInteger3(SEXP a) {
    if(TYPEOF(a) == INTSXP) {
        return TRUE;
    } else {
        return FALSE;
    }
}

RBoolean checkList(SEXP s) {
    if(TYPEOF(s) == VECSXP ) {
        return TRUE;
    } else {
        return FALSE;
    }
}

Rboolean isVectorList(SEXP s)
{
    switch (TYPEOF(s)) {
    case VECSXP:
    case EXPRSXP:
	return TRUE;
    default:
	return FALSE;
    }
}

