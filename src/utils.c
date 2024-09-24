#include "R_flint.h"

#if R_VERSION < R_Version(4, 4, 1)
void CLEAR_ATTRIB(SEXP x)
{
    SET_ATTRIB(CHK(x), R_NilValue);
    SET_OBJECT(x, 0);
    UNSET_S4_OBJECT(x);
	return;
}
#endif /* < 4.4.1 */

SEXP newObject(const char *what)
{
	SEXP class = PROTECT(R_do_MAKE_CLASS(what)),
		object = R_do_new_object(class);
	UNPROTECT(1);
	return object;
}

SEXP newBasic(const char *what, SEXPTYPE type, R_xlen_t length)
{
	SEXP s = PROTECT(newObject(what)),
		data = PROTECT(Rf_allocVector(type, length));
	s = R_do_slot_assign(s, R_flint_symbol_dot_data, data);
	UNPROTECT(2);
	return s;
}

SEXPTYPE checkType(SEXP object, SEXPTYPE *valid, const char *where)
{
	SEXPTYPE s = (SEXPTYPE) TYPEOF(object), t;
	while ((t = *(valid++)) != NILSXP)
		if (s == t)
			return t;
	ERROR_INVALID_TYPE(object, where);
	return t;
}

const char *checkClass(SEXP object, const char **valid, const char *where)
{
	int i;
	if (!Rf_isS4(object) || (i = R_check_class_etc(object, valid)) < 0)
		ERROR_INVALID_CLASS(object, where);
	return valid[i];
}

unsigned long long int asLength(SEXP length, const char *where)
{
	switch (TYPEOF(length)) {
	case INTSXP:
	{
		int tmp;
		if (XLENGTH(length) > 0 && (tmp = INTEGER(length)[0]) >= 0)
			return (unsigned long long int) tmp;
		break;
	}
	case REALSXP:
	{
		double tmp;
		if (XLENGTH(length) > 0 && !ISNAN(REAL(length)[0]) && (tmp = REAL(length)[0]) > -1.0 && tmp < 0x1.0p+64)
			return (unsigned long long int) tmp;
		break;
	}
	default:
		break;
	}
	Rf_error("invalid '%s' in '%s'", "length", where);
	return 0ull;
}

int asFlags(SEXP flags, const char *where)
{
	switch (TYPEOF(flags)) {
	case INTSXP:
		if (XLENGTH(flags) > 0)
			return INTEGER(flags)[0];
		break;
	default:
		break;
	}
	Rf_error("invalid '%s' in '%s'", "flags", where);
	return 0;
}
