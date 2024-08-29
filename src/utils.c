#include "R_flint.h"

SEXP R_flint_bits(void)
{
	return ScalarInteger(FLINT_BITS);
}

SEXP newObject(const char *what)
{
	SEXP class = PROTECT(R_do_MAKE_CLASS(what)),
		object = R_do_new_object(class);
	UNPROTECT(1);
	return object;
}

void assertClass(SEXP object, const char *what, const char *where)
{
	const char *valid[] = { what, "" };
	if (TYPEOF(object) != OBJSXP)
		Rf_error("invalid type \"%s\" in '%s'",
		         type2char((SEXPTYPE) TYPEOF(from)), where);
	if (R_check_class_etc(object, valid) < 0)
		Rf_error("invalid class \"%s\" in '%s'",
		         CHAR(STRING_ELT(getAttrib(from, R_ClassSymbol), 0)), where);
	return;
}

int asFlags(SEXP x)
{
	switch (TYPEOF(x)) {
	case INTSXP:
		if (XLENGTH(x) > 0)
			return INTEGER(x)[0];
		break;
	default:
		break;
	}
	Rf_error("invalid flags");
	return 0;
}

slong asPrec(SEXP x)
{
	switch (TYPEOF(x)) {
	case INTSXP:
	{
		int tmp;
		if (XLENGTH(x) > 0 && (tmp = INTEGER(x)[0]) != NA_INTEGER &&
		    tmp >= 0 && tmp <= WORD_MAX)
			return (slong) tmp;
		break;
	}
	case REALSXP:
	{
		double tmp;
		if (XLENGTH(x) > 0 && !ISNAN(tmp = REAL(x)[0]) &&
		    tmp >= 0 && tmp <= WORD_MAX)
			return (slong) tmp;
		break;
	}
	default:
		break;
	}
	Rf_error("invalid precision");
	return 0;
}
