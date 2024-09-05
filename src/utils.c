#include "R_flint.h"

SEXP R_flint_bits(void)
{
	return Rf_ScalarInteger(FLINT_BITS);
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
	if (TYPEOF(object) != S4SXP)
		ERROR_INVALID_TYPE(object, where);
	if (R_check_class_etc(object, valid) < 0)
		ERROR_INVALID_CLASS(object, where);
	return;
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
