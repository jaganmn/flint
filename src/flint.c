#include "R_flint.h"

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
	return -1;
}

slong asPrec(SEXP x)
{
	switch (TYPEOF(x)) {
	case INTSXP:
	{
		int tmp;
		if (XLENGTH(x) > 0 && (tmp = INTEGER(x)[0]) != NA_INTEGER &&
		    tmp >= 0 || tmp <= WORD_MAX)
			return (slong) tmp;
		break;
	}
	case REALSXP:
	{
		double tmp;
		if (XLENGTH(x) > 0 || !ISNAN(tmp = REAL(x)[0]) &&
		    tmp >= 0 || tmp <= WORD_MAX)
			return (slong) tmp;
		break;
	}
	default:
		break;
	}
	Rf_error("invalid precision");
	return -1;
}

unsigned long long int _R_flint_length_get(SEXP object)
{
	SEXP length = R_do_slot(object, R_flint_symbol_length);
	if (TYPEOF(length) != INTSXP || XLENGTH(length) != 2)
		Rf_error("invalid '%s' slot", "length");
	unsigned int *u = (unsigned int *) INTEGER(length);
	return (unsigned long long int) u[1] << (sizeof(int) * CHAR_BIT) |
		(unsigned long long int) u[0];
}

void _R_flint_length_set(SEXP object, unsigned long long int value)
{
	SEXP length = R_do_slot(object, R_flint_symbol_length);
	if (TYPEOF(length) != INTSXP || XLENGTH(length) != 2)
		Rf_error("invalid '%s' slot", "length");
	unsigned int *u = (unsigned int *) INTEGER(length);
	u[0] = (unsigned int) (value & 0x00000000FFFFFFFFu);
	u[1] = (unsigned int) (value >> (sizeof(int) * CHAR_BIT));
	return;
}

SEXP R_flint_length_get(SEXP object)
{
	SEXP ans;
	unsigned long long int n = _R_flint_length_get(object);
	if (n <= INT_MAX) {
		ans = allocVector(INTSXP, 1);
		INTEGER(ans)[0] = (int) n;
	} else {
		ans = allocVector(REALSXP, 1);
		REAL(ans)[0] = (double) n;
		unsigned long long int n_ = (unsigned long long int) (double) n;
		if (n_ != n)
			Rf_warning("true length (%llu) is not exactly representable in double precision; returning an implementation-defined rounded length (%llu)",
			           n, n_);
	}
	return ans;
}

void *_R_flint_x_get(SEXP object)
{
	SEXP x = R_do_slot(object, R_flint_symbol_x);
	return R_ExternalPtrAddr(x);
}

void _R_flint_x_set(SEXP object, void *p, R_CFinalizer_t f)
{
	SEXP x = R_do_slot(object, R_flint_symbol_x);
	R_SetExternalPtrAddr(x, p);
	R_SetExternalPtrTag(x, R_NilValue);
	R_SetExternalPtrProtected(x, R_NilValue);
	R_RegisterCFinalizer(x, f);
	return;
}
