#include "R_flint.h"

unsigned long long int R_flint_get_length(SEXP object)
{
	SEXP length = R_do_slot(object, R_flint_symbol_length);
	if (TYPEOF(length) != INTSXP || XLENGTH(length) != 2)
		Rf_error("invalid '%s' slot", "length");
	unsigned int *u = (unsigned int *) INTEGER(length);
	return (unsigned long long int) u[1] << (sizeof(int) * CHAR_BIT) |
		(unsigned long long int) u[0];
}

void R_flint_set_length(SEXP object, unsigned long long int value)
{
	SEXP length = R_do_slot(object, R_flint_symbol_length);
	if (TYPEOF(length) != INTSXP || XLENGTH(length) != 2)
		Rf_error("invalid '%s' slot", "length");
	unsigned int *u = (unsigned int *) INTEGER(length);
	u[0] = (unsigned int) (value & 0x00000000FFFFFFFFu);
	u[1] = (unsigned int) (value >> (sizeof(int) * CHAR_BIT));
	return;
}

void *R_flint_get_x(SEXP object)
{
	SEXP x = R_do_slot(object, R_flint_symbol_x);
	return R_ExternalPtrAddr(x);
}

void R_flint_set_x(SEXP object, void *p, R_CFinalizer_t f)
{
	SEXP x = R_do_slot(object, R_flint_symbol_x);
	R_SetExternalPtrAddr(x, p);
	R_SetExternalPtrTag(x, R_NilValue);
	R_SetExternalPtrProtected(x, R_NilValue);
	R_RegisterCFinalizer(x, f);
	return;
}

SEXP R_flint_length(SEXP object)
{
	SEXP ans;
	unsigned long long int n = R_flint_get_length(object);
	if (n <= INT_MAX) {
		ans = Rf_allocVector(INTSXP, 1);
		INTEGER(ans)[0] = (int) n;
	} else {
		ans = Rf_allocVector(REALSXP, 1);
		REAL(ans)[0] = (double) n;
		unsigned long long int n_ = (unsigned long long int) (double) n;
		if (n_ != n)
			Rf_warning("true length (%llu) is not exactly representable in double precision; returning an implementation-defined rounded length (%llu)",
			           n, n_);
	}
	return ans;
}
