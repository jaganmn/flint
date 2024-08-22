#include "R_flint.h"

SEXP R_flint_length(SEXP s_x)
{
	SEXP length = R_do_slot(s_x, R_flint_symbol_length);
	unsigned int *u = (unsigned int *) INTEGER(length);
	unsigned long long int n = 0;
	if (XLENGTH(length) == 1)
		n = u[0];
	else if (XLENGTH(length) == 2)
		n = u[1] << (sizeof(int) * CHAR_BIT) | u[0];
	else {
		Rf_error("invalid '%s' slot", "length");
		return R_NilValue;
	}
	SEXP ans;
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
