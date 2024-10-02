#include <gmp.h>
#include <mpfr.h>
#include <flint/flint.h>
#include <Rinternals.h>
#include <Rversion.h>
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

SEXP R_flint_bits(void)
{
	return Rf_ScalarInteger(FLINT_BITS);
}

SEXP R_flint_version(void)
{
	SEXP ans, nms;
	PROTECT(ans = Rf_allocVector(INTSXP, 4));
	INTEGER(ans)[0] = R_Version(PACKAGE_VERSION_MAJOR,
	                            PACKAGE_VERSION_MINOR,
	                            PACKAGE_VERSION_PATCHLEVEL);
	INTEGER(ans)[1] = R_Version(__FLINT_VERSION,
	                            __FLINT_VERSION_MINOR,
	                            __FLINT_VERSION_PATCHLEVEL);
	INTEGER(ans)[2] = R_Version(MPFR_VERSION_MAJOR,
	                            MPFR_VERSION_MINOR,
	                            MPFR_VERSION_PATCHLEVEL);
	INTEGER(ans)[3] = R_Version(__GNU_MP_VERSION,
	                            __GNU_MP_VERSION_MINOR,
	                            __GNU_MP_VERSION_PATCHLEVEL);
	PROTECT(nms = Rf_allocVector(STRSXP, 4));
	SET_STRING_ELT(nms, 0, Rf_mkChar("package"));
	SET_STRING_ELT(nms, 1, Rf_mkChar("flint"));
	SET_STRING_ELT(nms, 2, Rf_mkChar("mpfr"));
	SET_STRING_ELT(nms, 3, Rf_mkChar("gmp"));
	Rf_setAttrib(ans, R_NamesSymbol, nms);
	UNPROTECT(2);
	return ans;
}
