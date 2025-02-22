#include "flint.h"

SEXP R_flint_abi(void)
{
#ifdef R_FLINT_ABI_64
	return Rf_ScalarInteger(64);
#else
	return Rf_ScalarInteger(32);
#endif
}

SEXP R_flint_bits_per_limb(void)
{
	return Rf_ScalarInteger(mp_bits_per_limb);
}

SEXP R_flint_long_long_limb(void)
{
#ifdef R_FLINT_ABI_LL
	return Rf_ScalarLogical(1);
#else
	return Rf_ScalarLogical(0);
#endif
}

SEXP R_flint_version(void)
{
	int libflint[3], libmpfr[3], libgmp[3];
	sscanf(flint_version,
	       "%d.%d.%d", &libflint[0], &libflint[1], &libflint[2]);
	sscanf(mpfr_get_version(),
	       "%d.%d.%d", &libmpfr[0], &libmpfr[1], &libmpfr[2]);
	sscanf(gmp_version,
	       "%d.%d.%d", &libgmp[0], &libgmp[1], &libgmp[2]);
	SEXP ans = PROTECT(Rf_allocVector(INTSXP, 7));
	INTEGER(ans)[0] = R_Version(PACKAGE_VERSION_MAJOR,
	                            PACKAGE_VERSION_MINOR,
	                            PACKAGE_VERSION_PATCHLEVEL);
	INTEGER(ans)[1] = R_Version(__FLINT_VERSION,
	                            __FLINT_VERSION_MINOR,
	                            __FLINT_VERSION_PATCHLEVEL);
	INTEGER(ans)[2] = R_Version(libflint[0], libflint[1], libflint[2]);
	INTEGER(ans)[3] = R_Version(MPFR_VERSION_MAJOR,
	                            MPFR_VERSION_MINOR,
	                            MPFR_VERSION_PATCHLEVEL);
	INTEGER(ans)[4] = R_Version(libmpfr[0], libmpfr[1], libmpfr[2]);
	INTEGER(ans)[5] = R_Version(__GNU_MP_VERSION,
	                            __GNU_MP_VERSION_MINOR,
	                            __GNU_MP_VERSION_PATCHLEVEL);
	INTEGER(ans)[6] = R_Version(libgmp[0], libgmp[1], libgmp[2]);
	SEXP nms = PROTECT(Rf_allocVector(STRSXP, 7));
	SET_STRING_ELT(nms, 0, Rf_mkChar("package"));
	SET_STRING_ELT(nms, 1, Rf_mkChar("flint.h"));
	SET_STRING_ELT(nms, 2, Rf_mkChar("libflint"));
	SET_STRING_ELT(nms, 3, Rf_mkChar("mpfr.h"));
	SET_STRING_ELT(nms, 4, Rf_mkChar("libmpfr"));
	SET_STRING_ELT(nms, 5, Rf_mkChar("gmp.h"));
	SET_STRING_ELT(nms, 6, Rf_mkChar("libgmp"));
	Rf_setAttrib(ans, R_NamesSymbol, nms);
	UNPROTECT(2);
	return ans;
}
