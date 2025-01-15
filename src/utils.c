#include "flint.h"

#if R_VERSION < R_Version(4, 5, 0)
void CLEAR_ATTRIB(SEXP x)
{
	SET_ATTRIB(x, R_NilValue);
	SET_OBJECT(x, 0);
	UNSET_S4_OBJECT(x);
	return;
}
#endif /* < 4.5.0 */

char *R_alloc_snprintf(size_t n, const char *format, ...)
{
	char *buffer = R_alloc(n, sizeof(char));
	va_list args;
	va_start(args, format);
	vsnprintf(buffer, n, format, args);
	va_end(args);
	return buffer;
}

SEXP newObject(const char *what)
{
	SEXP class = PROTECT(R_do_MAKE_CLASS(what)),
		object = R_do_new_object(class);
	UNPROTECT(1);
	return object;
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

mp_limb_t asLength(SEXP length, const char *where)
{
	switch (TYPEOF(length)) {
	case INTSXP:
	{
		const int *s = INTEGER_RO(length);
		if (XLENGTH(length) >= 1 && s[0] != NA_INTEGER && s[0] > -1)
			return (mp_limb_t) s[0];
		break;
	}
	case REALSXP:
	{
		const double *s = REAL_RO(length);
		if (XLENGTH(length) >= 1 && !ISNAN(s[0]) && s[0] > -1.0 &&
#ifdef R_FLINT_ABI_64
		    s[0] < 0x1.0p+64)
#else
		    s[0] < 0x1.0p+32)
#endif
			return (mp_limb_t) s[0];
		break;
	}
	}
	Rf_error(_("invalid '%s' in '%s'"), "length", where);
	return 0ull;
}

mpfr_prec_t asPrec(SEXP prec, const char *where)
{
	if (prec == R_NilValue) {
		static SEXP tag = NULL;
		if (!tag)
			tag = Rf_install("flint.prec");
		prec = Rf_GetOption1(tag);
		if (prec == R_NilValue)
			return DBL_MANT_DIG;
	}
	switch (TYPEOF(prec)) {
	case INTSXP:
	{
		const int *s = INTEGER_RO(prec);
		if (XLENGTH(prec) >= 1 && s[0] >= 1)
			return s[0];
		break;
	}
	case REALSXP:
	{
		const double *s = REAL_RO(prec);
		if (XLENGTH(prec) >= 1 && !ISNAN(s[0]) && s[0] >= 1.0 &&
#if SIZEOF_MPFR_PREC_T == 8
		    s[0] < 0x1.0p+63)
#else
		    s[0] < 0x1.0p+31)
#endif
			return (mpfr_prec_t) s[0];
		break;
	}
	}
	Rf_error(_("invalid '%s' in '%s'"), "prec", where);
	return 0;
}

mpfr_rnd_t asRnd(SEXP rnd, const char *where)
{
	if (rnd == R_NilValue) {
		static SEXP tag = NULL;
		if (!tag)
			tag = Rf_install("flint.rnd");
		rnd = Rf_GetOption1(tag);
		if (rnd == R_NilValue)
			return MPFR_RNDN;
	}
	if (TYPEOF(rnd) == STRSXP && XLENGTH(rnd) > 0 &&
	    (rnd = STRING_ELT(rnd, 0)) != NA_STRING) {
		switch (CHAR(rnd)[0]) {
		case 'N': case 'n':
			return MPFR_RNDN;
		case 'Z': case 'z':
			return MPFR_RNDZ;
		case 'U': case 'u':
			return MPFR_RNDU;
		case 'D': case 'd':
			return MPFR_RNDD;
		case 'A': case 'a':
			return MPFR_RNDA;
		}
	}
	Rf_error(_("invalid '%s' in '%s'"), "rnd", where);
	return -1;
}

int asBase(SEXP base, const char *where)
{
	switch (TYPEOF(base)) {
	case INTSXP:
	{
		const int *s = INTEGER_RO(base);
		if (XLENGTH(base) >= 1 && s[0] >= 2 && s[0] < 63)
			return (s[0] < 37) ? -s[0] : s[0];
		break;
	}
	case REALSXP:
	{
		const double *s = REAL_RO(base);
		if (XLENGTH(base) >= 1 && !ISNAN(s[0]) && s[0] >= 2.0 && s[0] < 63.0)
			return (s[0] < 37.0) ? -(int) s[0] : (int) s[0];
		break;
	}
	}
	Rf_error(_("invalid '%s' in '%s'"), "base", where);
	return 0;
}

size_t asDigits(SEXP digits, const char *where)
{
	switch (TYPEOF(digits)) {
	case INTSXP:
	{
		const int *s = INTEGER_RO(digits);
		if (XLENGTH(digits) >= 1 && s[0] > -1)
			return (size_t) s[0];
		break;
	}
	case REALSXP:
	{
		const double *s = REAL_RO(digits);
		if (XLENGTH(digits) >= 1 && !ISNAN(s[0]) && s[0] > -1.0 && s[0] < 0x1.0p+31)
			return (size_t) s[0];
		break;
	}
	}
	Rf_error(_("invalid '%s' in '%s'"), "digits", where);
	return 0u;
}

const char *asSep(SEXP sep, const char *where)
{
	switch (TYPEOF(sep)) {
	case STRSXP:
	{
		if (XLENGTH(sep) >= 1 && (sep = STRING_ELT(sep, 0)) != NA_STRING &&
		    CHAR(sep)[0] != '\0')
			return CHAR(sep);
		break;
	}
	}
	Rf_error(_("invalid '%s' in '%s'"), "sep", where);
	return (const char *) 0;
}

void  ucopy(unsigned int *uu, const mp_limb_t *u)
{
#ifdef R_FLINT_ABI_64
	uu[0] = (unsigned int) (u[0] & 0x00000000FFFFFFFFu);
	uu[1] = (unsigned int) (u[0] >> 32);
#else
	uu[0] = (unsigned int) u[0];
#endif
	return;
}

void uucopy(mp_limb_t *u, const unsigned int *uu)
{
#ifdef R_FLINT_ABI_64
	u[0] = (mp_limb_t) uu[1] << 32 | (mp_limb_t) uu[0];
#else
	u[0] = (mp_limb_t) uu[0];
#endif
	return;
}

size_t strmatch(const char *s, const char **ss)
{
	size_t pos = 0;
	while (1) {
		if (!ss[pos])
			return 0;
		if (!strcmp(s, ss[pos]))
			return pos + 1;
		pos += 1;
	}
	return 0;
}
