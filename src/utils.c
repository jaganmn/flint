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
		const int *s = INTEGER_RO(length);
		if (XLENGTH(length) >= 1 && s[0] != NA_INTEGER && s[0] > -1)
			return (unsigned long long int) s[0];
		break;
	}
	case REALSXP:
	{
		const double *s = REAL_RO(length);
		if (XLENGTH(length) >= 1 && !ISNAN(s[0]) && s[0] > -1.0 && s[0] < 0x1.0p+64)
			return (unsigned long long int) s[0];
		break;
	}
	}
	Rf_error(_("invalid '%s' in '%s'"), "length", where);
	return 0ull;
}

int asPrec(SEXP prec, const char *where)
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
		if (XLENGTH(prec) >= 1 && !ISNAN(s[0]) && s[0] >= 1.0 && s[0] < 0x1.0p+31)
			return (int) s[0];
		break;
	}
	}
	Rf_error(_("invalid '%s' in '%s'"), "prec", where);
	return 0;
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

void  ucopy(unsigned int *uu, const unsigned long long int *u)
{
	uu[0] = (unsigned int) (u[0] & 0x00000000FFFFFFFFu);
	uu[1] = (unsigned int) (u[0] >> (sizeof(int) * CHAR_BIT));
	return;
}

void uucopy(unsigned long long int *u, const unsigned int *uu)
{
	u[0] = (unsigned long long int) uu[1] << (sizeof(int) * CHAR_BIT) |
		(unsigned long long int) uu[0];
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
