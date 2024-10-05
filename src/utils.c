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
	char *buf = R_alloc(n, sizeof(char));
	va_list args;
	va_start(args, format);
	vsnprintf(buf, n, format, args);
	va_end(args);
	return buf;
}

void ucopy(unsigned long long int *u, unsigned int *uu, int from)
{
	if (from != 0) {
		/* uu -> u */
		u[0] = (unsigned long long int) uu[1] << (sizeof(int) * CHAR_BIT) |
			(unsigned long long int) uu[0];
	} else {
		/* u -> uu */
		uu[0] = (unsigned int) (u[0] & 0x00000000FFFFFFFFu);
		uu[1] = (unsigned int) (u[0] >> (sizeof(int) * CHAR_BIT));
	}
	return;
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
		int *s = INTEGER(length);
		if (XLENGTH(length) >= 1 && s[0] != NA_INTEGER && s[0] >= 0)
			return (unsigned long long int) s[0];
		break;
	}
	case REALSXP:
	{
		double *s = REAL(length);
		if (XLENGTH(length) >= 1 && !ISNAN(s[0]) && s[0] > -1.0 && s[0] < 0x1.0p+64)
			return (unsigned long long int) s[0];
		break;
	}
	}
	Rf_error(_("invalid '%s' in '%s'"), "length", where);
	return 0ull;
}

int asBase(SEXP base, const char *where)
{
	switch (TYPEOF(base)) {
	case INTSXP:
	{
		int *s = INTEGER(base);
		if (XLENGTH(base) >= 1 &&
		    ((s[0] >= -36 && s[0] <= -2) || (s[0] >= 2 && s[0] <= 62)))
			return (s[0] >= 2 && s[0] <= 36) ? -s[0] : s[0];
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
		int *s = INTEGER(digits);
		if (XLENGTH(digits) >= 1 && s[0] >= 0)
			return (size_t) s[0];
	}
	}
	Rf_error(_("invalid '%s' in '%s'"), "digits", where);
	return 0u;
}
