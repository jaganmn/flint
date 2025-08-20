#include "flint.h"

#if R_VERSION < R_Version(4, 5, 0)
void CLEAR_ATTRIB(SEXP s)
{
	SET_ATTRIB(s, R_NilValue);
	SET_OBJECT(s, 0);
	UNSET_S4_OBJECT(s);
	return;
}
#endif

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

SEXP copyVector(SEXP object)
{
	SEXPTYPE t = (SEXPTYPE) TYPEOF(object);
	R_xlen_t j, n = XLENGTH(object);
	SEXP ans = Rf_allocVector(t, n);
	switch (t) {
	case RAWSXP:
	{
		const Rbyte *x = RAW_RO(object);
		Rbyte *y = RAW(ans);
		for (j = 0; j < n; ++j)
			y[j] = x[j];
		break;
	}
	case LGLSXP:
	{
		const int *x = LOGICAL_RO(object);
		int *y = LOGICAL(ans);
		for (j = 0; j < n; ++j)
			y[j] = x[j];
		break;
	}
	case INTSXP:
	{
		const int *x = INTEGER_RO(object);
		int *y = INTEGER(ans);
		for (j = 0; j < n; ++j)
			y[j] = x[j];
		break;
	}
	case REALSXP:
	{
		const double *x = REAL_RO(object);
		double *y = REAL(ans);
		for (j = 0; j < n; ++j)
			y[j] = x[j];
		break;
	}
	case CPLXSXP:
	{
		const Rcomplex *x = COMPLEX_RO(object);
		Rcomplex *y = COMPLEX(ans);
		for (j = 0; j < n; ++j)
			y[j] = x[j];
		break;
	}
	case STRSXP:
		for (j = 0; j < n; ++j)
			SET_STRING_ELT(ans, j, STRING_ELT(object, j));
		break;
	case VECSXP:
	case EXPRSXP:
		for (j = 0; j < n; ++j)
			SET_VECTOR_ELT(ans, j, VECTOR_ELT(object, j));
		break;
	default:
		Rf_error(_("invalid type \"%s\" in vector copy"),
		         Rf_type2char(t));
	}
	return ans;
}

mp_limb_t validLength(SEXP length, SEXP dim, mp_limb_t dft)
{
	switch ((dim != R_NilValue) << 1 | (length != R_NilValue)) {
	case 0:
		return dft;
	case 1:
	{
		SEXPTYPE t = (SEXPTYPE) TYPEOF(length);
		switch (t) {
		case INTSXP:
		case REALSXP:
			break;
		default:
			Rf_error(_("'%s' has invalid type \"%s\""),
			         "length", Rf_type2char(t));
		}
		if (XLENGTH(length) != 1)
			Rf_error(_("length of '%s' [%lld] is not %d"),
			         "length", (long long int) XLENGTH(length), 1);
		if (t == INTSXP) {
			int l = INTEGER_RO(length)[0];
			if (l == NA_INTEGER)
				Rf_error(_("'%s' is NA"), "length");
			if (l <= -1)
				Rf_error(_("'%s' is negative"), "length");
			return (mp_limb_t) l;
		} else {
			double l = REAL_RO(length)[0];
			if (ISNAN(l))
				Rf_error(_("'%s' is NA"), "length");
			if (l <= -1.0)
				Rf_error(_("'%s' is negative"), "length");
#ifdef R_FLINT_ABI_64
			if (l >= 0x1.0p+64)
#else
			if (l >= 0x1.0p+32)
#endif
				Rf_error(_("'%s' exceeds maximum %llu"),
				         "length", (unsigned long long int) UWORD_MAX);
			return (mp_limb_t) l;
		}
	}
	case 2:
	{
		R_xlen_t i, m = XLENGTH(dim);
		const int *d = INTEGER_RO(dim);
		for (i = 0; i < m; ++i)
			if (d[i] <= 0)
				return 0;
		mp_limb_t l = 1;
		for (i = 0; i < m; ++i) {
			if (d[i] > UWORD_MAX / l)
				Rf_error(_("product of '%s' exceeds maximum %llu"),
				         "dim", (unsigned long long int) UWORD_MAX);
			l *= (unsigned int) d[i];
		}
		return l;
	}
	default:
		Rf_error(_("'%s' usage and '%s' usage are mutually exclusive"),
		         "length", "dim");
		return 0;
	}
}

SEXP validDim(SEXP dim)
{
	SEXPTYPE t = (SEXPTYPE) TYPEOF(dim);
	switch (t) {
	case NILSXP:
		return R_NilValue;
	case INTSXP:
	case REALSXP:
		break;
	default:
		Rf_error(_("'%s' has invalid type \"%s\""),
		         "dim", Rf_type2char(t));
	}
	R_xlen_t i, m = XLENGTH(dim);
	if (m == 0)
		Rf_error(_("length of '%s' is %d"),
		         "dim", 0);
	if (m > INT_MAX)
		Rf_error(_("length of '%s' exceeds maximum %d"),
		         "dim", INT_MAX);
	if (t == INTSXP) {
		const int *x = INTEGER_RO(dim);
		for (i = 0; i < m; ++i) {
			if (x[i] == NA_INTEGER)
				Rf_error(_("%s[[%d]] is NA"),
				         "dim", (int) i);
			if (x[i] < 0)
				Rf_error(_("%s[[%d]] is negative"),
				         "dim", (int) i);
		}
		return (ATTRIB(dim) != R_NilValue) ? copyVector(dim) : dim;
	} else {
		const double *x = REAL_RO(dim);
		for (i = 0; i < m; ++i) {
			if (ISNAN(x[i]))
				Rf_error(_("%s[[%d]] is NA"),
				         "dim", (int) i);
			if (x[i] <= -1.0)
				Rf_error(_("%s[[%d]] is negative"),
				         "dim", (int) i);
			if (x[i] >= 0x1.0p+31)
				Rf_error(_("%s[[%d]] exeeds maximum %d"),
				         "dim", (int) i, INT_MAX);
		}
		dim = Rf_coerceVector(dim, INTSXP);
		CLEAR_ATTRIB(dim);
		return dim;
	}
}

SEXP validDimNames(SEXP dimnames, SEXP dim)
{
	if (dim == R_NilValue) {
		if (dimnames != R_NilValue)
			Rf_error(_("attempt to assign '%s' to non-array"),
			         "dimnames");
		return R_NilValue;
	}
	SEXPTYPE t = (SEXPTYPE) TYPEOF(dimnames);
	switch (t) {
	case NILSXP:
		return R_NilValue;
	case VECSXP:
		break;
	default:
		Rf_error(_("'%s' has invalid type \"%s\""),
		         "dimnames", Rf_type2char(t));
	}
	R_xlen_t i, m = XLENGTH(dim);
	if (XLENGTH(dimnames) != m)
		Rf_error(_("length of '%s' [%lld] is not equal to length of '%s' [%lld]"),
		         "dimnames", (long long int) XLENGTH(dimnames),
		         "dim"     , (long long int) m);
	const int *d = INTEGER_RO(dim);
	SEXP a = ATTRIB(dimnames);
	int new = a != R_NilValue && (CDR(a) != R_NilValue || TAG(a) != R_NamesSymbol);
	for (i = 0; i < m; ++i) {
		SEXP elt = VECTOR_ELT(dimnames, i);
		t = (SEXPTYPE) TYPEOF(elt);
		switch (t) {
		case NILSXP:
			break;
		case RAWSXP:
		case LGLSXP:
		case INTSXP:
		case REALSXP:
		case CPLXSXP:
		case STRSXP:
		case VECSXP:
		case EXPRSXP:
			if (XLENGTH(elt) != d[i])
				Rf_error(_("length of %s[[%d]] [%lld] is not equal to %s[[%d]] [%lld]"),
				         "dimnames", (int) i, (long long int) XLENGTH(elt),
				         "dim"     , (int) i, (long long int) d[i]);
			new |= (t != STRSXP || ATTRIB(elt) != R_NilValue);
			break;
		default:
			Rf_error(_("%s[[%d]] has invalid type \"%s\""),
			         "dimnames", (int) i, Rf_type2char(t));
		}
	}
	if (!new)
		return dimnames;
	SEXP ans = PROTECT(Rf_allocVector(VECSXP, m));
	for (i = 0; i < m; ++i) {
		SEXP elt = VECTOR_ELT(dimnames, i);
		t = (SEXPTYPE) TYPEOF(elt);
		switch (t) {
		case NILSXP:
			break;
		case STRSXP:
			SET_VECTOR_ELT(ans, i, (ATTRIB(elt) != R_NilValue) ? copyVector(elt) : elt);
			break;
		case INTSXP:
			if (Rf_inherits(elt, "factor")) {
			SET_VECTOR_ELT(ans, i, Rf_asCharacterFactor(elt));
			break;
			}
		default:
			elt = Rf_coerceVector(elt, STRSXP);
			CLEAR_ATTRIB(elt);
			SET_VECTOR_ELT(ans, i, elt);
		}
	}
	SEXP namesdimnames = Rf_getAttrib(dimnames, R_NamesSymbol);
	if (namesdimnames != R_NilValue) {
		PROTECT(namesdimnames);
		if (ATTRIB(namesdimnames) == R_NilValue)
			Rf_setAttrib(ans, R_NamesSymbol, namesdimnames);
		else {
			PROTECT(namesdimnames = copyVector(namesdimnames));
			Rf_setAttrib(ans, R_NamesSymbol, namesdimnames);
			UNPROTECT(1);
		}
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

SEXP validNames(SEXP names, mp_limb_t length)
{
	SEXPTYPE t = (SEXPTYPE) TYPEOF(names);
	switch (t) {
	case NILSXP:
		return R_NilValue;
	case RAWSXP:
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case VECSXP:
	case EXPRSXP:
	{
		mp_limb_t m = (mp_limb_t) XLENGTH(names);
		if (m != length)
			Rf_error(_("length of '%s' [%llu] is not equal to length of object [%llu]"),
			         "names", (unsigned long long int) m,
			                  (unsigned long long int) length);
		break;
	}
	default:
		Rf_error(_("'%s' has invalid type \"%s\""),
		         "names", Rf_type2char(t));
	}
	switch (t) {
	case STRSXP:
		return (ATTRIB(names) != R_NilValue) ? copyVector(names) : names;
	case INTSXP:
		if (Rf_inherits(names, "factor"))
		return Rf_asCharacterFactor(names);
	default:
		names = Rf_coerceVector(names, STRSXP);
		CLEAR_ATTRIB(names);
		return names;
	}
}

void setDDNN(SEXP s, SEXP dim, SEXP dimnames, SEXP names)
{
	SEXP (*sets)(SEXP, SEXP, SEXP) =
		(TYPEOF(s) == OBJSXP) ? &R_do_slot_assign : &Rf_setAttrib;
	if (dim != R_NilValue) {
		sets(s, R_DimSymbol, dim);
		if (dimnames != R_NilValue)
			sets(s, R_DimNamesSymbol, dimnames);
	}
	if (names != R_NilValue)
		sets(s, R_NamesSymbol, names);
	return;
}

void setDDNN2(SEXP s, SEXP x, SEXP y,
              mp_limb_t ns, mp_limb_t nx, mp_limb_t ny)
{
	SEXP (*getx)(SEXP, SEXP) =
		(TYPEOF(x) == OBJSXP) ? &R_do_slot        : &Rf_getAttrib;
	SEXP (*gety)(SEXP, SEXP) =
		(TYPEOF(y) == OBJSXP) ? &R_do_slot        : &Rf_getAttrib;
	SEXP (*sets)(SEXP, SEXP, SEXP) =
		(TYPEOF(s) == OBJSXP) ? &R_do_slot_assign : &Rf_setAttrib;
	SEXP ax, ay;
	PROTECT(ax = getx(x, R_DimSymbol));
	PROTECT(ay = gety(y, R_DimSymbol));
	if (ax != R_NilValue && (ny > 0 || ay != R_NilValue)) {
		sets(s, R_DimSymbol, ax);
		if ((ax = getx(x, R_DimNamesSymbol)) != R_NilValue) {
			PROTECT(ax);
			sets(s, R_DimNamesSymbol, ax);
			UNPROTECT(1);
		}
	} else
	if (ay != R_NilValue && (nx > 0 || ax != R_NilValue)) {
		sets(s, R_DimSymbol, ay);
		if ((ay = gety(y, R_DimNamesSymbol)) != R_NilValue) {
			PROTECT(ay);
			sets(s, R_DimNamesSymbol, ay);
			UNPROTECT(1);
		}
	}
	UNPROTECT(2);
	ax = ay = NULL;
	if ((ns == nx && (ax = getx(x, R_NamesSymbol)) != R_NilValue) ||
	    (ns == ny && (ay = gety(y, R_NamesSymbol)) != R_NilValue)) {
		PROTECT((ax) ? ax : ay);
		sets(s, R_NamesSymbol, (ax) ? ax : ay);
		UNPROTECT(1);
	}
	return;
}

void setDDNN1(SEXP s, SEXP x)
{
	SEXP (*getx)(SEXP, SEXP) =
		(TYPEOF(x) == OBJSXP) ? &R_do_slot        : &Rf_getAttrib;
	SEXP (*sets)(SEXP, SEXP, SEXP) =
		(TYPEOF(s) == OBJSXP) ? &R_do_slot_assign : &Rf_setAttrib;
	SEXP a;
	if ((a = getx(x, R_DimSymbol)) != R_NilValue) {
		PROTECT(a);
		sets(s, R_DimSymbol, a);
		UNPROTECT(1);
		if ((a = getx(x, R_DimNamesSymbol)) != R_NilValue) {
			PROTECT(a);
			sets(s, R_DimNamesSymbol, a);
			UNPROTECT(1);
		}
	}
	if ((a = getx(x, R_NamesSymbol)) != R_NilValue) {
		PROTECT(a);
		sets(s, R_NamesSymbol, a);
		UNPROTECT(1);
	}
	return;
}

void checkConformable(SEXP x, SEXP y, mp_limb_t nx, mp_limb_t ny)
{
	SEXP (*getx)(SEXP, SEXP) =
		(TYPEOF(x) == OBJSXP) ? &R_do_slot : &Rf_getAttrib;
	SEXP (*gety)(SEXP, SEXP) =
		(TYPEOF(y) == OBJSXP) ? &R_do_slot : &Rf_getAttrib;
	SEXP ax, ay;
	R_xlen_t n;
	PROTECT(ax = getx(x, R_DimSymbol));
	PROTECT(ay = gety(y, R_DimSymbol));
	if (ax != R_NilValue && ay != R_NilValue &&
	    (nx != ny || (n = XLENGTH(ax)) != XLENGTH(ay) ||
	     (n > 0 && memcmp(INTEGER_RO(ax), INTEGER_RO(ay), sizeof(int) * (size_t) n) != 0)))
		Rf_error(_("non-conformable arrays"));
	if ((ax != R_NilValue && ny > nx) || (ay != R_NilValue && nx > ny))
		/* NB: stricter than R which allows if array length is zero */
		Rf_error(_("non-array length exceeds array length"));
	if (nx > 0 && ny > 0 && ((nx < ny) ? ny % nx : nx % ny))
		Rf_warning(_("longer object length is not a multiple of shorter object length"));
	UNPROTECT(2);
	return;
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
