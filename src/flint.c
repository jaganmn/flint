#include <flint/flint.h>
#include <flint/fmpz.h>
#include <flint/fmpq.h>
#include <flint/arf.h>
#include <flint/mag.h>
#include <flint/arb.h>
#include <flint/acb.h>
#include "flint.h"

void R_flint_abort(void)
{
	Rf_error(_("caught exception in libflint"));
	return;
}

void *R_flint_get_pointer(SEXP object)
{
	SEXP x = R_do_slot(object, R_flint_symbol_dot_xdata);
	void *p = R_ExternalPtrAddr(x);
	return p;
}

unsigned long long int R_flint_get_length(SEXP object)
{
	SEXP x = R_do_slot(object, R_flint_symbol_dot_xdata),
		length = R_ExternalPtrProtected(x);
	unsigned long long int n;
	uucopy(&n, (unsigned int *) INTEGER(length));
	return n;
}

R_flint_class_t R_flint_get_class(SEXP object)
{
	int i = (TYPEOF(object) == OBJSXP)
		? R_check_class_etc(object, R_flint_classes) : -1;
	return (R_flint_class_t) i;
}

void R_flint_set(SEXP object,
                 void *p, unsigned long long int n, R_CFinalizer_t f)
{
	SEXP length = PROTECT(Rf_allocVector(INTSXP, 2));
	ucopy((unsigned int *) INTEGER(length), &n);
	SEXP x = PROTECT(R_MakeExternalPtr(p, R_NilValue, length));
	R_RegisterCFinalizer(x, f);
	R_do_slot_assign(object, R_flint_symbol_dot_xdata, x);
	UNPROTECT(2);
	return;
}

SEXP R_flint_class(SEXP object)
{
	int i = (TYPEOF(object) == OBJSXP)
		? R_check_class_etc(object, R_flint_classes) : -1;
	SEXP ans = Rf_allocVector(STRSXP, 1);
	SET_STRING_ELT(ans, 0, (i < 0) ? NA_STRING : Rf_mkChar(R_flint_classes[i]));
	return ans;
}

SEXP R_flint_new(SEXP class)
{
	return newObject(CHAR(STRING_ELT(class, 0)));
}

SEXP R_flint_valid(SEXP object)
{
	SEXP x = R_do_slot(object, R_flint_symbol_dot_xdata),
		length = R_ExternalPtrProtected(x);
#define INVALID(...) Rf_mkString(R_alloc_snprintf(255, __VA_ARGS__))
	if (TYPEOF(length) != INTSXP)
		return INVALID(_("type of protected field is not \"%s\""), "integer");
	if (XLENGTH(length) != 2)
		return INVALID(_("length of protected field is not %d"), 2);
	int length0 = INTEGER_RO(length)[0] == 0 && INTEGER_RO(length)[1] == 0;
	if ((R_ExternalPtrAddr(x) == 0) != length0)
		return INVALID((length0)
		               ? _("length is zero and pointer field is non-zero")
		               : _("length is non-zero and pointer field is zero"));
#undef INVALID
	return Rf_ScalarLogical(1);
}

SEXP R_flint_length(SEXP object)
{
	SEXP ans;
	unsigned long long int n = R_flint_get_length(object);
	if (n <= INT_MAX) {
		ans = Rf_allocVector(INTSXP, 1);
		INTEGER(ans)[0] = (int) n;
	} else {
		unsigned long long int n_ = (unsigned long long int) (double) n;
		if (n_ >  n)
			n_ = (unsigned long long int) nextafter((double) n, 0.0);
		if (n_ != n)
			Rf_warning(_("true length %llu truncated to %llu"), n, n_);
		ans = Rf_allocVector(REALSXP, 1);
		REAL(ans)[0] = (double) n_;
	}
	return ans;
}

SEXP R_flint_triple(SEXP object)
{
	SEXP ans = PROTECT(Rf_allocVector(STRSXP, 3));
	char buffer[64];
	snprintf(buffer, 64,   "%s", R_flint_classes[R_flint_get_class  (object)]);
	SET_STRING_ELT(ans, 0, Rf_mkChar(buffer));
	snprintf(buffer, 64, "%llu",                 R_flint_get_length (object)) ;
	SET_STRING_ELT(ans, 1, Rf_mkChar(buffer));
	snprintf(buffer, 64,   "%p",                 R_flint_get_pointer(object)) ;
	SET_STRING_ELT(ans, 2, Rf_mkChar(buffer));
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_part(SEXP from, SEXP s_mode)
{
	R_flint_class_t class = R_flint_get_class(from);
	unsigned long long int j, n = R_flint_get_length(from);
	void *p = R_flint_get_pointer(from);
	R_CFinalizer_t f;
	const char *what;
	int mode = INTEGER_RO(s_mode)[0];

#define PART_CASE(xname, yname, xelt_t, yelt_t, xptr_t, yptr_t, part) \
	do { \
		xptr_t x = (xptr_t) p; \
		yptr_t y = (yptr_t) ((n) ? flint_calloc((size_t) n, sizeof(yelt_t)) : 0); \
		for (j = 0; j < n; ++j) \
			yname##_set(y + j, xname##_##part##ref(x + j)); \
		p = (void *) y; \
		f = (R_CFinalizer_t) &R_flint_##yname##_finalize; \
		what = #yname; \
	} while (0)

	switch (class) {
	case R_FLINT_CLASS_FMPQ:
	{
		if (mode == 0)
		PART_CASE(fmpq, fmpz, fmpq, fmpz, fmpq *, fmpz *, num);
		else
		PART_CASE(fmpq, fmpz, fmpq, fmpz, fmpq *, fmpz *, den);
		break;
	}
	case R_FLINT_CLASS_ARB:
	{
		if (mode == 0)
		PART_CASE(arb, arf, arb_t, arf_t, arb_ptr, arf_ptr, mid);
		else
		PART_CASE(arb, mag, arb_t, mag_t, arb_ptr, mag_ptr, rad);
		break;
	}
	case R_FLINT_CLASS_ACB:
	{
		if (mode == 0)
		PART_CASE(acb, arb, acb_t, arb_t, acb_ptr, arb_ptr, real);
		else
		PART_CASE(acb, arb, acb_t, arb_t, acb_ptr, arb_ptr, imag);
		break;
	}
	default:
		return R_NilValue;
	}

#undef PART_CASE

	SEXP to = PROTECT(newObject(what));
	R_flint_set(to, p, n, f);
	UNPROTECT(1);
	return to;
}

SEXP R_flint_subscript(SEXP from, SEXP subscript)
{
	R_flint_class_t class = R_flint_get_class(from);
	void *p = R_flint_get_pointer(from);
	unsigned long long int j,
		n = (unsigned long long int) XLENGTH(subscript);
	R_CFinalizer_t f;
	const char *what;

#define SUBSCRIPT_CASE(name, elt_t, ptr_t) \
	do { \
		ptr_t x = (ptr_t) p; \
		ptr_t y = (ptr_t) ((n) ? flint_calloc((size_t) n, sizeof(elt_t)) : 0); \
		if (TYPEOF(subscript) == INTSXP) { \
			const int *s = INTEGER_RO(subscript); \
			for (j = 0; j < n; ++j) \
				name##_set(y + j, x + s[j] - 1); \
		} else { \
			const double *s = REAL_RO(subscript); \
			for (j = 0; j < n; ++j) \
				name##_set(y + j, x + (unsigned long long int) s[j] - 1); \
		} \
		p = (void *) y; \
		f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
		what = #name; \
	} while (0)

	switch (class) {
	case R_FLINT_CLASS_SLONG:
	{
		SUBSCRIPT_CASE(slong, slong, slong *);
		break;
	}
	case R_FLINT_CLASS_ULONG:
	{
		SUBSCRIPT_CASE(ulong, ulong, ulong *);
		break;
	}
	case R_FLINT_CLASS_FMPZ:
	{
		SUBSCRIPT_CASE(fmpz, fmpz, fmpz *);
		break;
	}
	case R_FLINT_CLASS_FMPQ:
	{
		SUBSCRIPT_CASE(fmpq, fmpq, fmpq *);
		break;
	}
	case R_FLINT_CLASS_ARF:
	{
		SUBSCRIPT_CASE(arf, arf_t, arf_ptr);
		break;
	}
	case R_FLINT_CLASS_MAG:
	{
		SUBSCRIPT_CASE(mag, mag_t, mag_ptr);
		break;
	}
	case R_FLINT_CLASS_ARB:
	{
		SUBSCRIPT_CASE(arb, arb_t, arb_ptr);
		break;
	}
	case R_FLINT_CLASS_ACB:
	{
		SUBSCRIPT_CASE(acb, acb_t, acb_ptr);
		break;
	}
	default:
		return R_NilValue;
	}

#undef SUBSCRIPT_CASE

	SEXP to = PROTECT(newObject(what));
	R_flint_set(to, p, n, f);
	UNPROTECT(1);
	return to;
}

SEXP R_flint_subassign(SEXP from, SEXP subscript, SEXP value)
{
	R_flint_class_t class = R_flint_get_class(from);
	void *p = R_flint_get_pointer(from),
		*p__ = R_flint_get_pointer(value);
	unsigned long long int i, j,
		m = (unsigned long long int) XLENGTH(subscript),
		n = R_flint_get_length(from),
		n__ = R_flint_get_length(value);
	R_CFinalizer_t f;
	const char *what;

#define SUBASSIGN_CASE(name, elt_t, ptr_t) \
	do { \
		ptr_t v = (ptr_t) p__; \
		ptr_t x = (ptr_t) p; \
		ptr_t y = (ptr_t) ((n) ? flint_calloc((size_t) n, sizeof(elt_t)) : 0); \
		if (subscript == R_NilValue) { \
			for (j = 0; j < n; ++j) \
				name##_set(y + j, v + j % n__); \
		} else { \
			for (j = 0; j < n; ++j) \
				name##_set(y + j, x + j); \
			if (TYPEOF(subscript) == INTSXP) { \
				const int *s = INTEGER_RO(subscript); \
				for (i = 0; i < m; ++i) \
					name##_set(y + s[i] - 1, v + i % n__); \
			} else { \
				const double *s = REAL_RO(subscript); \
				for (i = 0; i < m; ++i) \
					name##_set(y + (unsigned long long int) s[i] - 1, v + i % n__); \
			} \
		} \
		p = (void *) y; \
		f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
		what = #name; \
	} while (0)

	switch (class) {
	case R_FLINT_CLASS_SLONG:
	{
		SUBASSIGN_CASE(slong, slong, slong *);
		break;
	}
	case R_FLINT_CLASS_ULONG:
	{
		SUBASSIGN_CASE(ulong, ulong, ulong *);
		break;
	}
	case R_FLINT_CLASS_FMPZ:
	{
		SUBASSIGN_CASE(fmpz, fmpz, fmpz *);
		break;
	}
	case R_FLINT_CLASS_FMPQ:
	{
		SUBASSIGN_CASE(fmpq, fmpq, fmpq *);
		break;
	}
	case R_FLINT_CLASS_ARF:
	{
		SUBASSIGN_CASE(arf, arf_t, arf_ptr);
		break;
	}
	case R_FLINT_CLASS_MAG:
	{
		SUBASSIGN_CASE(mag, mag_t, mag_ptr);
		break;
	}
	case R_FLINT_CLASS_ARB:
	{
		SUBASSIGN_CASE(arb, arb_t, arb_ptr);
		break;
	}
	case R_FLINT_CLASS_ACB:
	{
		SUBASSIGN_CASE(acb, acb_t, acb_ptr);
		break;
	}
	default:
		return R_NilValue;
	}

#undef SUBASSIGN_CASES

	SEXP to = PROTECT(newObject(what));
	R_flint_set(to, p, n, f);
	UNPROTECT(1);
	return to;
}
