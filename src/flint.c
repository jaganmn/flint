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
		ans = Rf_allocVector(REALSXP, 1);
		REAL(ans)[0] = (double) n_;
		if (n_ != n) {
			SEXP off;
			PROTECT(ans);
			PROTECT(off = Rf_allocVector(REALSXP, 1));
			REAL(off)[0] = (double) (n - n_);
			Rf_setAttrib(ans, R_flint_symbol_off, off);
#if 0
			Rf_warning(_("true length %llu truncated to %llu"), n, n_);
#endif
			UNPROTECT(2);
		}
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

SEXP R_flint_part(SEXP object, SEXP s_mode)
{
	R_flint_class_t class = R_flint_get_class(object);
	const void *x = R_flint_get_pointer(object);
	void *y;
	unsigned long long int j,
		nx = R_flint_get_length(object),
		ny = nx;
	R_CFinalizer_t f;
	const char *what;
	int mode = INTEGER_RO(s_mode)[0];

#define PART_CASE(xname, yname, xelt_t, yelt_t, xptr_t, yptr_t, part) \
	do { \
		xptr_t x__ = (xptr_t) x; \
		yptr_t y__ = (yptr_t) ((ny) ? flint_calloc((size_t) ny, sizeof(yelt_t)) : 0); \
		for (j = 0; j < ny; ++j) \
			yname##_set(y__ + j, xname##_##part##ref(x__ + j)); \
		y = (void *) y__; \
		f = (R_CFinalizer_t) &R_flint_##yname##_finalize; \
		what = #yname; \
	} while (0)

	switch (class) {
	case R_FLINT_CLASS_FMPQ:
		if (mode == 0)
		PART_CASE(fmpq, fmpz, fmpq, fmpz, const fmpq *, fmpz *, num);
		else
		PART_CASE(fmpq, fmpz, fmpq, fmpz, const fmpq *, fmpz *, den);
		break;
	case R_FLINT_CLASS_ARB:
		if (mode == 0)
		PART_CASE(arb, arf, arb_t, arf_t, arb_srcptr, arf_ptr, mid);
		else
		PART_CASE(arb, mag, arb_t, mag_t, arb_srcptr, mag_ptr, rad);
		break;
	case R_FLINT_CLASS_ACB:
		if (mode == 0)
		PART_CASE(acb, arb, acb_t, arb_t, acb_srcptr, arb_ptr, real);
		else
		PART_CASE(acb, arb, acb_t, arb_t, acb_srcptr, arb_ptr, imag);
		break;
	default:
		return R_NilValue;
	}

#undef PART_CASE

	SEXP ans = PROTECT(newObject(what));
	R_flint_set(ans, y, ny, f);
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_subscript(SEXP object, SEXP subscript)
{
	R_flint_class_t class = R_flint_get_class(object);
	const void *x = R_flint_get_pointer(object);
	void *y;
	unsigned long long int j,
#if 0
		nx = R_flint_get_length(object),
#endif
		ny = (unsigned long long int) XLENGTH(subscript);
	R_CFinalizer_t f;
	const char *what;

#define SUBSCRIPT_CASE(name, elt_t, xptr_t, yptr_t) \
	do { \
		xptr_t x__ = (xptr_t) x; \
		yptr_t y__ = (yptr_t) ((ny) ? flint_calloc((size_t) ny, sizeof(elt_t)) : 0); \
		if (TYPEOF(subscript) == INTSXP) { \
			const int *s__ = INTEGER_RO(subscript); \
			for (j = 0; j < ny; ++j) \
				name##_set(y__ + j, x__ + s__[j] - 1); \
		} else { \
			const double *s__ = REAL_RO(subscript); \
			for (j = 0; j < ny; ++j) \
				name##_set(y__ + j, x__ + (unsigned long long int) s__[j] - 1); \
		} \
		y = (void *) y__; \
		f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
		what = #name; \
	} while (0)

	switch (class) {
	case R_FLINT_CLASS_SLONG:
		SUBSCRIPT_CASE(slong, slong, const slong *, slong *);
		break;
	case R_FLINT_CLASS_ULONG:
		SUBSCRIPT_CASE(ulong, ulong, const ulong *, ulong *);
		break;
	case R_FLINT_CLASS_FMPZ:
		SUBSCRIPT_CASE(fmpz, fmpz, const fmpz *, fmpz *);
		break;
	case R_FLINT_CLASS_FMPQ:
		SUBSCRIPT_CASE(fmpq, fmpq, const fmpq *, fmpq *);
		break;
	case R_FLINT_CLASS_ARF:
		SUBSCRIPT_CASE(arf, arf_t, arf_srcptr, arf_ptr);
		break;
	case R_FLINT_CLASS_MAG:
		SUBSCRIPT_CASE(mag, mag_t, mag_srcptr, mag_ptr);
		break;
	case R_FLINT_CLASS_ARB:
		SUBSCRIPT_CASE(arb, arb_t, arb_srcptr, arb_ptr);
		break;
	case R_FLINT_CLASS_ACB:
		SUBSCRIPT_CASE(acb, acb_t, acb_srcptr, acb_ptr);
		break;
	default:
		return R_NilValue;
	}

#undef SUBSCRIPT_CASE

	SEXP ans = PROTECT(newObject(what));
	R_flint_set(ans, y, ny, f);
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_subassign(SEXP object, SEXP subscript, SEXP value)
{
	R_flint_class_t class = R_flint_get_class(object);
	const void
		*v = R_flint_get_pointer(value),
		*x = R_flint_get_pointer(object);
	void *y;
	unsigned long long int j,
		ns = (unsigned long long int) XLENGTH(subscript),
		nv = R_flint_get_length(value),
		nx = R_flint_get_length(object),
		ny = nx;
	R_CFinalizer_t f;
	const char *what;

#define SUBASSIGN_CASE(name, elt_t, xptr_t, yptr_t) \
	do { \
		xptr_t v__ = (xptr_t) v; \
		xptr_t x__ = (xptr_t) x; \
		yptr_t y__ = (yptr_t) ((ny) ? flint_calloc((size_t) ny, sizeof(elt_t)) : 0); \
		if (subscript == R_NilValue) { \
			for (j = 0; j < ny; ++j) \
				name##_set(y__ + j, v__ + j % nv); \
		} else { \
			for (j = 0; j < ny; ++j) \
				name##_set(y__ + j, x__ + j); \
			if (TYPEOF(subscript) == INTSXP) { \
				const int *s__ = INTEGER_RO(subscript); \
				for (j = 0; j < ns; ++j) \
					name##_set(y__ + s__[j] - 1, v__ + j % nv); \
			} else { \
				const double *s__ = REAL_RO(subscript); \
				for (j = 0; j < ns; ++j) \
					name##_set(y__ + (unsigned long long int) s__[j] - 1, v__ + j % nv); \
			} \
		} \
		y = (void *) y__; \
		f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
		what = #name; \
	} while (0)

	switch (class) {
	case R_FLINT_CLASS_SLONG:
		SUBASSIGN_CASE(slong, slong, const slong *, slong *);
		break;
	case R_FLINT_CLASS_ULONG:
		SUBASSIGN_CASE(ulong, ulong, const ulong *, ulong *);
		break;
	case R_FLINT_CLASS_FMPZ:
		SUBASSIGN_CASE(fmpz, fmpz, const fmpz *, fmpz *);
		break;
	case R_FLINT_CLASS_FMPQ:
		SUBASSIGN_CASE(fmpq, fmpq, const fmpq *, fmpq *);
		break;
	case R_FLINT_CLASS_ARF:
		SUBASSIGN_CASE(arf, arf_t, arf_srcptr, arf_ptr);
		break;
	case R_FLINT_CLASS_MAG:
		SUBASSIGN_CASE(mag, mag_t, mag_srcptr, mag_ptr);
		break;
	case R_FLINT_CLASS_ARB:
		SUBASSIGN_CASE(arb, arb_t, arb_srcptr, arb_ptr);
		break;
	case R_FLINT_CLASS_ACB:
		SUBASSIGN_CASE(acb, acb_t, acb_srcptr, acb_ptr);
		break;
	default:
		return R_NilValue;
	}

#undef SUBASSIGN_CASE

	SEXP to = PROTECT(newObject(what));
	R_flint_set(to, y, ny, f);
	UNPROTECT(1);
	return to;
}

SEXP R_flint_identical(SEXP object, SEXP reference)
{
	R_flint_class_t class = R_flint_get_class(reference);
	if (R_flint_get_class(object) != class)
		return Rf_ScalarLogical(0);
	unsigned long long int j, n = R_flint_get_length(reference);
	if (R_flint_get_length(object) != n)
		return Rf_ScalarLogical(0);
	const void
		*x = R_flint_get_pointer(object),
		*y = R_flint_get_pointer(reference);

#define IDENTICAL_CASE(name, ptr_t) \
	do { \
		ptr_t x__ = (ptr_t) x; \
		ptr_t y__ = (ptr_t) y; \
		for (j = 0; j < n; ++j) \
			if (!name##_equal(x__ + j, y__ + j)) \
				return Rf_ScalarLogical(0); \
	} while (0)

	if (x != y)
	switch (class) {
	case R_FLINT_CLASS_SLONG:
		IDENTICAL_CASE(slong, const slong *);
		break;
	case R_FLINT_CLASS_ULONG:
		IDENTICAL_CASE(ulong, const ulong *);
		break;
	case R_FLINT_CLASS_FMPZ:
		IDENTICAL_CASE(fmpz, const fmpz *);
		break;
	case R_FLINT_CLASS_FMPQ:
		IDENTICAL_CASE(fmpq, const fmpq *);
		break;
	case R_FLINT_CLASS_ARF:
		IDENTICAL_CASE(arf, arf_srcptr);
		break;
	case R_FLINT_CLASS_MAG:
		IDENTICAL_CASE(mag, mag_srcptr);
		break;
	case R_FLINT_CLASS_ARB:
		IDENTICAL_CASE(arb, arb_srcptr);
		break;
	case R_FLINT_CLASS_ACB:
		IDENTICAL_CASE(acb, acb_srcptr);
		break;
	default:
		break;
	}

#undef IDENTICAL_CASE

	return Rf_ScalarLogical(1);
}
