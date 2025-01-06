#include <gmp.h>
#include <flint/flint.h>
#include <flint/fmpz.h>
#include <flint/fmpq.h>
#include <flint/mag.h>
#include <flint/arf.h>
#include <flint/acf.h>
#include <flint/arb.h>
#include <flint/acb.h>
#include "flint.h"

#define R_FLINT_SWITCH(class, template) \
do { \
	switch (class) { \
	case R_FLINT_CLASS_SLONG: \
		template(slong, slong, const slong *, slong *); \
		break; \
	case R_FLINT_CLASS_ULONG: \
		template(ulong, ulong, const ulong *, ulong *); \
		break; \
	case R_FLINT_CLASS_FMPZ: \
		template(fmpz, fmpz, const fmpz *, fmpz *); \
		break; \
	case R_FLINT_CLASS_FMPQ: \
		template(fmpq, fmpq, const fmpq *, fmpq *); \
		break; \
	case R_FLINT_CLASS_MAG: \
		template(mag, mag_t, mag_srcptr, mag_ptr); \
		break; \
	case R_FLINT_CLASS_ARF: \
		template(arf, arf_t, arf_srcptr, arf_ptr); \
		break; \
	case R_FLINT_CLASS_ACF: \
		template(acf, acf_t, acf_srcptr, acf_ptr); \
		break; \
	case R_FLINT_CLASS_ARB: \
		template(arb, arb_t, arb_srcptr, arb_ptr); \
		break; \
	case R_FLINT_CLASS_ACB: \
		template(acb, acb_t, acb_srcptr, acb_ptr); \
		break; \
	default: \
		Rf_error(_("should never happen ...")); \
	} \
} while (0)

#define slong_zero(rop) *(rop) = 0
#define ulong_zero(rop) *(rop) = 0
#define slong_set(rop, op) *(rop) = *(op)
#define ulong_set(rop, op) *(rop) = *(op)
#define slong_equal(rop, op) (*(rop) == *(op))
#define ulong_equal(rop, op) (*(rop) == *(op))

#ifndef HAVE_ACF_ZERO
static R_INLINE
void acf_zero(acf_t res)
{
	arf_zero(acf_realref(res));
	arf_zero(acf_imagref(res));
	return;
}
#endif

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

unsigned long int R_flint_get_length(SEXP object)
{
	SEXP x = R_do_slot(object, R_flint_symbol_dot_xdata),
		length = R_ExternalPtrProtected(x);
	unsigned long int n;
	uucopy(&n, (unsigned int *) INTEGER_RO(length));
	return n;
}

R_flint_class_t R_flint_get_class(SEXP object)
{
	int i = (TYPEOF(object) == OBJSXP)
		? R_check_class_etc(object, R_flint_classes) : -1;
	return (R_flint_class_t) i;
}

void R_flint_set(SEXP object,
                 void *p, unsigned long int n, R_CFinalizer_t f)
{
	SEXP length = PROTECT(Rf_allocVector(INTSXP, 2));
	ucopy((unsigned int *) INTEGER(length), &n);
	SEXP x = PROTECT(R_MakeExternalPtr(p, R_NilValue, length));
	R_RegisterCFinalizer(x, f);
	R_do_slot_assign(object, R_flint_symbol_dot_xdata, x);
	UNPROTECT(2);
	return;
}

SEXP R_flint_bind(SEXP dots, SEXP s_usenames)
{
	if (XLENGTH(s_usenames) == 0)
		Rf_error(_("'%s' of length zero in '%s'"),
		         "use.names", "c.flint");
	int usenames = LOGICAL_RO(s_usenames)[0], anynamed = !usenames;
	R_xlen_t a, ndots = XLENGTH(dots);
	if (ndots == 0)
		return R_NilValue;
	R_flint_class_t class = R_flint_get_class(VECTOR_ELT(dots, 0));
	void *y;
	unsigned long int jx, jy = 0, nx, ny = 0;
	R_CFinalizer_t f;
	const char *what;

	SEXP elt;
	for (a = 0; a < ndots; ++a) {
		elt = VECTOR_ELT(dots, a);
		nx = R_flint_get_length(elt);
		if (nx > (unsigned long int) -1 - ny)
			Rf_error(_("value length would exceed maximum %lu"),
			         (unsigned long int) -1);
		if (!anynamed && XLENGTH(R_do_slot(elt, R_flint_symbol_names)) > 0)
			anynamed = 1;
		ny += nx;
	}

	SEXP sx, sy = R_NilValue;
	usenames = usenames && anynamed && ny <= R_XLEN_T_MAX;
	if (usenames)
		sy = Rf_allocVector(STRSXP, (R_xlen_t) ny);
	PROTECT(sy);

#define TEMPLATE(name, elt_t, xptr_t, yptr_t) \
	do { \
		xptr_t x__; \
		yptr_t y__ = (ny) ? flint_calloc(ny, sizeof(elt_t)) : 0; \
		y = y__; \
		f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
		what = #name; \
		for (a = 0; a < ndots; ++a) { \
			elt = VECTOR_ELT(dots, a); \
			nx = R_flint_get_length(elt); \
			x__ = R_flint_get_pointer(elt); \
			if (usenames && XLENGTH(sx = R_do_slot(elt, R_flint_symbol_names)) > 0) \
			for (jx = 0; jx < nx; ++jx, ++jy) { \
				name##_set(y__ + jy, x__ + jx); \
				SET_STRING_ELT(sy, (R_xlen_t) jy, \
				               STRING_ELT(sx, (R_xlen_t) jx)); \
			} \
			else \
			for (jx = 0; jx < nx; ++jx, ++jy) \
				name##_set(y__ + jy, x__ + jx); \
		} \
	} while (0)

	R_FLINT_SWITCH(class, TEMPLATE);

#undef TEMPLATE

	SEXP ans = PROTECT(newObject(what));
	R_flint_set(ans, y, ny, f);
	if (usenames)
		R_do_slot_assign(ans, R_flint_symbol_names, sy);
	UNPROTECT(2);
	return ans;
}

SEXP R_flint_class(SEXP object)
{
	int i = (TYPEOF(object) == OBJSXP)
		? R_check_class_etc(object, R_flint_classes) : -1;
	SEXP ans = Rf_allocVector(STRSXP, 1);
	SET_STRING_ELT(ans, 0, (i < 0) ? NA_STRING : Rf_mkChar(R_flint_classes[i]));
	return ans;
}

SEXP R_flint_identical(SEXP object, SEXP reference)
{
	R_flint_class_t class = R_flint_get_class(reference);
	if (R_flint_get_class(object) != class)
		return Rf_ScalarLogical(0);
	unsigned long int j, n = R_flint_get_length(reference);
	if (R_flint_get_length(object) != n)
		return Rf_ScalarLogical(0);
	const void
		*x = R_flint_get_pointer(object),
		*y = R_flint_get_pointer(reference);

#define TEMPLATE(name, elt_t, xptr_t, yptr_t) \
	do { \
		xptr_t x__ = x; \
		xptr_t y__ = y; \
		for (j = 0; j < n; ++j) \
			if (!name##_equal(x__ + j, y__ + j)) \
				return Rf_ScalarLogical(0); \
	} while (0)

	if (x != y)
	R_FLINT_SWITCH(class, TEMPLATE);

#undef TEMPLATE

	return Rf_ScalarLogical(1);
}

SEXP R_flint_length(SEXP object, SEXP s_exact)
{
	if (XLENGTH(s_exact) == 0)
		Rf_error(_("'%s' of length zero in '%s'"),
		         "exact", "flintLength");
	int exact = LOGICAL_RO(s_exact)[0];
	unsigned long int n = R_flint_get_length(object);
	SEXP ans;
	if (exact) {
		ans = newObject("ulong");
		ulong *p = flint_calloc(1, sizeof(ulong));
		R_flint_set(ans, p, 1, (R_CFinalizer_t) &R_flint_ulong_finalize);
		p[0] = n;
	} else if (n <= INT_MAX) {
		ans = Rf_allocVector(INTSXP, 1);
		INTEGER(ans)[0] = (int) n;
	} else {
		unsigned long int n_ = (unsigned long int) (double) n;
		if (n_ >  n)
			n_ = (unsigned long int) nextafter((double) n, 0.0);
		ans = Rf_allocVector(REALSXP, 1);
		REAL(ans)[0] = (double) n_;
		if (n_ != n) {
			SEXP off;
			PROTECT(ans);
			PROTECT(off = Rf_allocVector(INTSXP, 1));
			INTEGER(off)[0] = (int) (n - n_);
			Rf_setAttrib(ans, R_flint_symbol_off, off);
#if 0
			Rf_warning(_("true length %lu truncated to %lu"), n, n_);
#endif
			UNPROTECT(2);
		}
	}
	return ans;
}

SEXP R_flint_new(SEXP class)
{
	return newObject(CHAR(STRING_ELT(class, 0)));
}

SEXP R_flint_realloc(SEXP object, SEXP s_lengthout)
{
	int usenames = 1;
	R_flint_class_t class = R_flint_get_class(object);
	const void *x = R_flint_get_pointer(object);
	void *y;
	unsigned long int j,
		nx = R_flint_get_length(object),
		ny = 0,
		n0 = 0;
	R_CFinalizer_t f;
	const char *what;

	if (R_flint_get_length(s_lengthout) != 1)
		Rf_error(_("length(%s) not equal to 1 in '%s'"),
		         "value", "length<-");
	ny = ((ulong *) R_flint_get_pointer(s_lengthout))[0];
	n0 = (nx < ny) ? nx : ny;

	SEXP sx = PROTECT(R_do_slot(object, R_flint_symbol_names)),
		sy = R_NilValue;
	usenames = usenames && XLENGTH(sx) > 0 && ny <= R_XLEN_T_MAX;
	if (usenames)
		sy = Rf_allocVector(STRSXP, (R_xlen_t) ny);
	PROTECT(sy);

#define TEMPLATE(name, elt_t, xptr_t, yptr_t) \
	do { \
		xptr_t x__ = x; \
		yptr_t y__ = (ny) ? flint_calloc(ny, sizeof(elt_t)) : 0; \
		y = y__; \
		f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
		what = #name; \
		if (usenames) \
		for (j =  0; j < n0; ++j) { \
			name##_set(y__ + j, x__ + j); \
			SET_STRING_ELT(sy, (R_xlen_t) j, \
			               STRING_ELT(sx, (R_xlen_t) j)); \
		} \
		else \
		for (j =  0; j < n0; ++j) \
			name##_set(y__ + j, x__ + j); \
		for (j = n0; j < ny; ++j) \
			name##_zero(y__ + j); \
	} while (0)

	R_FLINT_SWITCH(class, TEMPLATE);

#undef TEMPLATE

	SEXP ans = PROTECT(newObject(what));
	R_flint_set(ans, y, ny, f);
	if (usenames)
		R_do_slot_assign(ans, R_flint_symbol_names, sy);
	UNPROTECT(3);
	return ans;
}

SEXP R_flint_rep_each(SEXP object, SEXP s_each, SEXP s_usenames)
{
	int usenames = LOGICAL_RO(s_usenames)[0];
	R_flint_class_t class = R_flint_get_class(object);
	const void *x = R_flint_get_pointer(object);
	void *y;
	unsigned long int jx, jy = 0,
		nx = R_flint_get_length(object),
		ny = 0;
	R_CFinalizer_t f;
	const char *what;

	if (R_flint_get_length(s_each) != 1)
		Rf_error(_("length(%s) not equal to 1 in '%s'"),
		         "each", "rep");
	ulong i, each = ((ulong *) R_flint_get_pointer(s_each))[0];
	if (each > 0 && nx > (unsigned long int) -1 / each)
		Rf_error(_("value length would exceed maximum %lu"),
		         (unsigned long int) -1);
	ny = nx * each;;

	SEXP sx = PROTECT(R_do_slot(object, R_flint_symbol_names)),
		sy = R_NilValue, nm;
	usenames = usenames && XLENGTH(sx) > 0 && ny <= R_XLEN_T_MAX;
	if (usenames)
		sy = Rf_allocVector(STRSXP, (R_xlen_t) ny);
	PROTECT(sy);

#define TEMPLATE(name, elt_t, xptr_t, yptr_t) \
	do { \
		xptr_t x__ = x; \
		yptr_t y__ = (ny) ? flint_calloc(ny, sizeof(elt_t)) : 0; \
		y = y__; \
		f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
		what = #name; \
		if (usenames) \
		for (jx = 0; jx < nx; ++jx) { \
			nm = STRING_ELT(sx, (R_xlen_t) jx); \
			for (i = 0; i < each; ++i, ++jy) { \
				name##_set(y__ + jy, x__ + jx); \
				SET_STRING_ELT(sy, (R_xlen_t) jy, nm); \
			} \
		} \
		else \
		for (jx = 0; jx < nx; ++jx) \
			for (i = 0; i < each; ++i, ++jy) \
				name##_set(y__ + jy, x__ + jx); \
	} while (0)

	R_FLINT_SWITCH(class, TEMPLATE);

#undef TEMPLATE

	SEXP ans = PROTECT(newObject(what));
	R_flint_set(ans, y, ny, f);
	if (usenames)
		R_do_slot_assign(ans, R_flint_symbol_names, sy);
	UNPROTECT(3);
	return ans;
}

SEXP R_flint_rep_lengthout(SEXP object, SEXP s_lengthout, SEXP s_usenames)
{
	int usenames = LOGICAL_RO(s_usenames)[0];
	R_flint_class_t class = R_flint_get_class(object);
	const void *x = R_flint_get_pointer(object);
	void *y;
	unsigned long int q, r, i, jx, jy = 0,
		nx = R_flint_get_length(object),
		ny = 0;
	R_CFinalizer_t f;
	const char *what;

	if (R_flint_get_length(s_lengthout) != 1)
		Rf_error(_("length(%s) not equal to 1 in '%s'"),
		         "length.out", "rep");
	ny = ((ulong *) R_flint_get_pointer(s_lengthout))[0];
	if (ny == 0)
		q = r = 0;
	else {
		if (nx == 0)
			Rf_error(_("'%s' of length zero cannot be recycled to nonzero length"),
			         "x");
		q = ny / nx, r = ny % nx;
	}

	SEXP sx = PROTECT(R_do_slot(object, R_flint_symbol_names)),
		sy = R_NilValue;
	usenames = usenames && XLENGTH(sx) > 0 && ny <= R_XLEN_T_MAX;
	if (usenames)
		sy = Rf_allocVector(STRSXP, (R_xlen_t) ny);
	PROTECT(sy);

#define TEMPLATE(name, elt_t, xptr_t, yptr_t) \
	do { \
		xptr_t x__ = x; \
		yptr_t y__ = (ny) ? flint_calloc(ny, sizeof(elt_t)) : 0; \
		y = y__; \
		f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
		what = #name; \
		if (usenames) { \
		for (i = 0; i < q; ++i) { \
			for (jx = 0; jx < nx; ++jx, ++jy) { \
				name##_set(y__ + jy, x__ + jx); \
				SET_STRING_ELT(sy, (R_xlen_t) jy, \
				               STRING_ELT(sx, (R_xlen_t) jx)); \
			} \
		} \
		for (jx = 0; jx < r; ++jx, ++jy) { \
			name##_set(y__ + jy, x__ + jx); \
			SET_STRING_ELT(sy, (R_xlen_t) jy, \
			               STRING_ELT(sx, (R_xlen_t) jx)); \
		} \
		} else { \
		for (i = 0; i < q; ++i) \
			for (jx = 0; jx < nx; ++jx, ++jy) \
				name##_set(y__ + jy, x__ + jx); \
		for (jx = 0; jx < r; ++jx, ++jy) \
			name##_set(y__ + jy, x__ + jx); \
		} \
	} while (0)

	R_FLINT_SWITCH(class, TEMPLATE);

#undef TEMPLATE

	SEXP ans = PROTECT(newObject(what));
	R_flint_set(ans, y, ny, f);
	if (usenames)
		R_do_slot_assign(ans, R_flint_symbol_names, sy);
	UNPROTECT(3);
	return ans;
}

SEXP R_flint_rep_times(SEXP object, SEXP s_times, SEXP s_usenames)
{
	int usenames = LOGICAL_RO(s_usenames)[0];
	R_flint_class_t class = R_flint_get_class(object);
	const void *x = R_flint_get_pointer(object);
	void *y;
	unsigned long int jx, jy = 0,
		nx = R_flint_get_length(object),
		ny = 0;
	R_CFinalizer_t f;
	const char *what;

	unsigned long int ntimes = R_flint_get_length(s_times);
	if (ntimes != 1 && ntimes != nx)
		Rf_error(_("length(%s) not equal to 1 or length(%s) in '%s'"),
		         "times", "x", "rep");
	ulong i, t;
	const ulong *times = R_flint_get_pointer(s_times);
	if (ntimes == 1) {
		t = times[0];
		if (t > 0 && nx > (unsigned long int) -1 / t)
			Rf_error(_("value length would exceed maximum %lu"),
			         (unsigned long int) -1);
		ny = nx * t;
	} else {
		for (jx = 0; jx < nx; ++jx) {
			t = times[jx];
			if (t > (unsigned long int) -1 - ny)
				Rf_error(_("value length would exceed maximum %lu"),
				         (unsigned long int) -1);
			ny += t;
		}
	}

	SEXP sx = PROTECT(R_do_slot(object, R_flint_symbol_names)),
		sy = R_NilValue, nm;
	usenames = usenames && XLENGTH(sx) > 0 && ny <= R_XLEN_T_MAX;
	if (usenames)
		sy = Rf_allocVector(STRSXP, (R_xlen_t) ny);
	PROTECT(sy);

#define TEMPLATE(name, elt_t, xptr_t, yptr_t) \
	do { \
		xptr_t x__ = x; \
		yptr_t y__ = (ny) ? flint_calloc(ny, sizeof(elt_t)) : 0; \
		y = y__; \
		f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
		what = #name; \
		if (ntimes == 1) { \
			t = times[0]; \
			if (usenames) \
			for (i = 0; i < t; ++i) { \
				for (jx = 0; jx < nx; ++jx, ++jy) { \
					name##_set(y__ + jy, x__ + jx); \
					SET_STRING_ELT(sy, (R_xlen_t) jy, \
					               STRING_ELT(sx, (R_xlen_t) jx)); \
				} \
			} \
			else \
			for (i = 0; i < t; ++i) \
				for (jx = 0; jx < nx; ++jx, ++jy) \
					name##_set(y__ + jy, x__ + jx); \
		} else { \
			if (usenames) \
			for (jx = 0; jx < nx; ++jx) { \
				nm = STRING_ELT(sx, (R_xlen_t) nx); \
				t = times[jx]; \
				for (i = 0; i < t; ++i, ++jy) { \
					name##_set(y__ + jy, x__ + jx); \
					SET_STRING_ELT(sy, (R_xlen_t) jy, nm); \
				} \
			} \
			else \
			for (jx = 0; jx < nx; ++jx) { \
				t = times[jx]; \
				for (i = 0; i < t; ++i, ++jy) \
					name##_set(y__ + jy, x__ + jx); \
			} \
		} \
	} while (0)

	R_FLINT_SWITCH(class, TEMPLATE);

#undef TEMPLATE

	SEXP ans = PROTECT(newObject(what));
	R_flint_set(ans, y, ny, f);
	if (usenames)
		R_do_slot_assign(ans, R_flint_symbol_names, sy);
	UNPROTECT(3);
	return ans;
}

SEXP R_flint_size(SEXP object)
{
	R_flint_class_t class = R_flint_get_class(object);
	const void *x = R_flint_get_pointer(object);
	unsigned long int j, n = R_flint_get_length(object);
	size_t count = 0;

#define fmpz_size(p) \
	do { \
		if (COEFF_IS_MPZ(*(p))) { \
			count += sizeof(mpz_t); \
			count += mpz_size(COEFF_TO_PTR(*(p))) * sizeof(mp_limb_t); \
		} \
	} while (0)

#define fmpq_size(p) \
	do { \
		fmpz_size(fmpq_numref(p)); \
		fmpz_size(fmpq_denref(p)); \
	} while (0)

#define mag_size(p) \
	do { \
		fmpz_size(MAG_EXPREF(p)); \
	} while (0)

#define arf_size(p) \
	do { \
		fmpz_size(ARF_EXPREF(p)); \
		if (ARF_HAS_PTR(p)) \
			count += (size_t) ARF_PTR_ALLOC(p) * sizeof(mp_limb_t); \
	} while (0)

#define acf_size(p) \
	do { \
		arf_size(acf_realref(p)); \
		arf_size(acf_imagref(p)); \
	} while (0)

#define arb_size(p) \
	do { \
		arf_size(arb_midref(p)); \
		mag_size(arb_radref(p)); \
	} while (0)

#define acb_size(p) \
	do { \
		arb_size(acb_realref(p)); \
		arb_size(acb_imagref(p)); \
	} while (0)

#define SIZE_CASE(name, elt_t, ptr_t) \
	do { \
		ptr_t x__ = x; \
		count += n * sizeof(elt_t); \
		for (j = 0; j < n; ++j) \
			name##_size(x__ + j); \
	} while (0)

	switch (class) {
	case R_FLINT_CLASS_SLONG:
		count += n * sizeof(slong);
		break;
	case R_FLINT_CLASS_ULONG:
		count += n * sizeof(ulong);
		break;
	case R_FLINT_CLASS_FMPZ:
		SIZE_CASE(fmpz, fmpz, const fmpz *);
		break;
	case R_FLINT_CLASS_FMPQ:
		SIZE_CASE(fmpq, fmpq, const fmpq *);
		break;
	case R_FLINT_CLASS_MAG:
		SIZE_CASE(mag, mag_t, mag_srcptr);
		break;
	case R_FLINT_CLASS_ARF:
		SIZE_CASE(arf, arf_t, arf_srcptr);
		break;
	case R_FLINT_CLASS_ACF:
		SIZE_CASE(acf, acf_t, acf_srcptr);
		break;
	case R_FLINT_CLASS_ARB:
		SIZE_CASE(arb, arb_t, arb_srcptr);
		break;
	case R_FLINT_CLASS_ACB:
		SIZE_CASE(acb, acb_t, acb_srcptr);
		break;
	default:
		return R_NilValue;
	}

#undef SIZE_CASE

	return Rf_ScalarReal((double) count);
}

SEXP R_flint_subassign(SEXP object, SEXP subscript, SEXP value)
{
	int usenames = 1;
	R_flint_class_t class = R_flint_get_class(object);
	const void
		*v = R_flint_get_pointer(value),
		*x = R_flint_get_pointer(object);
	void *y;
	unsigned long int j,
		ns = (unsigned long int) XLENGTH(subscript),
		nv = R_flint_get_length(value),
		nx = R_flint_get_length(object),
		ny = nx;
	R_CFinalizer_t f;
	const char *what;

	SEXP sx = PROTECT(R_do_slot(object, R_flint_symbol_names));
	usenames = usenames && XLENGTH(sx) > 0 && ny <= R_XLEN_T_MAX;

#define TEMPLATE(name, elt_t, xptr_t, yptr_t) \
	do { \
		xptr_t v__ = v; \
		xptr_t x__ = x; \
		yptr_t y__ = (ny) ? flint_calloc(ny, sizeof(elt_t)) : 0; \
		y = y__; \
		f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
		what = #name; \
		if (subscript == R_NilValue) { \
			for (j = 0; j < ny; ++j) \
				name##_set(y__ + j, v__ + j % nv); \
		} else { \
			for (j = 0; j < ny; ++j) \
				name##_set(y__ + j, x__ + j); \
			switch (TYPEOF(subscript)) { \
			case INTSXP: \
			{ \
				const int *s__ = INTEGER_RO(subscript); \
				for (j = 0; j < ns; ++j) \
					name##_set(y__ + (unsigned long int) s__[j] - 1, v__ + j % nv); \
				break; \
			} \
			case REALSXP: \
			{ \
				const double *s__ = REAL_RO(subscript); \
				for (j = 0; j < ns; ++j) \
					name##_set(y__ + (unsigned long int) s__[j] - 1, v__ + j % nv); \
				break; \
			} \
			case OBJSXP: \
			{ \
				const ulong *s__ = R_flint_get_pointer(subscript); \
				for (j = 0; j < ns; ++j) \
					name##_set(y__ + (unsigned long int) s__[j] - 1, v__ + j % nv); \
				break; \
			} \
			} \
		} \
	} while (0)

	R_FLINT_SWITCH(class, TEMPLATE);

#undef TEMPLATE

	SEXP ans = PROTECT(newObject(what));
	R_flint_set(ans, y, ny, f);
	if (usenames)
		R_do_slot_assign(ans, R_flint_symbol_names, sx);
	UNPROTECT(2);
	return ans;
}

SEXP R_flint_subscript(SEXP object, SEXP subscript, SEXP s_usenames)
{
	int usenames = LOGICAL_RO(s_usenames)[0];
	R_flint_class_t class = R_flint_get_class(object);
	const void *x = R_flint_get_pointer(object);
	void *y;
	unsigned long int jx, jy,
#if 0
		nx = R_flint_get_length(object),
#endif
		ny = (unsigned long int) XLENGTH(subscript);
	R_CFinalizer_t f;
	const char *what;

	SEXP sx = PROTECT(R_do_slot(object, R_flint_symbol_names)),
		sy = R_NilValue;
	usenames = usenames && XLENGTH(sx) > 0 && ny <= R_XLEN_T_MAX;
	if (usenames)
		sy = Rf_allocVector(STRSXP, (R_xlen_t) ny);
	PROTECT(sy);

#define TEMPLATE(name, elt_t, xptr_t, yptr_t) \
	do { \
		xptr_t x__ = x; \
		yptr_t y__ = (ny) ? flint_calloc(ny, sizeof(elt_t)) : 0; \
		y = y__; \
		f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
		what = #name; \
		switch (TYPEOF(subscript)) { \
		case INTSXP: \
		{ \
			const int *s__ = INTEGER_RO(subscript); \
			if (usenames) \
			for (jy = 0; jy < ny; ++jy) { \
				jx = (unsigned long int) s__[jy] - 1; \
				name##_set(y__ + jy, x__ + jx); \
				SET_STRING_ELT(sy, (R_xlen_t) jy, \
				               STRING_ELT(sx, (R_xlen_t) jx)); \
			} \
			else \
			for (jy = 0; jy < ny; ++jy) \
				name##_set(y__ + jy, x__ + s__[jy] - 1); \
			break; \
		} \
		case REALSXP: \
		{ \
			const double *s__ = REAL_RO(subscript); \
			if (usenames) \
			for (jy = 0; jy < ny; ++jy) { \
				jx = (unsigned long int) s__[jy] - 1; \
				name##_set(y__ + jy, x__ + jx); \
				SET_STRING_ELT(sy, (R_xlen_t) jy, \
				               STRING_ELT(sx, (R_xlen_t) jx)); \
			} \
			else \
			for (jy = 0; jy < ny; ++jy) \
				name##_set(y__ + jy, x__ + (unsigned long int) s__[jy] - 1); \
			break; \
		} \
		case OBJSXP: \
		{ \
			const ulong *s__ = R_flint_get_pointer(subscript); \
			if (usenames) \
			for (jy = 0; jy < ny; ++jy) { \
				jx = (unsigned long int) s__[jy] - 1; \
				name##_set(y__ + jy, x__ + jx); \
				SET_STRING_ELT(sy, (R_xlen_t) jy, \
				               STRING_ELT(sx, (R_xlen_t) jx)); \
			} \
			else \
			for (jy = 0; jy < ny; ++jy) \
				name##_set(y__ + jy, x__ + (unsigned long int) s__[jy] - 1); \
			break; \
		} \
		} \
	} while (0)

	R_FLINT_SWITCH(class, TEMPLATE);

#undef TEMPLATE

	SEXP ans = PROTECT(newObject(what));
	R_flint_set(ans, y, ny, f);
	if (usenames)
		R_do_slot_assign(ans, R_flint_symbol_names, sy);
	UNPROTECT(3);
	return ans;
}

SEXP R_flint_triple(SEXP object)
{
	SEXP ans = PROTECT(Rf_allocVector(STRSXP, 3));
	char buffer[64];
	snprintf(buffer, 64,  "%s", R_flint_classes[R_flint_get_class  (object)]);
	SET_STRING_ELT(ans, 0, Rf_mkChar(buffer));
	snprintf(buffer, 64, "%lu",                 R_flint_get_length (object)) ;
	SET_STRING_ELT(ans, 1, Rf_mkChar(buffer));
	snprintf(buffer, 64,  "%p",                 R_flint_get_pointer(object)) ;
	SET_STRING_ELT(ans, 2, Rf_mkChar(buffer));
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_valid(SEXP object)
{
#define INVALID(...) Rf_mkString(R_alloc_snprintf(255, __VA_ARGS__))
	SEXP x = R_do_slot(object, R_flint_symbol_dot_xdata),
		length = R_ExternalPtrProtected(x);
	if (TYPEOF(length) != INTSXP)
		return INVALID(_("type of protected field is not \"%s\""), "integer");
#ifdef R_FLINT_ABI_64
#define NPROTECTED 2
#else
#define NPROTECTED 1
#endif
	if (XLENGTH(length) != NPROTECTED)
		return INVALID(_("length of protected field is not %d"), NPROTECTED);
#undef NPROTECTED
	unsigned long int n;
	uucopy(&n, (unsigned int *) INTEGER_RO(length));
	if ((R_ExternalPtrAddr(x) == 0) != (n == 0))
		return INVALID((n == 0)
		               ? _("object length is zero and pointer field is nonzero")
		               : _("object length is nonzero and pointer field is zero"));
	unsigned long int n_ = (unsigned long int) XLENGTH(R_do_slot(object, R_flint_symbol_names));
	if (n_ != 0 && n_ != n)
		return INVALID(_("object length and '%s' slot length are not equal"), "names");
	return Rf_ScalarLogical(1);
#undef INVALID
}
