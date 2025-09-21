#include "flint.h"

#define R_FLINT_SWITCH(class, template) \
do { \
	switch (class) { \
	case R_FLINT_CLASS_ULONG: \
		template(ulong, ulong, const ulong *, ulong *); \
		break; \
	case R_FLINT_CLASS_SLONG: \
		template(slong, slong, const slong *, slong *); \
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

#define ulong_zero(rop) *(rop) = 0
#define slong_zero(rop) *(rop) = 0
#define ulong_set(rop, op) *(rop) = *(op)
#define slong_set(rop, op) *(rop) = *(op)
#define ulong_equal(rop, op) (*(rop) == *(op))
#define slong_equal(rop, op) (*(rop) == *(op))
#define ulong_conj(rop, op) ulong_set(rop, op)
#define slong_conj(rop, op) slong_set(rop, op)
#define fmpz_conj(rop, op) fmpz_set(rop, op)
#define fmpq_conj(rop, op) fmpq_set(rop, op)
#define mag_conj(rop, op) mag_set(rop, op)
#define arf_conj(rop, op) arf_set(rop, op)
#define arb_conj(rop, op) arb_set(rop, op)

#ifndef HAVE_ACF_ZERO
static R_INLINE
void acf_zero(acf_t res)
{
	arf_zero(acf_realref(res));
	arf_zero(acf_imagref(res));
	return;
}
#endif

#ifndef HAVE_ACF_CONJ
static R_INLINE
void acf_conj(acf_t res, const acf_t x)
{
	arf_set(acf_realref(res), acf_realref(x));
	arf_neg(acf_imagref(res), acf_imagref(x));
	return;
}
#endif

#if __FLINT_RELEASE < 30100
FLINT_NORETURN
#endif
void R_flint_abort(void)
{
	Rf_error(_("caught exception in libflint"));
#if __FLINT_RELEASE >= 30100
	return;
#endif
}

void *R_flint_get_pointer(SEXP object)
{
	SEXP x = R_do_slot(object, R_flint_symbol_dot_xdata);
	void *p = R_ExternalPtrAddr(x);
	return p;
}

mp_limb_t R_flint_get_length(SEXP object)
{
	SEXP x = R_do_slot(object, R_flint_symbol_dot_xdata),
		length = R_ExternalPtrProtected(x);
	mp_limb_t n;
	uucopy(&n, (const unsigned int *) INTEGER_RO(length));
	return n;
}

R_flint_class_t R_flint_get_class(SEXP object)
{
	int i = (TYPEOF(object) == OBJSXP)
		? R_check_class_etc(object, R_flint_classes) : -1;
	return (R_flint_class_t) i;
}

void R_flint_set(SEXP object,
                 void *p, mp_limb_t n, R_CFinalizer_t f)
{
	SEXP length = PROTECT(Rf_allocVector(INTSXP, 2));
	ucopy((unsigned int *) INTEGER(length), &n);
	SEXP x = PROTECT(R_MakeExternalPtr(p, R_NilValue, length));
	R_RegisterCFinalizer(x, f);
	R_do_slot_assign(object, R_flint_symbol_dot_xdata, x);
	UNPROTECT(2);
	return;
}

SEXP R_flint_bind(SEXP s_op, SEXP s_usenames, SEXP args, SEXP exps)
{
	int op = INTEGER_RO(s_op)[0], usenames;

	if (op == 2) { /* c */

		if (XLENGTH(s_usenames) == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "use.names", "c.flint");
		usenames = LOGICAL_RO(s_usenames)[0];

	} else { /* cbind, rbind */

		if (XLENGTH(s_usenames) == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "deparse.level", (op == 1) ? "cbind.flint" : "rbind.flint");
		usenames = INTEGER_RO(s_usenames)[0];
		if (usenames <= -1 || usenames > 2)
			Rf_error(_("'%s' is not 0, 1, or 2"),
			         "deparse.level");

	}

	R_xlen_t jargs, nargs = XLENGTH(args);
	if (nargs == 0)
		return R_NilValue;
	exps = CDR(exps);

	SEXP x, e;
	R_flint_class_t class = R_flint_get_class(VECTOR_ELT(args, 0));
	void *y = (void *) 0;
	mp_limb_t jx, jy = 0, nx, ny = 0;
	R_CFinalizer_t f = (void *) 0;
	const char *what;

	if (op == 2) { /* c */

		int anynames = !usenames;
		for (jargs = 0, e = exps; jargs < nargs; ++jargs, e = CDR(e)) {
			x = VECTOR_ELT(args, jargs);
			nx = R_flint_get_length(x);
			if (nx > UWORD_MAX - ny)
				Rf_error(_("value length would exceed maximum %llu"),
				         (unsigned long long int) UWORD_MAX);
			if (!anynames &&
			    (TAG(e) != R_NilValue ||
			     R_do_slot(x, R_flint_symbol_names) != R_NilValue))
				anynames = 1;
			ny += nx;
		}
		anynames = usenames && anynames && ny <= R_XLEN_T_MAX;

#define TEMPLATE(name, elt_t, xptr_t, yptr_t) \
		do { \
			xptr_t x__; \
			yptr_t y__ = (ny) ? flint_calloc(ny, sizeof(elt_t)) : 0; \
			y = y__; \
			f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
			what = #name; \
			for (jargs = 0; jargs < nargs; ++jargs) { \
				x = VECTOR_ELT(args, jargs); \
				nx = R_flint_get_length(x); \
				x__ = R_flint_get_pointer(x); \
				for (jx = 0; jx < nx; ++jx, ++jy) \
					name##_set(y__ + jy, x__ + jx); \
			} \
		} while (0)

		R_FLINT_SWITCH(class, TEMPLATE);

#undef TEMPLATE

		SEXP ans = PROTECT(newObject(what));
		R_flint_set(ans, y, ny, f);

		if (anynames) {

		SEXP namesx,
			namesy = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t) ny));
		char buf[8192];
		const char *sa, *sb;
		size_t la, lb;

		jy = 0;
		for (jargs = 0, e = exps; jargs < nargs; ++jargs, e = CDR(e)) {
			x = VECTOR_ELT(args, jargs);
			nx = R_flint_get_length(x);
			namesx = R_do_slot(x, R_flint_symbol_names);
			if (TAG(e) == R_NilValue) {
				if (namesx == R_NilValue)
					jy += nx;
				else
					for (jx = 0; jx < nx; ++jx, ++jy)
						SET_STRING_ELT(namesy, (R_xlen_t) jy,
						               STRING_ELT(namesx, (R_xlen_t) jx));
			} else {
				sa = Rf_translateCharUTF8(PRINTNAME(TAG(e)));
				la = strlen(sa);
				if (namesx == R_NilValue)
					for (jx = 0; jx < nx; ++jx, ++jy) {
						lb = (size_t) log10((double) (jx + 1)) + 1;
						snprintf(buf, la + lb + 1, "%s%llu",
						         sa, (unsigned long long int) (jx + 1));
						SET_STRING_ELT(namesy, (R_xlen_t) jy,
						               Rf_mkCharCE(buf, CE_UTF8));
					}
				else
					for (jx = 0; jx < nx; ++jx, ++jy) {
						sb = Rf_translateCharUTF8(STRING_ELT(namesx, (R_xlen_t) jx));
						lb = strlen(sb);
						snprintf(buf, la + lb + 1, "%s.%s",
						         sa, sb);
						SET_STRING_ELT(namesy, (R_xlen_t) jy,
						               Rf_mkCharCE(buf, CE_UTF8));
					}
			}
		}

		R_do_slot_assign(ans, R_flint_symbol_names, namesy);
		UNPROTECT(1);

		}

		UNPROTECT(1);
		return ans;

	} else { /* cbind, rbind */

		SEXP dimx, dimy = PROTECT(Rf_allocVector(INTSXP, 2));
		const int *dx;
		int *dy = INTEGER(dimy), i, j;
		dy[ op] =  0;
		dy[!op] = -1;

		R_xlen_t nnull = 0;
		for (jargs = 0; jargs < nargs; ++jargs) {
			x = VECTOR_ELT(args, jargs);
			dimx = R_do_slot(x, R_flint_symbol_dim);
			if (dimx != R_NilValue && XLENGTH(dimx) == 2) {
				dx = INTEGER_RO(dimx);
				if (dx[op] > INT_MAX - dy[op])
					Rf_error(_("dimensions would exceed maximum %d"),
					         INT_MAX);
				dy[op] += dx[op];
				if (dx[!op] != dy[!op]) {
					if (dy[!op] >= 0) {
						if (op == 1)
						Rf_error(_("number of rows of matrix arguments must match"));
						else
						Rf_error(_("number of columns of matrix arguments must match"));
					}
					dy[!op] = dx[!op];
				}
			} else {
				nx = R_flint_get_length(x);
				if (nx == 0)
					++nnull;
				else {
					if (dy[op] == INT_MAX)
						Rf_error(_("dimensions would exceed maximum %d"),
						         INT_MAX);
					dy[op] += 1;
					if (nx > ny)
						ny = nx;
				}
			}
		}
		if (dy[!op] == -1) {
			if (ny > INT_MAX)
				Rf_error(_("dimensions would exceed maximum %d"),
				         INT_MAX);
			dy[!op] = (int) ny;
		}
		if (dy[!op] ==  0) {
			if (nnull > INT_MAX - dy[op])
				Rf_error(_("dimensions would exceed maximum %d"),
				         INT_MAX);
			dy[ op] += (int) nnull;
		}
		if ((unsigned int) dy[0] > UWORD_MAX / (unsigned int) dy[1])
			Rf_error(_("value length would exceed maximum %llu"),
			         (unsigned long long int) UWORD_MAX);
		ny = (mp_limb_t) dy[0] * (mp_limb_t) dy[1];

		SEXP dimnamesx, namesx;
		int anynames[] = { 0, 0 };
		for (jargs = 0, e = exps; jargs < nargs; ++jargs, e = CDR(e)) {
			x = VECTOR_ELT(args, jargs);
			dimx = R_do_slot(x, R_flint_symbol_dim);
			if (dimx != R_NilValue && XLENGTH(dimx) == 2) {
				dimnamesx = R_do_slot(x, R_flint_symbol_dimnames);
				if (dimnamesx != R_NilValue) {
				if (!anynames[ op] && VECTOR_ELT(dimnamesx,  op) != R_NilValue)
					anynames[ op] = 1;
				if (!anynames[!op] && VECTOR_ELT(dimnamesx, !op) != R_NilValue)
					anynames[!op] = 1;
				}
			} else if ((nx = R_flint_get_length(x)) == dy[!op] || nx > 0) {
				if (!anynames[ op] &&
				    (TAG(e) != R_NilValue || usenames == 2 || (usenames == 1 && TYPEOF(CAR(e)) == SYMSXP)))
					anynames[ op] = 1;
				if (!anynames[!op] && nx == dy[!op] &&
				    R_do_slot(x, R_flint_symbol_names) != R_NilValue)
					anynames[!op] = 1;
			}
			if (anynames[0] && anynames[1])
				break;
		}

#define TEMPLATE(name, elt_t, xptr_t, yptr_t) \
		do { \
			xptr_t x__; \
			yptr_t y__ = (ny) ? flint_calloc(ny, sizeof(elt_t)) : 0; \
			y = y__; \
			f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
			what = #name; \
			for (jargs = 0; jargs < nargs; ++jargs) { \
				x = VECTOR_ELT(args, jargs); \
				x__ = R_flint_get_pointer(x); \
				dimx = R_do_slot(x, R_flint_symbol_dim); \
				if (dimx != R_NilValue && XLENGTH(dimx) == 2) { \
					dx = INTEGER_RO(dimx); \
					if (op == 1) { \
						nx = (mp_limb_t) dx[0] * (mp_limb_t) dx[1]; \
						for (jx = 0; jx < nx; ++jx, ++jy) \
							name##_set(y__ + jy, x__ + jx); \
					} else { \
						for (j = 0, jx = 0; j < dx[1]; ++j, jy += dy[0] - dx[0]) \
							for (i = 0; i < dx[0]; ++i, ++jx, ++jy) \
								name##_set(y__ + jy, x__ + jx); \
						jy -= ny - dx[0]; \
					} \
				} else if ((nx = R_flint_get_length(x)) == dy[!op]) { \
					if (op == 1) \
						for (jx = 0; jx < nx; ++jx, ++jy) \
							name##_set(y__ + jy, x__ + jx); \
					else { \
						for (jx = 0; jx < nx; ++jx, jy += dy[op]) \
							name##_set(y__ + jy, x__ + jx); \
						jy -= ny - 1; \
					} \
				} else if (nx == 1) { \
					if (op == 1) \
						for (i = 0; i < dy[0]; ++i, ++jy) \
							name##_set(y__ + jy, x__); \
					else { \
						for (j = 0; j < dy[1]; ++j, jy += dy[0]) \
							name##_set(y__ + jy, x__); \
						jy -= ny - 1; \
					} \
				} else if (nx > 0) { \
					if (op == 1) { \
						if (dy[0] % nx) \
							Rf_warning(_("number of rows of return value is not a multiple of vector argument length")); \
						for (i = 0, jx = 0; i < dy[0]; ++i, jx = (++jx == nx) ? 0 : jx, ++jy) \
							name##_set(y__ + jy, x__ + jx); \
					} else { \
						if (dy[1] % nx) \
							Rf_warning(_("number of columns of return value is not a multiple of vector argument length")); \
						for (j = 0, jx = 0; j < dy[1]; ++j, jx = (++jx == nx) ? 0 : jx, jy += dy[0]) \
							name##_set(y__ + jy, x__ + jx); \
						jy -= ny - 1; \
					} \
				} \
			} \
		} while (0)

		R_FLINT_SWITCH(class, TEMPLATE);

#undef TEMPLATE

		SEXP ans = PROTECT(newObject(what));
		R_flint_set(ans, y, ny, f);
		R_do_slot_assign(ans, R_flint_symbol_dim, dimy);

		if (anynames[0] || anynames[1]) {

			SEXP dimnamesy = PROTECT(Rf_allocVector(VECSXP, 2)),
				marnamesy[] = { R_NilValue, R_NilValue },
				tag = PROTECT(Rf_allocVector(EXPRSXP, 1));
			if (anynames[op]) {
				marnamesy[op] = Rf_allocVector(STRSXP, dy[op]);
				SET_VECTOR_ELT(dimnamesy, op, marnamesy[op]);
			}
			PROTECT(marnamesy[op]);

			SEXP t;
			int pos = 0;
			for (jargs = 0, e = exps; jargs < nargs; ++jargs, exps = CDR(exps)) {
				x = VECTOR_ELT(args, jargs);
				dimx = R_do_slot(x, R_flint_symbol_dim);
				if (dimx != R_NilValue && XLENGTH(dimx) == 2) {
					PROTECT(dimx);
					PROTECT(dimnamesx = R_do_slot(x, R_flint_symbol_dimnames));
					dx = INTEGER_RO(dimx);
					if (dimnamesx != R_NilValue) {
					if (anynames[ op]) {
						t = VECTOR_ELT(dimnamesx,  op);
						if (t != R_NilValue)
							for (i = 0; i < dx[op]; ++i)
								SET_STRING_ELT(marnamesy[op], pos++, STRING_ELT(t, i));
						else
							pos += dx[op];
					}
					if (anynames[!op] && marnamesy[!op] == R_NilValue) {
						t = VECTOR_ELT(dimnamesx, !op);
						if (t != R_NilValue) {
							marnamesy[!op] = t;
							SET_VECTOR_ELT(dimnamesy, !op, marnamesy[!op]);
						}
					}
					}
					UNPROTECT(2);
				} else if ((nx = R_flint_get_length(x)) == dy[!op] || nx > 0) {
					if (anynames[ op]) {
						if (TAG(e) != R_NilValue || usenames == 2 || (usenames == 1 && TYPEOF(CAR(e)) == SYMSXP)) {
							SET_VECTOR_ELT(tag, 0, (TAG(e) != R_NilValue) ? TAG(e) : CAR(e));
							SET_STRING_ELT(marnamesy[op], pos++, STRING_ELT(Rf_coerceVector(tag, STRSXP), 0));
						}
						else
							pos += 1;
					}
					if (anynames[!op] && nx == dy[!op] &&
					    marnamesy[!op] == R_NilValue) {
						namesx = R_do_slot(x, R_flint_symbol_names);
						if (namesx != R_NilValue) {
							marnamesy[!op] = namesx;
							SET_VECTOR_ELT(dimnamesy, !op, marnamesy[!op]);
						}
					}
				}
			}

			R_do_slot_assign(ans, R_flint_symbol_dimnames, dimnamesy);
			UNPROTECT(3);

		}

		UNPROTECT(2);
		return ans;

	}
}

SEXP R_flint_class(SEXP object)
{
	int i = (TYPEOF(object) == OBJSXP)
		? R_check_class_etc(object, R_flint_classes) : -1;
	SEXP ans = PROTECT(Rf_allocVector(STRSXP, 1));
	SET_STRING_ELT(ans, 0, (i < 0) ? NA_STRING : Rf_mkChar(R_flint_classes[i]));
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_diag(SEXP object, SEXP s_nrow, SEXP s_ncol)
{
	R_flint_class_t class = R_flint_get_class(object);
	const void *x = R_flint_get_pointer(object);
	void *y = (void *) 0;
	mp_limb_t jx = 0, jy = 0,
		nx = R_flint_get_length(object),
		ny = 0;
	R_CFinalizer_t f = (void *) 0;
	const char *what;

	if (s_nrow != R_NilValue && s_ncol != R_NilValue) {

		SEXP dimy = PROTECT(Rf_allocVector(INTSXP, 2));
		int *dy = INTEGER(dimy);
		if (XLENGTH(s_nrow) == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "nrow", "diag");
		if ((dy[0] = INTEGER_RO(s_nrow)[0]) == NA_INTEGER || dy[0] < 0)
			Rf_error(_("'%s' is not a non-negative integer"),
			         "nrow");
		if (XLENGTH(s_ncol) == 0)
			Rf_error(_("'%s' of length zero in '%s'"),
			         "ncol", "diag");
		if ((dy[1] = INTEGER_RO(s_ncol)[0]) == NA_INTEGER || dy[1] < 0)
			Rf_error(_("'%s' is not a non-negative integer"),
			         "ncol");
		int i, j, k = (dy[0] < dy[1]) ? dy[0] : dy[1];
		if (k > 0) {
		if ((unsigned int) dy[0] > UWORD_MAX / (unsigned int) dy[1])
			Rf_error(_("value length would exceed maximum %llu"),
			         (unsigned long long int) UWORD_MAX);
		if (nx == 0)
			Rf_error(_("'%s' of length zero cannot be recycled to nonzero length"),
			         "x");
		ny = (mp_limb_t) dy[0] * (mp_limb_t) dy[1];
		}

#define TEMPLATE(name, elt_t, xptr_t, yptr_t) \
		do { \
			xptr_t x__ = x; \
			yptr_t y__ = (ny) ? flint_calloc(ny, sizeof(elt_t)) : 0; \
			y = y__; \
			f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
			what = #name; \
			for (j = 0; j < k; ++j, jx = (++jx == nx) ? 0 : jx) { \
				for (i = 0; i < j; ++i, ++jy) \
					name##_zero(y__ + jy); \
				name##_set(y__ + jy, x__ + jx); \
				++i; ++jy; \
				for (; i < dy[0]; ++i, ++jy) \
					name##_zero(y__ + jy); \
			} \
			for (; j < dy[1]; ++j) \
				for (i = 0; i < dy[0]; ++i) \
					name##_zero(y__ + jy); \
		} while (0)

		R_FLINT_SWITCH(class, TEMPLATE);

#undef TEMPLATE

		SEXP ans = PROTECT(newObject(what));
		R_flint_set(ans, y, ny, f);
		R_do_slot_assign(ans, R_flint_symbol_dim, dimy);
		UNPROTECT(2);
		return ans;

	} else {

		SEXP dimx = R_do_slot(object, R_flint_symbol_dim);
		const int *dx = INTEGER_RO(dimx);
		int j, k = (dx[0] < dx[1]) ? dx[0] : dx[1];
		mp_limb_t off = (unsigned int) dx[0] + 1;
		ny = (mp_limb_t) k;

#define TEMPLATE(name, elt_t, xptr_t, yptr_t) \
		do { \
			xptr_t x__ = x; \
			yptr_t y__ = (ny) ? flint_calloc(ny, sizeof(elt_t)) : 0; \
			y = y__; \
			f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
			what = #name; \
			for (j = 0; j < k; ++j, ++jy, jx += off) \
				name##_set(y__ + jy, x__ + jx); \
		} while (0)

		R_FLINT_SWITCH(class, TEMPLATE);

#undef TEMPLATE

		SEXP ans = PROTECT(newObject(what));
		R_flint_set(ans, y, ny, f);
		UNPROTECT(1);
		return ans;

	}
}

SEXP R_flint_find_interval(SEXP object, SEXP breaks,
                           SEXP s_left_open,
                           SEXP s_rightmost_closed,
                           SEXP s_all_inside)
{
	if (XLENGTH(s_left_open) == 0)
		Rf_error(_("'%s' of length zero in '%s'"),
		         "left.open", "findInterval");
	if (XLENGTH(s_rightmost_closed) == 0)
		Rf_error(_("'%s' of length zero in '%s'"),
		         "rightmost.closed", "findInterval");
	if (XLENGTH(s_all_inside) == 0)
		Rf_error(_("'%s' of length zero in '%s'"),
		         "all.inside", "findInterval");
	int left_open = LOGICAL_RO(s_left_open)[0],
		rightmost_closed = LOGICAL_RO(s_rightmost_closed)[0],
		all_inside = LOGICAL_RO(s_all_inside)[0],
		chk;
	R_flint_class_t class = R_flint_get_class(object);
	const void
		*x = R_flint_get_pointer(object),
		*b = R_flint_get_pointer(breaks);
	mp_limb_t j,
		nx = R_flint_get_length(object),
		nb = R_flint_get_length(breaks),
		ny = nx,
		ilo = 0, ihi, iav, ist, iof = (all_inside) ? 1 : 0;
	SEXP ans = PROTECT(newObject("ulong"));
	ulong *y = (ny) ? flint_calloc(ny, sizeof(ulong)) : 0;
	R_flint_set(ans, y, ny, (R_CFinalizer_t) &R_flint_ulong_finalize);

#define ulong_cmp_l(x, jx, b, jb)                x[jx] < b[jb]
#define slong_cmp_l(x, jx, b, jb)                x[jx] < b[jb]
#define  fmpz_cmp_l(x, jx, b, jb) fmpz_cmp(x + jx, b + jb) < 0
#define  fmpq_cmp_l(x, jx, b, jb) fmpq_cmp(x + jx, b + jb) < 0
#define   mag_cmp_l(x, jx, b, jb)  mag_cmp(x + jx, b + jb) < 0
#define   arf_cmp_l(x, jx, b, jb)  arf_cmp(x + jx, b + jb) < 0

#define ulong_cmp_le(x, jx, b, jb)                x[jx] <= b[jb]
#define slong_cmp_le(x, jx, b, jb)                x[jx] <= b[jb]
#define  fmpz_cmp_le(x, jx, b, jb) fmpz_cmp(x + jx, b + jb) <= 0
#define  fmpq_cmp_le(x, jx, b, jb) fmpq_cmp(x + jx, b + jb) <= 0
#define   mag_cmp_le(x, jx, b, jb)  mag_cmp(x + jx, b + jb) <= 0
#define   arf_cmp_le(x, jx, b, jb)  arf_cmp(x + jx, b + jb) <= 0

#define TEMPLATE(name, xptr_t) \
	do { \
		xptr_t x__ = x; \
		xptr_t b__ = b; \
		--b__; \
		if (left_open) { \
		for (j = 0; j < nx; ++j) { \
			if (ilo == 0) { \
				if (name##_cmp_le(x__, j, b__, 1)) { \
					y[j] = (all_inside || (rightmost_closed && !(name##_cmp_l(x__, j, b__, 1)))) ? 1 : 0; \
					continue; \
				} \
				ilo = 1; \
			} \
			if ((ihi = ilo + 1) >= nb) { \
				if (name##_cmp_l(b__, nb, x__, j)) { \
					y[j] = nb - iof; \
					continue; \
				} \
				ilo = nb - 1; \
				ihi = nb; \
			} \
			if (name##_cmp_le(x__, j, b__, ihi)) { \
				if (name##_cmp_l(b__, ilo, x__, j)) { \
					y[j] = ilo; \
					continue; \
				} \
				ist = 1; \
				chk = 1; \
				while (ist && ist < ilo) { \
					ihi = ilo; \
					ilo -= ist; \
					if (name##_cmp_l(b__, ilo, x__, j)) { \
						chk = 0; \
						break; \
					} \
					ist = ist << 1; \
				} \
				if (chk) { \
					ihi = ilo; \
					if (name##_cmp_le(x__, j, b__, 1)) { \
						y[j] = (all_inside || (rightmost_closed && !(name##_cmp_l(x__, j, b__, 1)))) ? 1 : 0; \
						ilo = 0; \
						continue; \
					} \
					ilo = 1; \
				} \
			} else { \
				ist = 1; \
				chk = 1; \
				while (ist && ist < nb - ihi) { \
					ilo = ihi; \
					ihi += ist; \
					if (name##_cmp_le(x__, j, b__, ihi)) { \
						chk = 0; \
						break; \
					} \
					ist = ist << 1; \
				} \
				if (chk) { \
					ilo = ihi; \
					if (name##_cmp_l(b__, nb, x__, j)) { \
						y[j] = nb - iof; \
						ihi = nb; \
						continue; \
					} \
					ihi = nb - 1; \
				} \
			} \
			while (1) { \
				iav = ilo + (ihi - ilo) / 2; \
				if (iav == ilo) { \
					y[j] = ilo; \
					break; \
				} \
				else if (name##_cmp_l(b__, iav, x__, j)) \
					ilo = iav; \
				else \
					ihi = iav; \
			} \
		} \
		} else { \
		for (j = 0; j < nx; ++j) { \
			if (ilo == 0) { \
				if (name##_cmp_l(x__, j, b__, 1)) { \
					y[j] = iof; \
					continue; \
				} \
				ilo = 1; \
			} \
			if ((ihi = ilo + 1) >= nb) { \
				if (name##_cmp_le(b__, nb, x__, j)) { \
					y[j] = (all_inside || (rightmost_closed && !(name##_cmp_l(b__, nb, x__, j)))) ? nb - 1 : nb; \
					continue; \
				} \
				ilo = nb - 1; \
				ihi = nb; \
			} \
			if (name##_cmp_l(x__, j, b__, ihi)) { \
				if (name##_cmp_le(b__, ilo, x__, j)) { \
					y[j] = ilo; \
					continue; \
				} \
				ist = 1; \
				chk = 1; \
				while (ist && ist < ilo) { \
					ihi = ilo; \
					ilo -= ist; \
					if (name##_cmp_le(b__, ilo, x__, j)) { \
						chk = 0; \
						break; \
					} \
					ist = ist << 1; \
				} \
				if (chk) { \
					ihi = ilo; \
					if (name##_cmp_l(x__, j, b__, 1)) { \
						y[j] = iof; \
						ilo = 0; \
						continue; \
					} \
					ilo = 1; \
				} \
			} else { \
				ist = 1; \
				chk = 1; \
				while (ist && ist < nb - ihi) { \
					ilo = ihi; \
					ihi += ist; \
					if (name##_cmp_l(x__, j, b__, ihi)) { \
						chk = 0; \
						break; \
					} \
					ist = ist << 1; \
				} \
				if (chk) { \
					ilo = ihi; \
					if (name##_cmp_le(b__, nb, x__, j)) { \
						y[j] = (all_inside || (rightmost_closed && !(name##_cmp_l(b__, nb, x__, j)))) ? nb - 1 : nb; \
						ihi = nb; \
						continue; \
					} \
					ihi = nb - 1; \
				} \
			} \
			while (1) { \
				iav = ilo + (ihi - ilo) / 2; \
				if (iav == ilo) { \
					y[j] = ilo; \
					break; \
				} \
				else if (name##_cmp_le(b__, iav, x__, j)) \
					ilo = iav; \
				else \
					ihi = iav; \
			} \
		} \
		} \
	} while (0)

	switch (class) {
	case R_FLINT_CLASS_ULONG:
		TEMPLATE(ulong, const ulong *);
		break;
	case R_FLINT_CLASS_SLONG:
		TEMPLATE(slong, const slong *);
		break;
	case R_FLINT_CLASS_FMPZ:
		TEMPLATE(fmpz, const fmpz *);
		break;
	case R_FLINT_CLASS_FMPQ:
		TEMPLATE(fmpq, const fmpq *);
		break;
	case R_FLINT_CLASS_MAG:
		TEMPLATE(mag, mag_srcptr);
		break;
	case R_FLINT_CLASS_ARF:
		TEMPLATE(arf, arf_srcptr);
		break;
	default:
		Rf_error(_("should never happen ..."));
	}

#undef TEMPLATE

#undef ulong_cmp_l
#undef slong_cmp_l
#undef  fmpz_cmp_l
#undef  fmpq_cmp_l
#undef   mag_cmp_l
#undef   arf_cmp_l

#undef ulong_cmp_le
#undef slong_cmp_le
#undef  fmpz_cmp_le
#undef  fmpq_cmp_le
#undef   mag_cmp_le
#undef   arf_cmp_le

	UNPROTECT(1);
	return ans;
}

SEXP R_flint_identical(SEXP object, SEXP reference)
{
	R_flint_class_t class = R_flint_get_class(reference);
	if (class == R_FLINT_CLASS_INVALID)
		return Rf_ScalarLogical(R_compute_identical(object, reference, 16) != 0);
	if (R_flint_get_class(object) != class)
		return Rf_ScalarLogical(0);
	mp_limb_t j, n = R_flint_get_length(reference);
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

	SEXP ax, ay, symbol[3] = { R_flint_symbol_dim, R_flint_symbol_dimnames, R_flint_symbol_names };
	for (j = 0; j < 3; ++j) {
		PROTECT(ax = R_do_slot(   object, symbol[j]));
		PROTECT(ay = R_do_slot(reference, symbol[j]));
		int t = R_compute_identical(ax, ay, 16);
		UNPROTECT(2);
		if (!t) return Rf_ScalarLogical(0);
	}

	return Rf_ScalarLogical(1);
}

SEXP R_flint_length(SEXP object, SEXP s_exact)
{
	R_flint_class_t class = R_flint_get_class(object);
	if (class == R_FLINT_CLASS_INVALID)
		return Rf_ScalarInteger(NA_INTEGER);
	if (XLENGTH(s_exact) == 0)
		Rf_error(_("'%s' of length zero in '%s'"),
		         "exact", "flintLength");
	int exact = LOGICAL_RO(s_exact)[0];
	mp_limb_t n = R_flint_get_length(object);
	SEXP ans;
	if (exact) {
		PROTECT(ans = newObject("ulong"));
		ulong *p = flint_calloc(1, sizeof(ulong));
		R_flint_set(ans, p, 1, (R_CFinalizer_t) &R_flint_ulong_finalize);
		p[0] = n;
	} else if (n <= INT_MAX) {
		PROTECT(ans = Rf_allocVector(INTSXP, 1));
		INTEGER(ans)[0] = (int) n;
	} else {
		mp_limb_t n_ = (mp_limb_t) (double) n;
		if (n_ >  n)
			n_ = (mp_limb_t) nextafter((double) n, 0.0);
		PROTECT(ans = Rf_allocVector(REALSXP, 1));
		REAL(ans)[0] = (double) n_;
		if (n_ != n) {
			SEXP off = PROTECT(Rf_allocVector(INTSXP, 1));
			INTEGER(off)[0] = (int) (n - n_);
			Rf_setAttrib(ans, R_flint_symbol_off, off);
#if 0
			Rf_warning(_("true length %lu truncated to %lu"), n, n_);
#endif
			UNPROTECT(1);
		}
	}
	UNPROTECT(1);
	return ans;
}

SEXP R_flint_length_assign(SEXP object, SEXP s_lengthout)
{
	int usenames = 1;
	R_flint_class_t class = R_flint_get_class(object);
	const void *x = R_flint_get_pointer(object);
	void *y = (void *) 0;
	mp_limb_t j,
		nx = R_flint_get_length(object),
		ny = 0,
		n0 = 0;
	R_CFinalizer_t f = (void *) 0;
	const char *what;

	if (R_flint_get_length(s_lengthout) != 1)
		Rf_error(_("length of '%s' is not equal to 1 in '%s'"),
		         "value", "length<-");
	ny = ((ulong *) R_flint_get_pointer(s_lengthout))[0];
	n0 = (nx < ny) ? nx : ny;

	SEXP sx = PROTECT(R_do_slot(object, R_flint_symbol_names)),
		sy = R_NilValue;
	usenames = usenames && sx != R_NilValue && ny <= R_XLEN_T_MAX;
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

/* FIXME: LISTSXP and VECSXP keeping names for strict compatibility */
SEXP R_flint_list(SEXP object, SEXP s_type)
{
	const char *type = CHAR(STRING_ELT(s_type, 0));
	R_flint_class_t class = R_flint_get_class(object);
	const void *x = R_flint_get_pointer(object);
	mp_limb_t j, n = R_flint_get_length(object);
	ERROR_TOO_LONG(n, (type[0] == 'p') ? INT_MAX : R_XLEN_T_MAX);
	PROTECT_INDEX pid;
	SEXP ans = (type[0] == 'p') ? Rf_allocList((int) n) : Rf_allocVector((type[0] == 'l') ? VECSXP : EXPRSXP, (R_xlen_t) n),
		tmp = R_NilValue;
	PROTECT(ans);
	PROTECT_WITH_INDEX(tmp, &pid);

#define TEMPLATE(name, elt_t, xptr_t, yptr_t) \
	do { \
		xptr_t x__ = x; \
		yptr_t y__; \
		if (type[0] == 'p') { \
			SEXP a = ans; \
			for (j = 0; j < n; ++j) { \
				y__ = flint_calloc(1, sizeof(elt_t)); \
				name##_set(y__, x__ + j); \
				REPROTECT(tmp = newObject(#name), pid); \
				R_flint_set(tmp, y__, 1, (R_CFinalizer_t) &R_flint_##name##_finalize); \
				SETCAR(a, tmp); \
				a = CDR(a); \
			} \
		} else { \
			for (j = 0; j < n; ++j) { \
				y__ = flint_calloc(1, sizeof(elt_t)); \
				name##_set(y__, x__ + j); \
				REPROTECT(tmp = newObject(#name), pid); \
				R_flint_set(tmp, y__, 1, (R_CFinalizer_t) &R_flint_##name##_finalize); \
				SET_VECTOR_ELT(ans, (R_xlen_t) j, tmp); \
			} \
		} \
	} while (0)

	R_FLINT_SWITCH(class, TEMPLATE);

#undef TEMPLATE

	UNPROTECT(2);
	return ans;
}

SEXP R_flint_new(SEXP class)
{
	return newObject(CHAR(STRING_ELT(class, 0)));
}

SEXP R_flint_rep_each(SEXP object, SEXP s_each, SEXP s_usenames)
{
	int usenames = LOGICAL_RO(s_usenames)[0];
	R_flint_class_t class = R_flint_get_class(object);
	const void *x = R_flint_get_pointer(object);
	void *y = (void *) 0;
	mp_limb_t jx, jy = 0,
		nx = R_flint_get_length(object),
		ny = 0;
	R_CFinalizer_t f = (void *) 0;
	const char *what;

	if (R_flint_get_length(s_each) != 1)
		Rf_error(_("length of '%s' is not equal to 1 in '%s'"),
		         "each", "rep");
	ulong i, each = ((ulong *) R_flint_get_pointer(s_each))[0];
	if (each > 0 && nx > UWORD_MAX / each)
		Rf_error(_("value length would exceed maximum %llu"),
		         (unsigned long long int) UWORD_MAX);
	ny = nx * each;;

	SEXP sx = PROTECT(R_do_slot(object, R_flint_symbol_names)),
		sy = R_NilValue, nm;
	usenames = usenames && sx != R_NilValue && ny <= R_XLEN_T_MAX;
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
	void *y = (void *) 0;
	mp_limb_t q, r, i, jx, jy = 0,
		nx = R_flint_get_length(object),
		ny = 0;
	R_CFinalizer_t f = (void *) 0;
	const char *what;

	if (R_flint_get_length(s_lengthout) != 1)
		Rf_error(_("length of '%s' is not equal to 1 in '%s'"),
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
	usenames = usenames && sx != R_NilValue && ny <= R_XLEN_T_MAX;
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
	void *y = (void *) 0;
	mp_limb_t jx, jy = 0,
		nx = R_flint_get_length(object),
		ny = 0;
	R_CFinalizer_t f = (void *) 0;
	const char *what;

	mp_limb_t ntimes = R_flint_get_length(s_times);
	if (ntimes != 1 && ntimes != nx)
		Rf_error(_("length of '%s' is not equal to 1 or length(%s) in '%s'"),
		         "times", "x", "rep");
	ulong i, t;
	const ulong *times = R_flint_get_pointer(s_times);
	if (ntimes == 1) {
		t = times[0];
		if (t > 0 && nx > UWORD_MAX / t)
			Rf_error(_("value length would exceed maximum %llu"),
			         (unsigned long long int) UWORD_MAX);
		ny = nx * t;
	} else {
		for (jx = 0; jx < nx; ++jx) {
			t = times[jx];
			if (t > UWORD_MAX - ny)
				Rf_error(_("value length would exceed maximum %llu"),
				         (unsigned long long int) UWORD_MAX);
			ny += t;
		}
	}

	SEXP sx = PROTECT(R_do_slot(object, R_flint_symbol_names)),
		sy = R_NilValue, nm;
	usenames = usenames && sx != R_NilValue && ny <= R_XLEN_T_MAX;
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
				nm = STRING_ELT(sx, (R_xlen_t) jx); \
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
	if (class == R_FLINT_CLASS_INVALID)
		return Rf_ScalarReal(NA_REAL);
	const void *x = R_flint_get_pointer(object);
	mp_limb_t j, n = R_flint_get_length(object);
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
	case R_FLINT_CLASS_ULONG:
		count += n * sizeof(ulong);
		break;
	case R_FLINT_CLASS_SLONG:
		count += n * sizeof(slong);
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
		break;
	}

#undef SIZE_CASE

	return Rf_ScalarReal((double) count);
}

SEXP R_flint_subassign(SEXP object, SEXP subscript, SEXP s_op,
                       SEXP value)
{
	R_flint_class_t class = R_flint_get_class(object);
	const void
		*v = R_flint_get_pointer(value),
		*x = R_flint_get_pointer(object);
	void *y = (void *) 0;
	mp_limb_t jx, jy,
		nx = R_flint_get_length(object),
		ny = nx;
	R_CFinalizer_t f = (void *) 0;
	const char *what;

	int op = INTEGER_RO(s_op)[0];
	switch (op) {
	case 2: /* [[<- */
	{
		switch (TYPEOF(subscript)) {
		case INTSXP:
			jx = (mp_limb_t) (INTEGER_RO(subscript)[0] - 1);
			break;
		case REALSXP:
			jx = (mp_limb_t) REAL_RO(subscript)[0] - 1;
			break;
		case OBJSXP:
			jx = ((ulong *) R_flint_get_pointer(object))[0] - 1;
			break;
		case VECSXP:
		{
			SEXP elt, dimx = R_do_slot(object, R_flint_symbol_dim);
			const int *dx = INTEGER_RO(dimx);
			int k, ndx = LENGTH(dimx);
			mp_limb_t t = 1;
			jx = 0;
			for (k = 0; k < ndx; ++k) {
				elt = VECTOR_ELT(subscript, k);
				switch (TYPEOF(elt)) {
				case INTSXP:
					jx += t * (mp_limb_t) (INTEGER_RO(elt)[0] - 1);
					break;
				case REALSXP:
					jx += t * ((mp_limb_t) REAL_RO(elt)[0] - 1);
					break;
				case OBJSXP:
					jx += t * (((ulong *) R_flint_get_pointer(elt))[0] - 1);
					break;
				default:
					Rf_error(_("should never happen ..."));
					break;
				}
				t *= (mp_limb_t) dx[k];
			}
			break;
		}
		default:
			Rf_error(_("should never happen ..."));
			jx = 0;
			break;
		}

#define TEMPLATE(name, elt_t, xptr_t, yptr_t) \
		do { \
			xptr_t v__ = v; \
			xptr_t x__ = x; \
			yptr_t y__ = (ny) ? flint_calloc(ny, sizeof(elt_t)) : 0; \
			y = y__; \
			f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
			what = #name; \
			for (jy = 0; jy < ny; ++jy) \
				name##_set(y__ + jy, x__ + jy); \
			name##_set(y__ + jx, v__); \
		} while (0)

		R_FLINT_SWITCH(class, TEMPLATE);

#undef TEMPLATE

		SEXP ans = PROTECT(newObject(what));
		R_flint_set(ans, y, ny, f);
		setDDNN1(ans, object);
		UNPROTECT(1);
		return ans;
	}
	case 1: /*  [<- */
	{
		mp_limb_t js, jv = 0, ns, nv = R_flint_get_length(value);
		if (TYPEOF(subscript) != VECSXP) {

			switch (TYPEOF(subscript)) {
			case INTSXP:
			case REALSXP:
				ns = (mp_limb_t) XLENGTH(subscript);
				break;
			case OBJSXP:
				ns = R_flint_get_length(subscript);
				break;
			default:
				if (subscript != R_flint_symbol_missing)
					Rf_error(_("should never happen ..."));
				ns = nx;
				break;
			}
			if (ns > 0) {
			if (nv == 0)
				Rf_error(_("should never happen ..."));
			if (ns % nv > 0)
				Rf_warning(_("number of elements to replace is not a multiple of replacement length"));
			}

#define TEMPLATE(name, elt_t, xptr_t, yptr_t) \
			do { \
				xptr_t v__ = v; \
				xptr_t x__ = x; \
				yptr_t y__ = (ny) ? flint_calloc(ny, sizeof(elt_t)) : 0; \
				y = y__; \
				f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
				what = #name; \
				if (subscript != R_flint_symbol_missing) \
				for (jy = 0; jy < ny; ++jy) \
					name##_set(y__ + jy, x__ + jy); \
				switch (TYPEOF(subscript)) { \
				case INTSXP: \
				{ \
					const int *s__ = INTEGER_RO(subscript); \
					for (js = 0; js < ns; ++js, jv = (++jv >= nv) ? 0 : jv) \
						name##_set(y__ + (s__[js] - 1), v__ + jv); \
					break; \
				} \
				case REALSXP: \
				{ \
					const double *s__ = REAL_RO(subscript); \
					for (js = 0; js < ns; ++js, jv = (++jv >= nv) ? 0 : jv) \
						name##_set(y__ + ((mp_limb_t) s__[js] - 1), v__ + jv); \
					break; \
				} \
				case OBJSXP: \
				{ \
					const ulong *s__ = R_flint_get_pointer(subscript); \
					for (js = 0; js < ns; ++js, jv = (++jv >= nv) ? 0 : jv) \
						name##_set(y__ + (s__[js] - 1), v__ + jv); \
					break; \
				} \
				default: \
					for (js = 0; js < ns; ++js, jv = (++jv >= nv) ? 0 : jv) \
						name##_set(y__ + js, v__ + jv); \
					break; \
				} \
			} while (0)

			R_FLINT_SWITCH(class, TEMPLATE);

#undef TEMPLATE

			SEXP ans = PROTECT(newObject(what));
			R_flint_set(ans, y, ny, f);
			setDDNN1(ans, object);
			UNPROTECT(1);
			return ans;

		} else {

			SEXP elt, dimx = PROTECT(R_do_slot(object, R_flint_symbol_dim));
			const int *dx = INTEGER_RO(dimx);
			int k, ndx = LENGTH(dimx);
			int *ds = (void *) R_alloc((size_t) ndx, sizeof(mp_limb_t));
			mp_limb_t r, s = 0, t = 1;
			int over = 0;
			for (k = 0; k < ndx; ++k) {
				elt = VECTOR_ELT(subscript, k);
				switch (TYPEOF(elt)) {
				case INTSXP:
				case REALSXP:
					r = (mp_limb_t) XLENGTH(elt);
					break;
				case OBJSXP:
					r = R_flint_get_length(elt);
					break;
				default:
					if (elt != R_flint_symbol_missing)
						Rf_error(_("should never happen ..."));

					r = (mp_limb_t) dx[k];
					break;
				}
				if (r > 0) {
				if (r > INT_MAX)
					Rf_error(_("dimensions would exceed maximum %d"),
					         INT_MAX);
				if (!over && t > UWORD_MAX / r)
					over = 1;
				}
				s += r;
				t *= r;
				ds[k] = (int) r;
			}

			mp_limb_t
				*work = (void *) R_alloc(s, sizeof(mp_limb_t)),
				**ptr = (void *) R_alloc((size_t) ndx, sizeof(mp_limb_t *));
			int *pos = (void *) R_alloc((size_t) ndx, sizeof(int)), l;
			ns = t; t = 1;
			for (k = 0; k < ndx; ++k) {
				ptr[k] = work;
				pos[k] = 0;
				if (ds[k] > 0) {
				elt = VECTOR_ELT(subscript, k);
				switch (TYPEOF(elt)) {
				case INTSXP:
				{
					const int *s__ = INTEGER_RO(elt);
					for (l = 0; l < ds[k]; ++l)
						*(work++) = t * (mp_limb_t) (s__[l] - 1);
					break;
				}
				case REALSXP:
				{
					const double *s__ = REAL_RO(elt);
					for (l = 0; l < ds[k]; ++l)
						*(work++) = t * ((mp_limb_t) s__[l] - 1);
					break;
				}
				case OBJSXP:
				{
					const ulong *s__ = R_flint_get_pointer(elt);
					for (l = 0; l < ds[k]; ++l)
						*(work++) = t * (s__[l] - 1);
					break;
				}
				default:
					for (l = 0; l < ds[k]; ++l)
						*(work++) = t * (mp_limb_t) l;
					break;
				}
				}
				t *= (mp_limb_t) dx[k];
			}

#define TEMPLATE(name, elt_t, xptr_t, yptr_t) \
			do { \
				xptr_t v__ = v; \
				xptr_t x__ = x; \
				yptr_t y__ = (ny) ? flint_calloc(ny, sizeof(elt_t)) : 0; \
				y = y__; \
				f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
				what = #name; \
				for (jy = 0; jy < ny; ++jy) \
					name##_set(y__ + jy, x__ + jy); \
				for (js = 0; js < ns; ++js, jv = (++jv >= nv) ? 0 : jv) { \
					jx = 0; \
					for (k = 0; k < ndx; ++k) \
						jx += ptr[k][pos[k]]; \
					name##_set(y__ + jx, v__ + jv); \
					for (k = 0; k < ndx && ++pos[k] >= ds[k]; ++k) \
						pos[k] = 0; \
				} \
			} while (0)

			R_FLINT_SWITCH(class, TEMPLATE);

#undef TEMPLATE

			SEXP ans = PROTECT(newObject(what));
			R_flint_set(ans, y, ny, f);
			setDDNN1(ans, object);
			UNPROTECT(2);
			return ans;

		}
	}
	default:
		Rf_error(_("should never happen ..."));
		return R_NilValue;
	}
}

SEXP R_flint_subscript(SEXP object, SEXP subscript, SEXP s_op)
{
	R_flint_class_t class = R_flint_get_class(object);
	const void *x = R_flint_get_pointer(object);
	void *y = (void *) 0;
	mp_limb_t jx;
	R_CFinalizer_t f = (void *) 0;
	const char *what;

	int op = INTEGER_RO(s_op)[0];
	switch (op) {
	case 2: /* [[ */
	{
		switch (TYPEOF(subscript)) {
		case INTSXP:
			jx = (mp_limb_t) (INTEGER_RO(subscript)[0] - 1);
			break;
		case REALSXP:
			jx = (mp_limb_t) REAL_RO(subscript)[0] - 1;
			break;
		case OBJSXP:
			jx = ((ulong *) R_flint_get_pointer(object))[0] - 1;
			break;
		case VECSXP:
		{
			SEXP elt, dimx = R_do_slot(object, R_flint_symbol_dim);
			const int *dx = INTEGER_RO(dimx);
			int k, ndx = LENGTH(dimx);
			mp_limb_t t = 1;
			jx = 0;
			for (k = 0; k < ndx; ++k) {
				elt = VECTOR_ELT(subscript, k);
				switch (TYPEOF(elt)) {
				case INTSXP:
					jx += t * (mp_limb_t) (INTEGER_RO(elt)[0] - 1);
					break;
				case REALSXP:
					jx += t * ((mp_limb_t) REAL_RO(elt)[0] - 1);
					break;
				case OBJSXP:
					jx += t * (((ulong *) R_flint_get_pointer(elt))[0] - 1);
					break;
				default:
					Rf_error(_("should never happen ..."));
					break;
				}
				t *= (mp_limb_t) dx[k];
			}
			break;
		}
		default:
			Rf_error(_("should never happen ..."));
			jx = 0;
			break;
		}

#define TEMPLATE(name, elt_t, xptr_t, yptr_t) \
		do { \
			xptr_t x__ = x; \
			yptr_t y__ = flint_calloc(1, sizeof(elt_t)); \
			y = y__; \
			f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
			what = #name; \
			name##_set(y__, x__ + jx); \
		} while (0)

		R_FLINT_SWITCH(class, TEMPLATE);

#undef TEMPLATE

		SEXP ans = PROTECT(newObject(what));
		R_flint_set(ans, y, 1, f);
		UNPROTECT(1);
		return ans;
	}
	case 1: /*  [ */
	{
		mp_limb_t js, ns;
		if (TYPEOF(subscript) != VECSXP) {

			switch (TYPEOF(subscript)) {
			case INTSXP:
			case REALSXP:
				ns = (mp_limb_t) XLENGTH(subscript);
				break;
			case OBJSXP:
				ns = R_flint_get_length(subscript);
				break;
			default:
				if (subscript != R_flint_symbol_missing)
					Rf_error(_("should never happen ..."));
				ns = R_flint_get_length(object);
				break;
			}

			SEXP namesx = PROTECT(R_do_slot(object, R_flint_symbol_names)),
				namesy = R_NilValue;
			if (subscript == R_flint_symbol_missing)
				namesy = namesx;
			else if (namesx != R_NilValue && ns > 0 && ns <= R_XLEN_T_MAX)
				namesy = Rf_allocVector(STRSXP, (R_xlen_t) ns);
			PROTECT(namesy);

#define TEMPLATE(name, elt_t, xptr_t, yptr_t) \
			do { \
				xptr_t x__ = x; \
				yptr_t y__ = (ns) ? flint_calloc(ns, sizeof(elt_t)) : 0; \
				y = y__; \
				f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
				what = #name; \
				switch (TYPEOF(subscript)) { \
				case INTSXP: \
				{ \
					const int *s__ = INTEGER_RO(subscript); \
					if (namesy == R_NilValue) \
					for (js = 0; js < ns; ++js) \
						name##_set(y__ + js, x__ + (s__[js] - 1)); \
					else \
					for (js = 0; js < ns; ++js) { \
						name##_set(y__ + js, x__ + (s__[js] - 1)); \
						SET_STRING_ELT(namesy, (R_xlen_t) js, \
						               STRING_ELT(namesx, s__[js] - 1)); \
					} \
					break; \
				} \
				case REALSXP: \
				{ \
					const double *s__ = REAL_RO(subscript); \
					if (namesy == R_NilValue) \
					for (js = 0; js < ns; ++js) \
						name##_set(y__ + js, x__ + ((mp_limb_t) s__[js] - 1)); \
					else \
					for (js = 0; js < ns; ++js) { \
						name##_set(y__ + js, x__ + ((mp_limb_t) s__[js] - 1)); \
						SET_STRING_ELT(namesy, (R_xlen_t) js, \
						               STRING_ELT(namesx, (R_xlen_t) ((mp_limb_t) s__[js] - 1))); \
					} \
					break; \
				} \
				case OBJSXP: \
				{ \
					const ulong *s__ = R_flint_get_pointer(subscript); \
					if (namesy == R_NilValue) \
					for (js = 0; js < ns; ++js) \
						name##_set(y__ + js, x__ + (s__[js] - 1)); \
					else \
					for (js = 0; js < ns; ++js) { \
						name##_set(y__ + js, x__ + (s__[js] - 1)); \
						SET_STRING_ELT(namesy, (R_xlen_t) js, \
						               STRING_ELT(namesx, (R_xlen_t) (s__[js] - 1))); \
					} \
					break; \
				} \
				default: \
					for (js = 0; js < ns; ++js) \
						name##_set(y__ + js, x__ + js); \
					break; \
				} \
			} while (0)

			R_FLINT_SWITCH(class, TEMPLATE);

#undef TEMPLATE

			SEXP ans = PROTECT(newObject(what));
			R_flint_set(ans, y, ns, f);
			setDDNN(ans, R_NilValue, R_NilValue, namesy);
			UNPROTECT(3);
			return ans;

		} else {

			SEXP elt,
				dimx = PROTECT(R_do_slot(object, R_flint_symbol_dim)),
				dimy = PROTECT(Rf_allocVector(INTSXP, XLENGTH(dimx))),
				dimnamesx = PROTECT(R_do_slot(object, R_flint_symbol_dimnames)),
				dimnamesy = R_NilValue,
				namesdimnamesx = PROTECT(Rf_getAttrib(dimnamesx, R_NamesSymbol));
			const int *dx = INTEGER_RO(dimx);
			int k, ndx = LENGTH(dimx);
			int *ds = INTEGER(dimy);
			mp_limb_t r, s = 0, t = 1;
			int anynames = namesdimnamesx != R_NilValue;
			for (k = 0; k < ndx; ++k) {
				elt = VECTOR_ELT(subscript, k);
				switch (TYPEOF(elt)) {
				case INTSXP:
				case REALSXP:
					r = (mp_limb_t) XLENGTH(elt);
					break;
				case OBJSXP:
					r = R_flint_get_length(elt);
					break;
				default:
					if (elt != R_flint_symbol_missing)
						Rf_error(_("should never happen ..."));

					r = (mp_limb_t) dx[k];
					break;
				}
				if (r > 0) {
				if (r > INT_MAX)
					Rf_error(_("dimensions would exceed maximum %d"),
					         INT_MAX);
				if (t > UWORD_MAX / r)
					Rf_error(_("value length would exceed maximum %llu"),
					         (unsigned long long int) UWORD_MAX);
				if (!anynames && dimnamesx != R_NilValue &&
				    VECTOR_ELT(dimnamesx, k) != R_NilValue)
					anynames = 1;
				}
				s += r;
				t *= r;
				ds[k] = (int) r;
			}
			if (anynames) {
				dimnamesy = Rf_allocVector(VECSXP, XLENGTH(dimnamesx));
				if (namesdimnamesx != R_NilValue) {
				PROTECT(dimnamesy);
				Rf_setAttrib(dimnamesy, R_NamesSymbol, namesdimnamesx);
				UNPROTECT(1);
				}
			}
			PROTECT(dimnamesy);

			SEXP marnamesx, marnamesy;
			mp_limb_t
				*work = (void *) R_alloc((size_t) s, sizeof(mp_limb_t)),
				**ptr = (void *) R_alloc((size_t) ndx, sizeof(mp_limb_t *));
			int l, *pos = (void *) R_alloc((size_t) ndx, sizeof(int));
			ns = t; t = 1;
			for (k = 0; k < ndx; ++k) {
				ptr[k] = work;
				pos[k] = 0;
				if (ds[k] > 0) {
				elt = VECTOR_ELT(subscript, k);
				marnamesx = (anynames) ? VECTOR_ELT(dimnamesx, k) : R_NilValue;
				switch (TYPEOF(elt)) {
				case INTSXP:
				{
					const int *s__ = INTEGER_RO(elt);
					if (marnamesx == R_NilValue)
					for (l = 0; l < ds[k]; ++l)
						*(work++) = t * (mp_limb_t) (s__[l] - 1);
					else {
					marnamesy = Rf_allocVector(STRSXP, ds[k]);
					for (l = 0; l < ds[k]; ++l) {
						*(work++) = t * (mp_limb_t) (s__[l] - 1);
						SET_STRING_ELT(marnamesy, l,
						               STRING_ELT(marnamesx, s__[l] - 1));
					}
					SET_VECTOR_ELT(dimnamesy, k, marnamesy);
					}
					break;
				}
				case REALSXP:
				{
					const double *s__ = REAL_RO(elt);
					if (marnamesx == R_NilValue)
					for (l = 0; l < ds[k]; ++l)
						*(work++) = t * ((mp_limb_t) s__[l] - 1);
					else {
					marnamesy = Rf_allocVector(STRSXP, ds[k]);
					for (l = 0; l < ds[k]; ++l) {
						*(work++) = t * ((mp_limb_t) s__[l] - 1);
						SET_STRING_ELT(marnamesy, l,
						               STRING_ELT(marnamesx, (R_xlen_t) ((mp_limb_t) s__[l] - 1)));
					}
					SET_VECTOR_ELT(dimnamesy, k, marnamesy);
					}
					break;
				}
				case OBJSXP:
				{
					const ulong *s__ = R_flint_get_pointer(elt);
					if (marnamesx == R_NilValue)
					for (l = 0; l < ds[k]; ++l)
						*(work++) = t * (s__[l] - 1);
					else {
					marnamesy = Rf_allocVector(STRSXP, ds[k]);
					for (l = 0; l < ds[k]; ++l) {
						*(work++) = t * (s__[l] - 1);
						SET_STRING_ELT(marnamesy, l,
						               STRING_ELT(marnamesx, (R_xlen_t) (s__[l] - 1)));
					}
					SET_VECTOR_ELT(dimnamesy, k, marnamesy);
					}
					break;
				}
				default:
					for (l = 0; l < ds[k]; ++l)
						*(work++) = t * (mp_limb_t) l;
					if (marnamesx != R_NilValue)
					SET_VECTOR_ELT(dimnamesy, k, marnamesx);
					break;
				}
				}
				t *= (mp_limb_t) dx[k];
			}

#define TEMPLATE(name, elt_t, xptr_t, yptr_t) \
			do { \
				xptr_t x__ = x; \
				yptr_t y__ = (ns) ? flint_calloc(ns, sizeof(elt_t)) : 0; \
				y = y__; \
				f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
				what = #name; \
				for (js = 0; js < ns; ++js) { \
					jx = 0; \
					for (k = 0; k < ndx; ++k) \
						jx += ptr[k][pos[k]]; \
					name##_set(y__ + js, x__ + jx); \
					for (k = 0; k < ndx && ++pos[k] >= ds[k]; ++k) \
						pos[k] = 0; \
				} \
			} while (0)

			R_FLINT_SWITCH(class, TEMPLATE);

#undef TEMPLATE

			SEXP ans = PROTECT(newObject(what));
			R_flint_set(ans, y, ns, f);
			setDDNN(ans, dimy, dimnamesy, R_NilValue);
			UNPROTECT(6);
			return ans;
		}
	}
	default:
		Rf_error(_("should never happen ..."));
		return R_NilValue;
	}
}

SEXP R_flint_transpose(SEXP object, SEXP s_conjugate)
{
	int conjugate = LOGICAL_RO(s_conjugate)[0];
	R_flint_class_t class = R_flint_get_class(object);
	const void *x = R_flint_get_pointer(object);
	void *y = (void *) 0;
	mp_limb_t jx = 0, jy = 0,
		nx = R_flint_get_length(object),
		ny = nx;
	R_CFinalizer_t f = (void *) 0;
	const char *what;

	SEXP dimx = R_do_slot(object, R_flint_symbol_dim);
	int i, j, dx[2];
	if (dimx == R_NilValue) {
		if (nx > INT_MAX)
			Rf_error(_("number of columns would exceed maximum %d"),
			         INT_MAX);
		dx[0] = (int) nx;
		dx[1] = 1;
	} else {
		if (XLENGTH(dimx) != 2)
			Rf_error(_("'%s' is an array but not a matrix"), "x");
		dx[0] = INTEGER_RO(dimx)[0];
		dx[1] = INTEGER_RO(dimx)[1];
	}

#define TEMPLATE(name, elt_t, xptr_t, yptr_t) \
	do { \
		xptr_t x__ = x; \
		yptr_t y__ = (ny) ? flint_calloc(ny, sizeof(elt_t)) : 0; \
		y = y__; \
		f = (R_CFinalizer_t) &R_flint_##name##_finalize; \
		what = #name; \
		if (conjugate) \
		for (i = 0; i < dx[0]; ++i, jx -= nx - 1) \
			for (j = 0; j < dx[1]; ++j, jx += dx[0], ++jy) \
				name##_conj(y__ + jy, x__ + jx); \
		else \
		for (i = 0; i < dx[0]; ++i, jx -= nx - 1) \
			for (j = 0; j < dx[1]; ++j, jx += dx[0], ++jy) \
				name##_set (y__ + jy, x__ + jx); \
	} while (0)

	R_FLINT_SWITCH(class, TEMPLATE);

#undef TEMPLATE

	SEXP ans = PROTECT(newObject(what));
	R_flint_set(ans, y, ny, f);

	SEXP dimy = PROTECT(Rf_allocVector(INTSXP, 2));
	int *dy = INTEGER(dimy);
	dy[0] = dx[1];
	dy[1] = dx[0];
	R_do_slot_assign(ans, R_flint_symbol_dim, dimy);
	UNPROTECT(1);

	if (dimx == R_NilValue) {
		SEXP namesx = R_do_slot(object, R_flint_symbol_names);
		if (namesx != R_NilValue) {
		PROTECT(namesx);
		SEXP dimnamesy = PROTECT(Rf_allocVector(VECSXP, 2));
		SET_VECTOR_ELT(dimnamesy, 1, namesx);
		R_do_slot_assign(ans, R_flint_symbol_dimnames, dimnamesy);
		UNPROTECT(2);
		}
	} else {
		SEXP dimnamesx = R_do_slot(object, R_flint_symbol_dimnames);
		if (dimnamesx != R_NilValue) {
		PROTECT(dimnamesx);
		SEXP dimnamesy = PROTECT(Rf_allocVector(VECSXP, 2));
		SET_VECTOR_ELT(dimnamesy, 0, VECTOR_ELT(dimnamesx, 1));
		SET_VECTOR_ELT(dimnamesy, 1, VECTOR_ELT(dimnamesx, 0));
		SEXP namesdimnamesx = Rf_getAttrib(dimnamesx, R_NamesSymbol);
		if (namesdimnamesx != R_NilValue) {
		PROTECT(namesdimnamesx);
		SEXP namesdimnamesy = PROTECT(Rf_allocVector(STRSXP, 2));
		SET_STRING_ELT(namesdimnamesy, 0, STRING_ELT(namesdimnamesx, 1));
		SET_STRING_ELT(namesdimnamesy, 1, STRING_ELT(namesdimnamesx, 0));
		Rf_setAttrib(dimnamesy, R_NamesSymbol, namesdimnamesy);
		UNPROTECT(2);
		}
		R_do_slot_assign(ans, R_flint_symbol_dimnames, dimnamesy);
		UNPROTECT(2);
		}
	}

	UNPROTECT(1);
	return ans;
}

SEXP R_flint_triple(SEXP object)
{
	R_flint_class_t class = R_flint_get_class(object);
	if (class == R_FLINT_CLASS_INVALID) {
		SEXP ans = Rf_allocVector(STRSXP, 3);
		for (R_xlen_t i = 0; i < 3; ++i)
			SET_STRING_ELT(ans, i, NA_STRING);
		return ans;
	}
	SEXP ans = PROTECT(Rf_allocVector(STRSXP, 3));
	char buffer[64];
	snprintf(buffer, 64, "%s",
	         R_flint_classes[R_flint_get_class(object)]);
	SET_STRING_ELT(ans, 0, Rf_mkChar(buffer));
	snprintf(buffer, 64, "%llu",
	         (unsigned long long int) R_flint_get_length(object));
	SET_STRING_ELT(ans, 1, Rf_mkChar(buffer));
	snprintf(buffer, 64, "%p",
	         R_flint_get_pointer(object));
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
		return INVALID(_("type of protected field of '%s' is not \"%s\""),
		               ".xData", "integer");
#ifdef R_FLINT_ABI_64
#define NPROTECTED 2
#else
#define NPROTECTED 1
#endif
	if (XLENGTH(length) != NPROTECTED)
		return INVALID(_("length of protected field of '%s' is not %d"),
		               ".xData", NPROTECTED);
#undef NPROTECTED
	mp_limb_t n;
	uucopy(&n, (const unsigned int *) INTEGER_RO(length));
	if ((R_ExternalPtrAddr(x) == 0) != (n == 0))
		return INVALID((n == 0)
		               ? _("length of '%s' is zero and pointer field of '%s' is nonzero")
		               : _("length of '%s' is nonzero and pointer field of '%s' is zero"),
		               "object", ".xData");
	SEXP dim = PROTECT(R_do_slot(object, R_flint_symbol_dim)),
		dimnames = PROTECT(R_do_slot(object, R_flint_symbol_dimnames)),
		names = PROTECT(R_do_slot(object, R_flint_symbol_names));
	UNPROTECT(3);
	if (dim != R_NilValue) {
		R_xlen_t i, m = XLENGTH(dim);
		if (m == 0)
			return INVALID(_("length of '%s' is %d"),
			               "dim", 0);
		if (m > INT_MAX)
			return INVALID(_("length of '%s' exceeds maximum %d"),
			               "dim", INT_MAX);
		const int *d = INTEGER_RO(dim);
		int f = 1;
		for (i = 0; i < m; ++i) {
			if (d[i] == NA_INTEGER)
				return INVALID(_("%s[[%d]] is NA"),
				               "dim", (int) i);
			if (d[i] < 0)
				return INVALID(_("%s[[%d]] is negative"),
				               "dim", (int) i);
			f &= d[i] > 0;
		}
		if (f) {
	    mp_limb_t l = 1;
		for (i = 0; i < m; ++i) {
			if (d[i] > UWORD_MAX / l)
				return INVALID(_("product of '%s' exceeds maximum %llu"),
				               "dim", UWORD_MAX);
			l *= (unsigned int) d[i];
		}
		if (l != n)
			return INVALID(_("product of '%s' [%llu] is not equal to length of object [%llu]"),
			               "dim", (unsigned long long int) l,
			                      (unsigned long long int) n);
		}
	}
	if (dimnames != R_NilValue) {
		if (dim == R_NilValue)
			return INVALID(_("'%s' is not NULL and '%s' is NULL"),
			               "dimnames", "dim");
		R_xlen_t i, m = XLENGTH(dim);
		if (XLENGTH(dimnames) != m)
			return INVALID(_("length of '%s' [%lld] is not equal to length of '%s' [%lld]"),
			               "dimnames", (long long int) XLENGTH(dimnames),
			               "dim"     , (long long int) m);
		const int *d = INTEGER_RO(dim);
		for (i = 0; i < m; ++i) {
			SEXP elt = VECTOR_ELT(dimnames, i);
			if (elt != R_NilValue) {
				if (TYPEOF(elt) != STRSXP)
					return INVALID(_("invalid type \"%s\" for %s[[%d]]"),
					               Rf_type2char(TYPEOF(elt)),
					               "dimnames", (int) i);
				if (XLENGTH(elt) != d[i])
					return INVALID(_("length of %s[[%d]] [%lld] is not equal to %s[[%d]] [%lld]"),
					               "dimnames", (int) i, (long long int) XLENGTH(elt),
					               "dim"     , (int) i, (long long int) d[i]);
			}
		}
	}
	if (names != R_NilValue) {
		mp_limb_t m = (mp_limb_t) XLENGTH(names);
		if (m != n)
			return INVALID(_("length of '%s' [%llu] is not equal to length of object [%llu]"),
			               "names", (unsigned long long int) m,
			                        (unsigned long long int) n);
	}
	return Rf_ScalarLogical(1);
#undef INVALID
}
