#include "flint.h"

#define PROGRESS_MAX 60
#define PROGRESS(p, s, n) \
do { \
	if (progress >= (p)) { \
		Rprintf((s)); \
		if ((n)++ == PROGRESS_MAX) { \
			Rprintf("\n"); \
			(n) = 0; \
		} \
	} \
} while (0)

typedef enum {
	RK_PASS = 0,
	RK_FAIL_NOOP,
	RK_FAIL_HMIN,
	RK_FAIL_SMAX,
	RK_INVALID = -1
} rk_status_t;

static
void
rk_istep(SEXP call, arf_ptr callt, arf_ptr cally,
         arf_srcptr t0, arf_srcptr h,
         arf_ptr y0, arf_ptr y1, arf_ptr y2, mp_limb_t ny,
         arf_srcptr a, arf_srcptr b, arf_srcptr bb, arf_srcptr c,
         mp_limb_t d,
         arf_ptr ak, arf_ptr bk, arf_ptr bbk, arf_ptr kk,
         slong prec, arf_rnd_t rnd)
{
	mp_limb_t i, j, jy;
	for (jy = 0; jy < ny; ++jy) {
		arf_zero( bk + jy);
		if (bb)
		arf_zero(bbk + jy);
	}
	for (i = 0; i < d; ++i) {
		for (jy = 0; jy < ny; ++jy)
			arf_zero(ak + jy);
		for (j = 0; j < i; ++j, ++a)
			for (jy = 0; jy < ny; ++jy)
				arf_addmul(ak + jy, a, kk + j * ny + jy, prec, rnd);
		arf_set(callt, t0);
		arf_addmul(callt, c + i, h, prec, rnd);
		for (jy = 0; jy < ny; ++jy) {
			arf_set(cally + jy, y0 + jy);
			arf_addmul(cally + jy, ak + jy, h, prec, rnd);
		}
		SEXP value = Rf_eval(call, R_ClosureEnv(CAR(call)));
		if (R_flint_get_class(value) != R_FLINT_CLASS_ARF)
			Rf_error(_("class of value of '%s' call is not \"%s\""),
			         "func", "arf");
		if (R_flint_get_length(value) != ny)
			Rf_error(_("length of value of '%s' call is not %s"),
			         "func", "length(y0)");
		arf_srcptr k = R_flint_get_pointer(value);
		for (jy = 0; jy < ny; ++jy) {
			arf_set(kk + i * ny + jy, k + jy);
			arf_addmul( bk + jy,  b + i, k + jy, prec, rnd);
			if (bb)
			arf_addmul(bbk + jy, bb + i, k + jy, prec, rnd);
		}
	}
	for (jy = 0; jy < ny; ++jy) {
		arf_set(y1 + jy, y0 + jy);
		arf_addmul(y1 + jy,  bk + jy, h, prec, rnd);
		if (bb) {
		arf_set(y2 + jy, y0 + jy);
		arf_addmul(y2 + jy, bbk + jy, h, prec, rnd);
		}
	}
	return;
}

static
rk_status_t
rk_estep(SEXP call, arf_ptr callt, arf_ptr cally,
         arf_srcptr t0, arf_srcptr t1,
         arf_ptr *y0, arf_ptr *y1, arf_ptr *y2, mp_limb_t ny,
         arf_srcptr rtol, mp_limb_t rmsk,
         arf_srcptr atol, mp_limb_t amsk,
         arf_srcptr hmin, arf_srcptr hmax, arf_ptr hcur,
         ulong smax, ulong *scur,
         arf_srcptr a, arf_srcptr b, arf_srcptr bb, arf_srcptr c,
         mp_limb_t d, mp_limb_t p,
         arf_ptr ak, arf_ptr bk, arf_ptr bbk, arf_ptr kk,
         int progress, unsigned int *count,
         slong prec, arf_rnd_t rnd, arf_ptr work)
{
	int cmp = -1;
	mp_limb_t jy;
	arf_ptr w0 = work, w1 = work + 1, swap;
	arf_set(w0, t0);
	if (bb) {
		arf_ptr hscl = work + 2;
		while (cmp < 0 && *scur < smax) {
			arf_add(w1, w0, hcur, prec, rnd);
			if (arf_equal(w1, w0))
				return RK_FAIL_NOOP;
			cmp = arf_cmp(w1, t1);
			if (cmp >= 0)
				arf_sub(hcur, t1, w0, prec, rnd);
			rk_istep(call, callt, cally, w0, hcur, *y0, *y1, *y2, ny,
			         a, b, bb, c, d, ak, bk, bbk, kk, prec, rnd);
			*scur += 1;
			arf_pos_inf(hscl);
			for (jy = 0; jy < ny; ++jy) {
				arf_set(ak + jy, atol + (amsk & jy));
				arf_addmul(ak + jy, rtol + (rmsk & jy), *y2 + jy, prec, rnd);
				arf_sub(bk + jy, *y1 + jy, *y2 + jy, prec, rnd);
				if (!arf_is_nan(ak + jy) &&
				    !arf_is_nan(bk + jy) && !arf_is_zero(bk + jy)) {
					arf_div(bbk + jy, ak + jy, bk + jy, prec, rnd);
					if (arf_cmpabs(bbk + jy, hscl) < 0)
						arf_abs(hscl, bbk + jy);
				}
			}
			arf_root(hscl, hscl, p, prec, rnd);
			if (arf_cmp_2exp_si(hscl, 0) >= 0) {
				if (arf_cmp_2exp_si(hscl,  4) >= 0)
					arf_mul_2exp_si(hcur, hcur,  4);
				else
					arf_mul(hcur, hcur, hscl, prec, rnd);
				if (arf_cmp(hcur, hmax) > 0)
					arf_set(hcur, hmax);
				swap =  w0;  w0 =  w1;  w1 = swap;
				swap = *y0; *y0 = *y1; *y1 = swap;
				PROGRESS(2, "o", *count);
			} else {
				if (arf_cmp_2exp_si(hscl, -4) <= 0)
					arf_mul_2exp_si(hcur, hcur, -4);
				else
					arf_mul(hcur, hcur, hscl, prec, rnd);
				if (arf_cmp(hcur, hmin) < 0)
					return RK_FAIL_HMIN;
				cmp = -1;
				PROGRESS(2, "x", *count);
			}
		}
	} else {
		arf_ptr hrem = work + 2, htmp;
		while (cmp < 0 && *scur < smax) {
			arf_add(w1, w0, hcur, prec, rnd);
			if (arf_equal(w1, w0))
				return RK_FAIL_NOOP;
			cmp = arf_cmp(w1, t1);
			if (cmp < 0)
				htmp = hcur;
			else {
				arf_sub(hrem, t1, w0, prec, rnd);
				htmp = hrem;
			}
			rk_istep(call, callt, cally, w0, htmp, *y0, *y1, *y2, ny,
			         a, b, bb, c, d, ak, bk, bbk, kk, prec, rnd);
			*scur += 1;
			swap =  w0;  w0 =  w1;  w1 = swap;
			swap = *y0; *y0 = *y1; *y1 = swap;
			PROGRESS(2, "o", *count);
		}
	}
	return (cmp < 0) ? RK_FAIL_SMAX : RK_PASS;
}

SEXP R_flint_arf_calc_rk(SEXP s_res, SEXP s_func, SEXP s_t, SEXP s_y0, SEXP s_param, SEXP s_rtol, SEXP s_atol, SEXP s_hmin, SEXP s_hmax, SEXP s_hini, SEXP s_smax, SEXP s_method, SEXP s_progress, SEXP s_prec)
{
	slong prec = asPrec(s_prec, __func__);
	arf_rnd_t rnd = ARF_RND_NEAR;
	int adapt = VECTOR_ELT(s_method, 2) != R_NilValue;

	mp_limb_t jt, jy,
		nt = R_flint_get_length(s_t),
		ny = R_flint_get_length(s_y0);
	arf_srcptr
		t = R_flint_get_pointer(s_t),
		y = R_flint_get_pointer(s_y0);

	if (nt == 0 || ny == 0)
		Rf_error(_("length of '%s' is not positive"),
		         (nt == 0) ? "t" : "y0");
	if (nt > INT_MAX || ny > INT_MAX)
		Rf_error(_("dimensions would exceed maximum %d"),
		         INT_MAX);
	if (ny > UWORD_MAX / nt)
		Rf_error(_("length would exceed maximum %llu"),
		         (unsigned long long int) UWORD_MAX);

	arf_srcptr
		a  = R_flint_get_pointer(VECTOR_ELT(s_method, 0)),
		b  = R_flint_get_pointer(VECTOR_ELT(s_method, 1)),
		bb = (adapt)
		   ? R_flint_get_pointer(VECTOR_ELT(s_method, 2))
		   : 0,
		c  = R_flint_get_pointer(VECTOR_ELT(s_method, 3));
	mp_limb_t
		d  = (mp_limb_t) INTEGER(VECTOR_ELT(s_method, 4))[0],
		p  = (mp_limb_t) INTEGER(VECTOR_ELT(s_method, 5))[0],
		nwork = 6 + (4 + 2 * (mp_limb_t) adapt + d) * ny;

	SEXP s_work = PROTECT(newFlint(R_FLINT_CLASS_ARF, 0, nwork));
	arf_ptr work = R_flint_get_pointer(s_work),
		y0 = work + 6, y1 = y0 + ny, y2 = (adapt) ? y1 + ny : y1,
		ak = y2 + ny, bk = ak + ny, bbk = (adapt) ? bk + ny : bk,
		kk = bbk + ny;

	for (jt = 0; jt < nt; ++jt)
		if (!arf_is_finite(t + jt))
			Rf_error(_("'%s' is not finite"), "t");
	arf_pos_inf(work);
	for (jt = 1; jt < nt; ++jt) {
		arf_sub(work + 1, t + jt, t + jt - 1, prec, rnd);
		if (arf_sgn(work + 1) <= 0)
			Rf_error(_("'%s' is not increasing"), "t");
		if (arf_cmp(work + 1, work) < 0)
			arf_set(work, work + 1);
	}

	mp_limb_t nrtol = 1, natol = 1, rmsk = 0, amsk = 0;
	arf_ptr rtol = 0, atol = 0, hmin = 0, hmax = 0, hcur = 0;
	ulong smax = 0, scur = 0;
	int progress = 0;

	if (adapt) {

	if (s_rtol == R_NilValue) {
		rtol = work + 3;
		arf_set_si_2exp_si(rtol, 1, -prec/2);
	} else {
		rtol = R_flint_get_pointer(s_atol);
		if ((nrtol = R_flint_get_length(s_rtol)) != 1 && nrtol != ny)
			Rf_error(_("length of '%s' is not %d or %s"),
			         "rtol", 1, "length(y0)");
		for (jy = 0; jy < nrtol; ++jy)
			if (arf_is_nan(rtol + jy) || arf_sgn(rtol + jy) < 0)
				Rf_error(_("'%s' is not non-negative"), "rtol");
	}
	if (nrtol == ny)
		rmsk = UWORD_MAX;

	if (s_atol == R_NilValue) {
		atol = work + 4;
		arf_set_si_2exp_si(atol, 1, -prec/2);
	} else {
		atol = R_flint_get_pointer(s_atol);
		if ((natol = R_flint_get_length(s_atol)) != 1 && natol != ny)
			Rf_error(_("length of '%s' is not %d or %s"),
			         "atol", 1, "length(y0)");
		for (jy = 0; jy < natol; ++jy)
			if (arf_is_nan(atol + jy) || arf_sgn(atol + jy) < 0)
				Rf_error(_("'%s' is not non-negative"), "atol");
	}
	if (natol == ny)
		amsk = UWORD_MAX;

	hmin = R_flint_get_pointer(s_hmin);
	if (R_flint_get_length(s_hmin) != 1)
		Rf_error(_("length of '%s' is not %d"), "hmin", 1);
	if (arf_is_nan(hmin) || arf_sgn(hmin) <  0 ||
	    arf_cmp(hmin, work) > 0)
		Rf_error(_("'%s' is not in [%s, %s]"),
		         "hmin",    "0", "min(diff(t))");

	hmax = R_flint_get_pointer(s_hmax);
	if (R_flint_get_length(s_hmax) != 1)
		Rf_error(_("length of '%s' is not %d"), "hmax", 1);
	if (arf_is_nan(hmax) || arf_sgn(hmax) <= 0 ||
	    arf_cmp(hmax, hmin) < 0)
		Rf_error(_("'%s' is not in [%s, %s] or not positive"),
		         "hmax", "hmin", "Inf");

	if (s_hini == R_NilValue) {
		hcur = work + 5;
		arf_set(hcur, work);
	} else {
		hcur = R_flint_get_pointer(s_hini);
		if (R_flint_get_length(s_hini) != 1)
			Rf_error(_("length of '%s' is not %d"), "hini", 1);
	}
	if (arf_is_nan(hcur) || arf_sgn(hcur) <= 0 ||
	    arf_cmp(hcur, hmin) < 0 || arf_cmp(hcur, hmax) > 0)
		Rf_error(_("'%s' is not in [%s, %s] or not positive"),
		         "hini", "hmin", "hmax");

	} else {

	if (s_hini == R_NilValue) {
		hcur = work + 5;
		arf_set(hcur, work);
	} else {
		hcur = R_flint_get_pointer(s_hini);
		if (R_flint_get_length(s_hini) != 1)
			Rf_error(_("length of '%s' is not %d"), "hini", 1);
		if (arf_is_nan(hcur) || arf_sgn(hcur) <= 0)
			Rf_error(_("'%s' is not positive"),
			         "hini");
	}

	}

	if (s_smax == R_NilValue)
		smax = (prec > (UWORD_MAX >> 8)) ? UWORD_MAX : ((ulong) prec) << 8;
	else {
		if (R_flint_get_length(s_smax) != 1)
			Rf_error(_("length of '%s' is not %d"), "smax", 1);
		smax = ((ulong *) R_flint_get_pointer(s_smax))[0];
	}
	if (nt > 1)
	smax = (smax > UWORD_MAX / (nt - 1)) ? UWORD_MAX : smax * (nt - 1);

	if (XLENGTH(s_progress) != 1)
		Rf_error(_("length of '%s' is not %d"), "progress", 1);
	progress = INTEGER(s_progress)[0];

	/* R: func(t, y, param, prec) */
	SEXP s_a0 = s_func;
	SEXP s_a1 = PROTECT(newFlint(R_FLINT_CLASS_ARF, 0, 1));
	SEXP s_a2 = PROTECT(newFlint(R_FLINT_CLASS_ARF, 0, ny));
	SEXP s_a3 = s_param;
	SEXP s_a4 = PROTECT(newFlint(R_FLINT_CLASS_SLONG, 0, 1));
	SEXP call = PROTECT(Rf_lang5(s_a0, s_a1, s_a2, s_a3, s_a4));

	arf_ptr a1 = R_flint_get_pointer(s_a1);
	arf_ptr a2 = R_flint_get_pointer(s_a2);
	((slong *) R_flint_get_pointer(s_a4))[0] = prec;

	arf_ptr rest = flint_calloc(nt     , sizeof(arf_t));
	arf_ptr resy = flint_calloc(nt * ny, sizeof(arf_t));
	R_flint_set(VECTOR_ELT(s_res, 0), rest, nt     , (R_CFinalizer_t) &R_flint_arf_finalize);
	R_flint_set(VECTOR_ELT(s_res, 1), resy, nt * ny, (R_CFinalizer_t) &R_flint_arf_finalize);
	for (jt = 0; jt < nt; ++jt)
		arf_set(rest + jt     , t + jt);
	for (jy = 0; jy < ny; ++jy) {
		arf_set(resy + jy * nt, y + jy);
		arf_set(  y0 + jy     , y + jy);
	}

	rk_status_t status = RK_PASS;
	unsigned int count = 0;
	for (jt = 1; jt < nt; ++jt) {
		status =
		rk_estep(call, a1, a2, t + jt - 1, t + jt, &y0, &y1, &y2, ny,
		         rtol, rmsk, atol, amsk, hmin, hmax, hcur, smax, &scur,
		         a, b, bb, c, d, p, ak, bk, bbk, kk, progress, &count,
		         prec, rnd, work);
		if (status == RK_PASS) {
			for (jy = 0; jy < ny; ++jy)
				arf_set(resy + jy * nt + jt, y0 + jy);
			PROGRESS(1, ".", count);
		} else {
			mp_limb_t jt__;
			for (jy = 0; jy < ny; ++jy)
				for (jt__ = jt; jt__ < nt; ++jt__)
					arf_nan(resy + jy * nt + jt__);
			break;
		}
	}
	if (progress > 0 && count > 0)
		Rprintf("\n");

	switch (status) {
	case RK_FAIL_NOOP:
		Rf_warning(_("returning early due to t+h==t in external step from %s[%d] to %s[%d]"),
		           "t", (int) jt, "t", (int) (jt + 1));
		break;
	case RK_FAIL_HMIN:
		Rf_warning(_("returning early due to h<hmin in external step from %s[%d] to %s[%d]"),
		           "t", (int) jt, "t", (int) (jt + 1));
		break;
	case RK_FAIL_SMAX:
		Rf_warning(_("returning early due to s>smax in external step from %s[%d] to %s[%d]"),
		           "t", (int) jt, "t", (int) (jt + 1));
		break;
	default:
		break;
	}

	SEXP dim = PROTECT(Rf_allocVector(INTSXP, 2));
	INTEGER(dim)[0] = (int) nt;
	INTEGER(dim)[1] = (int) ny;
	R_do_slot_assign(VECTOR_ELT(s_res, 1), R_flint_symbol_dim, dim);
	UNPROTECT(1);

	SEXP colnames = R_do_slot(s_y0, R_flint_symbol_names);
	if (colnames != R_NilValue) {
	PROTECT(colnames);
	SEXP dimnames = PROTECT(Rf_allocVector(VECSXP, 2));
	SET_VECTOR_ELT(dimnames, 1, colnames);
	R_do_slot_assign(VECTOR_ELT(s_res, 1), R_flint_symbol_dimnames, dimnames);
	UNPROTECT(2);
	}

	UNPROTECT(5);
	return R_NilValue;
}
