#include "flint.h"
#include "fallback.h"

#ifndef HAVE_FMPQ_CLOG_UI
slong fmpq_clog_ui(const fmpq_t x, ulong b)
{
	slong clog = fmpz_clog_ui(fmpq_numref(x), b) -
		fmpz_flog_ui(fmpq_denref(x), b);
	/* 'clog' can be off by 1: test if b^(clog-1) < x  */
	clog -= 1;
	fmpz_t p;
	fmpz_init(p);
	fmpz_set_si(p, 10);
	if (clog >= 0) {
		fmpz_pow_ui(p, p, (ulong) clog);
		if (fmpq_cmp_fmpz(x, p) > 0)
			clog += 1;
	} else {
		fmpq_t y;
		fmpq_init(y);
		fmpq_inv(y, x);
		fmpz_pow_ui(p, p, (ulong) -1 - (ulong) clog + 1);
		if (fmpq_cmp_fmpz(y, p) < 0)
			clog += 1;
		fmpq_clear(y);
	}
	fmpz_clear(p);
	return clog;
}
#endif

#ifndef HAVE_MAG_GET_D_LOWER
double mag_get_d_lower(const mag_t z)
{
	if (mag_is_zero(z))
		return 0.0;
	else if (mag_is_inf(z))
		return D_INF;
	else if (MAG_EXP(z) < -1000 || MAG_EXP(z) > 1000) {
		if (fmpz_sgn(MAG_EXPREF(z)) < 0)
			return 0.0;
		else
			return ldexp(1.0, 1000);
	}
	else
		return
#ifdef HAVE_D_MUL_2EXP
			d_mul_2exp
#else
			ldexp
#endif
			((double) MAG_MAN(z), (int) MAG_EXP(z) - MAG_BITS);
}
#endif

#ifndef HAVE_MAG_DIV_UI_LOWER
void mag_div_ui_lower(mag_t z, const mag_t x, ulong y)
{
	mag_t t;
	mag_init(t);
	mag_set_ui(t, y);
	mag_div_lower(z, x, t);
	mag_clear(t);
	return;
}
#endif

#ifndef HAVE_MAG_LOG1P_LOWER
void mag_log1p_lower(mag_t z, const mag_t x)
{
	mag_t t, u;
	mag_init(t);
	mag_init(u);
	mag_one(u);
	mag_add_lower(t, u, x);
	mag_log_lower(z, t);
	mag_clear(t);
	mag_clear(u);
	return;
}
#endif

#ifndef HAVE_MAG_EXPM1_LOWER
void mag_expm1_lower(mag_t z, const mag_t x)
{
	mag_t t, u;
	mag_init(t);
	mag_init(u);
	mag_one(u);
	mag_exp_lower(t, x);
	mag_sub_lower(z, t, u);
	mag_clear(t);
	mag_clear(u);
	return;
}
#endif

#ifndef HAVE_ACF_IS_ZERO
int acf_is_zero(const acf_t x)
{
	return
		arf_is_zero(acf_realref(x)) &&
		arf_is_zero(acf_imagref(x));
}
#endif

#ifndef HAVE_ACF_IS_NAN
int acf_is_nan(const acf_t x)
{
	return
		arf_is_nan(acf_realref(x)) ||
		arf_is_nan(acf_imagref(x));
}
#endif

#ifndef HAVE_ACF_IS_INF
int acf_is_inf(const acf_t x)
{
	return
		arf_is_inf(acf_realref(x)) ||
		arf_is_inf(acf_imagref(x));
}
#endif

#ifndef HAVE_ACF_IS_FINITE
int acf_is_finite(const acf_t x)
{
	return
		arf_is_finite(acf_realref(x)) &&
		arf_is_finite(acf_imagref(x));
}
#endif

#ifndef HAVE_ACF_ZERO
void acf_zero(acf_t z)
{
	arf_zero(acf_realref(z));
	arf_zero(acf_imagref(z));
	return;
}
#endif

#ifndef HAVE_ACF_ONE
void acf_one(acf_t z)
{
	arf_one (acf_realref(z));
	arf_zero(acf_imagref(z));
	return;
}
#endif

#ifndef HAVE_ACF_NAN
void acf_nan(acf_t z)
{
	arf_nan(acf_realref(z));
	arf_nan(acf_imagref(z));
	return;
}
#endif

#ifndef HAVE_ACF_CONJ
void acf_conj(acf_t z, const acf_t x)
{
	arf_set(acf_realref(z), acf_realref(x));
	arf_neg(acf_imagref(z), acf_imagref(x));
	return;
}
#endif

#ifndef HAVE_ACF_DIV
int acf_div(acf_t z, const acf_t x, const acf_t y, slong prec, arf_rnd_t rnd)
{
	int a, b;
	arf_t u, v, w;
	arf_init(u);
	arf_init(v);
	arf_init(w);
	arf_mul(u, acf_realref(x), acf_realref(y), ARF_PREC_EXACT, ARF_RND_DOWN);
	arf_addmul(u, acf_imagref(x), acf_imagref(y), ARF_PREC_EXACT, ARF_RND_DOWN);
	arf_mul(v, acf_imagref(x), acf_realref(y), ARF_PREC_EXACT, ARF_RND_DOWN);
	arf_submul(v, acf_realref(x), acf_imagref(y), ARF_PREC_EXACT, ARF_RND_DOWN);
	arf_mul(w, acf_realref(y), acf_realref(y), ARF_PREC_EXACT, ARF_RND_DOWN);
	arf_addmul(w, acf_imagref(y), acf_imagref(y), ARF_PREC_EXACT, ARF_RND_DOWN);
	a = arf_div(acf_realref(z), u, w, prec, rnd) != 0;
	b = arf_div(acf_imagref(z), v, w, prec, rnd) != 0;
	arf_clear(u);
	arf_clear(v);
	arf_clear(w);
	return a | (b << 1);
}
#endif

#ifndef HAVE_ACF_DIV_UI
int acf_div_ui(acf_t z, const acf_t x, ulong y, slong prec, arf_rnd_t rnd)
{
	int a, b;
	a = arf_div_ui(acf_realref(z), acf_realref(x), y, prec, rnd);
	b = arf_div_ui(acf_imagref(z), acf_imagref(x), y, prec, rnd);
	return a | (b << 1);
}
#endif

#ifndef HAVE_ARB_SET_ROUND_UI
void arb_set_round_ui(arb_t z, ulong x, slong prec)
{
	arf_set_ui(arb_midref(z), x);
	mag_zero(arb_radref(z));
	arb_set_round(z, z, prec);
	return;
}
#endif

#ifndef HAVE_ARB_SET_ROUND_SI
void arb_set_round_si(arb_t z, slong x, slong prec)
{
	arf_set_si(arb_midref(z), x);
	mag_zero(arb_radref(z));
	arb_set_round(z, z, prec);
	return;
}
#endif

#ifndef HAVE_ARB_SET_MAG
void arb_set_mag(arb_t z, const mag_t x)
{
	arf_set_mag(arb_midref(z), x);
	mag_zero(arb_radref(z));
	return;
}
#endif

#ifndef HAVE_ARB_FDIV_Q
void arb_fdiv_q(arb_t z, const arb_t x, const arb_t y, slong prec)
{
	arb_t q;
	arb_init(q);
	arb_div(q, x, y, prec);
	arb_floor(z, q, prec);
	arb_clear(q);
	return;
}
#endif

#ifndef HAVE_ARB_FDIV_R
void arb_fdiv_r(arb_t z, const arb_t x, const arb_t y, slong prec)
{
	arb_t q, f;
	arb_init(q);
	arb_init(f);
	arb_div(q, x, y, prec);
	arb_floor(f, q, prec);
	arb_set(z, x);
	arb_submul(z, f, y, prec);
	arb_clear(q);
	arb_clear(f);
	return;
}
#endif

#ifndef HAVE_ARB_ARG
void arb_arg(arb_t z, const arb_t x, slong prec)
{
	acb_t x0;
	acb_init(x0);
	arb_set(acb_realref(x0), x);
	acb_arg(z, x0, prec);
	acb_clear(x0);
	return;
}
#endif

#ifndef HAVE_ARB_LOG_BASE
void arb_log_base(arb_t z, const arb_t x, const arb_t b, slong prec)
{
	arb_t s, t;
	arb_init(s);
	arb_init(t);
	arb_log(s, x, prec);
	arb_log(t, b, prec);
	arb_div(z, s, t, prec);
	arb_clear(s);
	arb_clear(t);
	return;
}
#endif

#ifndef HAVE_ARB_POLYGAMMA
void arb_polygamma(arb_t z, const arb_t s, const arb_t x, slong prec)
{
	acb_t z0, s0, x0;
	acb_init(z0);
	acb_init(s0);
	acb_init(x0);
	arb_set(acb_realref(s0), s);
	arb_set(acb_realref(x0), x);
	acb_polygamma(z0, s0, x0, prec);
	arb_set(z, acb_realref(z0));
	acb_clear(z0);
	acb_clear(s0);
	acb_clear(x0);
	return;
}
#endif

#ifndef HAVE_ACB_LOG_BASE
void acb_log_base(acb_t z, const acb_t x, const acb_t b, slong prec)
{
	acb_t s, t;
	acb_init(s);
	acb_init(t);
	acb_log(s, x, prec);
	acb_log(t, b, prec);
	acb_div(z, s, t, prec);
	acb_clear(s);
	acb_clear(t);
	return;
}
#endif

#ifndef HAVE_ACB_LOG_BASE_UI
void acb_log_base_ui(acb_t z, const acb_t x, ulong b, slong prec)
{
	acb_t t;
	acb_init(t);
	acb_set_ui(t, b);
	acb_log_base(z, x, t, prec);
	acb_clear(t);
	return;
}
#endif
