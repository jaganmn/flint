#ifndef R_FLINT_FALLBACK_H
#define R_FLINT_FALLBACK_H

#include "flint.h"

slong fmpq_clog_ui(const fmpq_t, ulong);

double mag_get_d_lower(const mag_t);
void mag_div_ui_lower(mag_t, const mag_t, ulong);
void mag_log1p_lower(mag_t, const mag_t);
void mag_expm1_lower(mag_t, const mag_t);

int acf_is_zero(const acf_t);
int acf_is_nan(const acf_t);
int acf_is_inf(const acf_t);
int acf_is_finite(const acf_t);
void acf_zero(acf_t);
void acf_one(acf_t);
void acf_nan(acf_t);
void acf_conj(acf_t, const acf_t);
int acf_div(acf_t, const acf_t, const acf_t, slong, arf_rnd_t);
int acf_div_ui(acf_t, const acf_t, ulong, slong, arf_rnd_t);

void arb_set_round_ui(arb_t, ulong, slong);
void arb_set_round_si(arb_t, slong, slong);
void arb_set_mag(arb_t, const mag_t);
void arb_fdiv_q(arb_t, const arb_t, const arb_t, slong);
void arb_fdiv_r(arb_t, const arb_t, const arb_t, slong);
void arb_arg(arb_t, const arb_t, slong);
void arb_log_base(arb_t, const arb_t, const arb_t, slong);
void arb_polygamma(arb_t, const arb_t, const arb_t, slong);

void acb_log_base(acb_t, const acb_t, const acb_t, slong);
void acb_log_base_ui(acb_t, const acb_t, ulong, slong);

#endif /* ! defined (R_FLINT_FALLBACK_H) */
