#ifndef R_FLINT_ACF_H
#define R_FLINT_ACF_H

#include <flint/arf_types.h>

typedef struct
{
	arf_struct real;
	arf_struct imag;
} acf_struct;

typedef acf_struct acf_t[1];
typedef acf_struct * acf_ptr;
typedef const acf_struct * acf_srcptr;

#define acf_realref(x) (&(x)->real)
#define acf_imagref(x) (&(x)->imag)

static R_INLINE
void acf_init(acf_t x)
{
	arf_init(acf_realref(x));
	arf_init(acf_imagref(x));
	return;
}

static R_INLINE
void acf_clear(acf_t x)
{
	arf_clear(acf_realref(x));
	arf_clear(acf_imagref(x));
	return;
}

static R_INLINE
void acf_set(acf_t y, const acf_t x)
{
	arf_set(acf_realref(y), acf_realref(x));
	arf_set(acf_imagref(y), acf_imagref(x));
	return;
}

static R_INLINE
void acf_zero(acf_t res)
{
	arf_zero(acf_realref(res));
	arf_zero(acf_imagref(res));
	return;
}

static R_INLINE
int acf_equal(const acf_t x, const acf_t y)
{
	return
		arf_equal(acf_realref(x), acf_realref(y)) &&
		arf_equal(acf_imagref(x), acf_imagref(y));
}

#endif /* ! defined (R_FLINT_ACF_H) */
