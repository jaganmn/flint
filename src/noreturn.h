/* flint-3.1.x/src/flint.h.in is broken due to nonportable */
/* use of keyword '_Noreturn' and keyword macro 'noreturn' */

#undef noreturn
#ifdef HAVE_D_MUL_2EXP
# define noreturn _Noreturn
#endif
#undef _Noreturn
#define _Noreturn R_FLINT_NORETURN

#if defined __STDC_VERSION__ && __STDC_VERSION__ >= 202311L
# define R_FLINT_NORETURN [[__noreturn__]]
#elif defined __STDC_VERSION__ && __STDC_VERSION__ >= 201112L
# undef _Noreturn
# define R_FLINT_NORETURN _Noreturn
#elif defined __GNUC__
# define R_FLINT_NORETURN __attribute__ ((__noreturn__))
#else
# define R_FLINT_NORETURN
#endif
