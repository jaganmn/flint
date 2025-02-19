/* flint-3.1.x/src/flint.h.in is broken due to nonportable */
/* use of keyword macro 'noreturn'                         */
#if __STDC_VERSION__ >= 201112L && __STDC_VERSION__ < 202311L
# include <stdnoreturn.h>
#else
# undef _Noreturn
# define _Noreturn
# undef noreturn
# define noreturn
#endif
