/* flint-3.1.x/src/flint.h.in is broken due to nonportable */
/* use of keyword '_Noreturn' and keyword macro 'noreturn' */
#if defined __STDC_VERSION__ && __STDC_VERSION__ >= 201112L
# include <stdnoreturn.h>
#else
# undef _Noreturn
# define _Noreturn
# undef noreturn
# define noreturn
#endif
