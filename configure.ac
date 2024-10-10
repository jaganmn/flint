AC_PREREQ([2.72])
AC_INIT([flint],
        [0.0.1],
        [https://github.com/jaganmn/flint/issues],
        [],
        [https://github.com/jaganmn/flint])
AC_CONFIG_SRCDIR([man/flint-class.Rd])
AC_CONFIG_HEADERS([src/config.h])

tail=${PACKAGE_VERSION}
head=${tail%%.*}
AC_DEFINE_UNQUOTED([PACKAGE_VERSION_MAJOR], [${head}],
                   [Define to the first field of the version string.])
tail=${tail#*.}
head=${tail%%.*}
AC_DEFINE_UNQUOTED([PACKAGE_VERSION_MINOR], [${head}],
                   [Define to the second field of the version string.])
tail=${tail#*.}
head=${tail}
AC_DEFINE_UNQUOTED([PACKAGE_VERSION_PATCHLEVEL], [${head}],
                   [Define to the third field of the version string.])

AC_ARG_VAR([R_HOME],
           [R installation directory])
AC_ARG_VAR([PKG_CONFIG],
           [path to pkg-config utility])
AC_ARG_VAR([PKG_CONFIG_PATH],
           [path to be prepended to pkg-config's default search path])
AC_ARG_VAR([PKG_CONFIG_LIBDIR],
           [path overriding pkg-config's default search path])
AC_ARG_VAR([R_FLINT_CPPFLAGS],
           [preprocessor options for the C compiler, overriding pkg-config])
AC_ARG_VAR([R_FLINT_CFLAGS],
           [debugging and optimization options for the C compiler, overriding pkg-config])
AC_ARG_VAR([R_FLINT_LDFLAGS],
           [options excluding libraries for the linker, overriding pkg-config])
AC_ARG_VAR([R_FLINT_LIBS],
           [libraries for the linker, overriding pkg-config [default=${R_FLINT_LIBS_DEFAULT=-lflint -lmpfr -lgmp}]])

AC_PROG_CC([])
AC_MSG_CHECKING([for R])
if test "x${R_HOME=`R RHOME`}" != "x" &&
   test -x "${R_HOME}/bin/R" &&
   "${R_HOME}/bin/R" --version >/dev/null 2>&1; then
	AC_MSG_RESULT([${R_HOME}/bin/R])
else
	AC_MSG_RESULT([no])
	AC_MSG_ERROR([install R before configuring R packages])
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
LDFLAGS=`"${R_HOME}/bin/R" CMD config LDFLAGS`
LIBS=

have_libflint=no
if test "x${R_FLINT_CPPFLAGS+x}" = "x" &&
   test "x${R_FLINT_CFLAGS+x}"   = "x" &&
   test "x${R_FLINT_LDFLAGS+x}"  = "x" &&
   test "x${R_FLINT_LIBS+x}"     = "x"; then
	dnl All R_FLINT_* are unset.
	AC_PATH_PROGS([PKG_CONFIG], [pkg-config pkgconf], [])
	if test "x${PKG_CONFIG}" = "x"; then
		R_FLINT_LIBS="${R_FLINT_LIBS_DEFAULT}"
	else
		AC_MSG_CHECKING([for flint.pc in pkg-config search path])
		if "${PKG_CONFIG}" --exists flint; then
			AC_MSG_RESULT([yes])
			save_CPPFLAGS="${CPPFLAGS}"
			save_LDFLAGS="${LDFLAGS}"
			R_FLINT_CPPFLAGS="`"${PKG_CONFIG}" --cflags flint`"
			R_FLINT_LDFLAGS="`"${PKG_CONFIG}" --libs-only-L --libs-only-other flint`"
			R_FLINT_LIBS="`"${PKG_CONFIG}" --libs-only-l flint`"
			CPPFLAGS="${CPPFLAGS} ${FLINT_CPPFLAGS}"
			LDFLAGS="${LDFLAGS} ${FLINT_LDFLAGS}"
			AC_MSG_NOTICE([checking if --libs suffices for linking libflint])
			AC_CHECK_LIB([flint], [flint_malloc], [have_libflint=yes], [],
			             [${R_FLINT_LIBS}])
			if test "x${have_libflint}" = "xno"; then
			R_FLINT_LIBS="`"${PKG_CONFIG}" --libs --static flint`"
			AC_MSG_NOTICE([checking if --libs --static suffices for linking libflint])
			AC_CHECK_LIB([flint], [flint_malloc], [have_libflint=yes], [],
			             [${R_FLINT_LIBS}])
			if test "x${have_libflint}" = "xno"; then
			AC_MSG_NOTICE([reverting to default compiler and linker options])
			R_FLINT_CPPFLAGS=
			R_FLINT_LDFLAGS=
			R_FLINT_LIBS="${R_FLINT_LIBS_DEFAULT}"
			fi
			fi
			CPPFLAGS="${save_CPPFLAGS}"
			LDFLAGS="${save_LDFLAGS}"
		else
			AC_MSG_RESULT([no])
			R_FLINT_LIBS="${R_FLINT_LIBS_DEFAULT}"
		fi
	fi
else
	dnl At least one R_FLINT_* is set.
	: {R_FLINT_LIBS=${R_FLINT_LIBS_DEFAULT}}
fi

AC_MSG_NOTICE([using R_FLINT_CPPFLAGS="${R_FLINT_CPPFLAGS}"])
AC_MSG_NOTICE([using R_FLINT_CFLAGS="${R_FLINT_CFLAGS}"])
AC_MSG_NOTICE([using R_FLINT_LDFLAGS="${R_FLINT_LDFLAGS}"])
AC_MSG_NOTICE([using R_FLINT_LIBS="${R_FLINT_LIBS}"])
CPPFLAGS="${CPPFLAGS} ${R_FLINT_CPPFLAGS}"
CFLAGS="${CFLAGS} ${R_FLINT_CFLAGS}"
LDFLAGS="${LDFLAGS} ${R_FLINT_LDFLAGS}"
LIBS="${LIBS} ${R_FLINT_LIBS}"

AC_CHECK_LIB([gmp], [__gmpz_init], [:],
             [AC_MSG_ERROR([failed to link GNU MP library, see https://gmplib.org/])])
AC_CHECK_LIB([mpfr], [mpfr_init], [:],
             [AC_MSG_ERROR([failed to link GNU MPFR library, see https://www.mpfr.org/])])
AC_CHECK_LIB([flint], [fmpz_init], [:],
             [AC_MSG_ERROR([failed to link FLINT library, see https://flintlib.org/])])

AC_CHECK_HEADER([gmp.h], [],
                [AC_MSG_ERROR([failed to compile GNU MP header, see https://gmplib.org/])])
AC_CHECK_HEADER([mpfr.h], [],
                [AC_MSG_ERROR([failed to compile GNU MPFR header, see https://www.mpfr.org/])])
AC_CHECK_HEADER([flint/flint.h], [],
                [AC_MSG_ERROR([failed to compile FLINT header, see https://flintlib.org/])])

AC_CHECK_TYPE([mpfr_uprec_t], [],
              [AC_MSG_ERROR([GNU MPFR missing unsigned precision type])],
              [[#include <mpfr.h>]])
AC_CHECK_TYPE([mpfr_uexp_t], [],
              [AC_MSG_ERROR([GNU MPFR missing unsigned exponent type])],
              [[#include <mpfr.h>]])
AC_CHECK_FUNC([flint], [arb_init], [:],
              [AC_MSG_ERROR([FLINT missing Arb sublibrary])])

AC_MSG_CHECKING([for int with bit width 32])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
int main()
{
	return sizeof(int) != 4;
}
                  ]])],
                  [AC_MSG_RESULT([yes])],
                  [
                   AC_MSG_RESULT([no])
                   AC_MSG_ERROR([package requires a 32-bit int])
                  ])

AC_MSG_CHECKING([for size_t with bit width at least 64])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#include <stddef.h>
int main()
{
	return sizeof(size_t) < 8;
}
                  ]])],
                  [AC_MSG_RESULT([yes])],
                  [
                   AC_MSG_RESULT([no])
                   AC_MSG_ERROR([package requires a >= 64-bit size_t])
                  ])

AC_MSG_CHECKING([for mpfr_exp_t in range of long int])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#include <limits.h>
#include <mpfr.h>
int main()
{
	return sizeof(mpfr_exp_t) > sizeof(long int);
}
                  ]])],
                  [AC_MSG_RESULT([yes])],
                  [
                   AC_MSG_RESULT([no])
                   AC_MSG_ERROR([GNU MPFR exponent can overflow long int])
                  ])

AC_MSG_CHECKING([for consistent limb size])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#include <limits.h>
#include <gmp.h>
#include <flint/flint.h>
int main()
{
	return FLINT_BITS != CHAR_BIT * sizeof(mp_limb_t) ||
		FLINT_BITS != mp_bits_per_limb;
}
                  ]])],
                  [AC_MSG_RESULT([yes])],
                  [
                   AC_MSG_RESULT([no])
                   AC_MSG_FAILURE([FLINT and GNU MP installations seem incompatible])
                  ])

AC_SUBST(R_FLINT_CPPFLAGS)
AC_SUBST(R_FLINT_CFLAGS)
AC_SUBST(R_FLINT_LDFLAGS)
AC_SUBST(R_FLINT_LIBS)
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
