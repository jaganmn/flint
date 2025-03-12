AC_PREREQ([2.72])
AC_INIT([flint],
        [0.0.4],
        [https://github.com/jaganmn/flint/issues],
        [],
        [https://github.com/jaganmn/flint])
AC_CONFIG_SRCDIR([man/flint-class.Rd])
AC_CONFIG_HEADERS([src/config.h])

save_IFS="${IFS}"; IFS=.
set x ${PACKAGE_VERSION}
shift; v1=$1; v2=$2; shift; shift; v3=$*
set --
IFS="${save_IFS}"
AC_DEFINE_UNQUOTED([PACKAGE_VERSION_MAJOR], [${v1}],
                   [Define to the first field of the version string.])
AC_DEFINE_UNQUOTED([PACKAGE_VERSION_MINOR], [${v2}],
                   [Define to the second field of the version string.])
AC_DEFINE_UNQUOTED([PACKAGE_VERSION_PATCHLEVEL], [${v3}],
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
           [options (except -lx) for the linker, overriding pkg-config])
AC_ARG_VAR([R_FLINT_LIBS],
           [options (only -lx) and libraries for the linker, overriding pkg-config [default=-lflint -lmpfr -lgmp]])

AC_MSG_CHECKING([for R])
if test "x${R_HOME=`R RHOME`}" != "x" &&
   test -x "${R_HOME}/bin/R" &&
   "${R_HOME}/bin/R" --version >/dev/null 2>&1; then
	AC_MSG_RESULT([${R_HOME}/bin/R])
else
	AC_MSG_RESULT([no])
	AC_MSG_ERROR([install R before configuring R packages])
fi

AC_PROG_CC([])
AC_PROG_SED([])

R_INCLUDE_DIR=`"${R_HOME}/bin/R" --no-echo --vanilla -e "cat(R.home(\"include\"))"`

CC=`"${R_HOME}/bin/R" CMD config CC`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
LDFLAGS=`"${R_HOME}/bin/R" CMD config LDFLAGS`
LIBS=
if test "x${R_FLINT_CPPFLAGS+x}" = "x" &&
   test "x${R_FLINT_CFLAGS+x}" = "x" &&
   test "x${R_FLINT_LDFLAGS+x}" = "x" &&
   test "x${R_FLINT_LIBS+x}" = "x"; then
	dnl All R_FLINT_* are unset.
	AC_PATH_PROGS([PKG_CONFIG], [pkg-config pkgconf], [])
	if test "x${PKG_CONFIG}" != "x" &&
	   test -x "${PKG_CONFIG}" &&
	   "${PKG_CONFIG}" --version >/dev/null 2>&1; then
		dnl We seem to have a working pkg-config.
		for pkg in gmp mpfr flint; do
			AC_MSG_CHECKING([for ${pkg}.pc in pkg-config search path])
			if "${PKG_CONFIG}" --exists ${pkg}; then
				AC_MSG_RESULT([yes])
				save_CPPFLAGS="${CPPFLAGS}"
				save_LDFLAGS="${LDFLAGS}"
				save_LIBS="${LIBS}"
				pkg_CPPFLAGS=`"${PKG_CONFIG}" --cflags ${pkg}`
				pkg_LDFLAGS=`"${PKG_CONFIG}" --libs-only-L --libs-only-other ${pkg}`
				pkg_LIBS=`"${PKG_CONFIG}" --libs-only-l ${pkg}`
				pkg_LIBS_extra=
				if test "x${pkg}" = "xflint" &&
				   "${PKG_CONFIG}" --exists "flint < 3.1.0"; then
					dnl FLINT 3.0.x installs broken flint.pc
					pkg_LIBS_extra=-lflint
				fi
				pkg_LIBS="${pkg_LIBS_extra} ${pkg_LIBS}"
				CPPFLAGS="${pkg_CPPFLAGS} ${R_FLINT_CPPFLAGS} ${CPPFLAGS}"
				LDFLAGS="${pkg_LDFLAGS} ${R_FLINT_CPPFLAGS} ${LDFLAGS}"
				LIBS="${R_FLINT_LIBS} ${LIBS}"
				AC_MSG_NOTICE([checking whether --libs suffices for linking lib${pkg}])
				case ${pkg} in
					gmp ) pkg_fn=__gmpz_init ;;
					mpfr ) pkg_fn=mpfr_init ;;
					flint ) pkg_fn=fmpz_init ;;
				esac
				AC_CHECK_LIB([${pkg}], [${pkg_fn}],
				             [pkg_found=yes], [pkg_found=no],
				             [${pkg_LIBS}])
				if test "x${pkg_found}" = "xno"; then
				pkg_LIBS=`"${PKG_CONFIG}" --libs-only-l --static flint`
				pkg_LIBS="${pkg_LIBS_extra} ${pkg_LIBS}"
				AC_MSG_NOTICE([checking whether --libs --static suffices for linking lib${pkg}])
				AC_CHECK_LIB([${pkg}], [${pkg_fn}],
				             [pkg_found=yes], [pkg_found=no],
				             [${pkg_LIBS}])
				if test "x${pkg_found}" = "xno"; then
				pkg_CPPFLAGS=
				pkg_LDFLAGS=
				pkg_LIBS=-l${pkg}
				fi
				fi
				R_FLINT_CPPFLAGS="${pkg_CPPFLAGS} ${R_FLINT_CPPFLAGS}"
				R_FLINT_LDFLAGS="${pkg_LDFLAGS} ${R_FLINT_LDFLAGS}"
				R_FLINT_LIBS="${pkg_LIBS} ${R_FLINT_LIBS}"
				CPPFLAGS="${save_CPPFLAGS}"
				LDFLAGS="${save_LDFLAGS}"
				LIBS="${save_LIBS}"
			else
				AC_MSG_RESULT([no])
				R_FLINT_LIBS="-l${pkg} ${R_FLINT_LIBS}"
			fi
		done
		dnl Use pkg-config machinery to deduplicate options.
		printf "Name: config\n"                                   > config.pc
		printf "Description: The config library\n"               >> config.pc
		printf "URL: https://config.org/\n"                      >> config.pc
		printf "Version: %s\n" "${PACKAGE_VERSION}"              >> config.pc
		printf "Cflags: %s\n" "${R_FLINT_CPPFLAGS}"              >> config.pc
		printf "Libs: %s\n" "${R_FLINT_LDFLAGS} ${R_FLINT_LIBS}" >> config.pc
		R_FLINT_CPPFLAGS=`"${PKG_CONFIG}" --cflags config.pc`
		R_FLINT_LDFLAGS=`"${PKG_CONFIG}" --libs-only-L --libs-only-other config.pc`
		R_FLINT_LIBS=`"${PKG_CONFIG}" --libs-only-l config.pc`
		rm -f config.pc
		dnl Handle -lmpfr and -lgmp manually, as we have seen flint.pc and
		dnl mpfr.pc misusing Requires and Libs, polluting --libs-only-l output.
		for opt in -lmpfr -lgmp; do
			R_FLINT_LIBS=`echo "${R_FLINT_LIBS}" | ${SED} -e ":l" -e "s/\(.*\)${opt} \(.*${opt}.*\)/\1\2/" -e "tl"`
		done
	else
		dnl We do not seem to have a working pkg-config.
		R_FLINT_LIBS="-lflint -lmpfr -lgmp"
	fi
else
	dnl At least one R_FLINT_* is set.
	: {R_FLINT_LIBS=-lflint -lmpfr -lgmp}
fi
CPPFLAGS="${CPPFLAGS} ${R_FLINT_CPPFLAGS}"
CFLAGS="${CFLAGS} ${R_FLINT_CFLAGS}"
LDFLAGS="${LDFLAGS} ${R_FLINT_LDFLAGS}"
LIBS="${LIBS} ${R_FLINT_LIBS}"

AC_MSG_NOTICE([using R_FLINT_CPPFLAGS="${R_FLINT_CPPFLAGS}"])
AC_MSG_NOTICE([using R_FLINT_CFLAGS="${R_FLINT_CFLAGS}"])
AC_MSG_NOTICE([using R_FLINT_LDFLAGS="${R_FLINT_LDFLAGS}"])
AC_MSG_NOTICE([using R_FLINT_LIBS="${R_FLINT_LIBS}"])

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
                [AC_MSG_ERROR([failed to compile FLINT header, see https://flintlib.org/])],
                [[
#undef _Noreturn
#define _Noreturn
#undef noreturn
#define noreturn
                ]])

AC_CHECK_TYPE([mpfr_uprec_t], [],
              [AC_MSG_ERROR([GNU MPFR missing unsigned precision type])],
              [[#include <mpfr.h>]])
AC_CHECK_TYPE([mpfr_uexp_t], [],
              [AC_MSG_ERROR([GNU MPFR missing unsigned exponent type])],
              [[#include <mpfr.h>]])

AC_CHECK_SIZEOF([int])
case ${ac_cv_sizeof_int} in
	4) : ;;
	*) AC_MSG_ERROR([package requires 32-bit int]) ;;
esac
AC_CHECK_SIZEOF([long int])
AC_CHECK_SIZEOF([long long int])
AC_CHECK_SIZEOF([size_t], [], [[#include <stddef.h>]])
AC_CHECK_SIZEOF([mp_limb_t], [], [[#include <gmp.h>]])
case ${ac_cv_sizeof_mp_limb_t} in
	4) : ;;
	8) AC_DEFINE([R_FLINT_ABI_64], [1],
	             [Define if and only if SIZEOF_MP_LIMB_T is 8.]) ;;
	*) AC_MSG_ERROR([package requires 32-bit or 64-bit limbs]) ;;
esac
case ${ac_cv_sizeof_mp_limb_t} in
	${ac_cv_sizeof_long_int})
	   : ;;
	${ac_cv_sizeof_long_long_int})
	   AC_DEFINE([R_FLINT_ABI_LL], [1],
	             [Define if and only if SIZEOF_MP_LIMB_T is equal to SIZEOF_LONG_LONG_INT and not equal to SIZEOF_LONG_INT.]) ;;
	*) AC_MSG_ERROR([package requires long or long long limbs]) ;;
esac
AC_CHECK_SIZEOF([mpfr_prec_t], [], [[#include <mpfr.h>]])
AC_CHECK_SIZEOF([mpfr_exp_t], [], [[#include <mpfr.h>]])

AC_MSG_CHECKING([for R_XLEN_T_MAX in range of mp_limb_t])
save_CPPFLAGS="${CPPFLAGS}"
CPPFLAGS="${CPPFLAGS} -I${R_INCLUDE_DIR}"
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#include <gmp.h>
#include <Rinternals.h>
int main()
{
	return R_XLEN_T_MAX > (mp_limb_t) -1;
}
                  ]])],
                  [AC_MSG_RESULT([yes])],
                  [
                   AC_MSG_RESULT([no])
                   AC_MSG_ERROR([basic vector length can overflow mp_limb_t])
                  ])
CPPFLAGS="${save_CPPFLAGS}"

AC_MSG_CHECKING([for mpfr_uprec_t in range of mp_limb_t])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#include <mpfr.h>
int main()
{
	return sizeof(mpfr_uprec_t) > sizeof(mp_limb_t);
}
                  ]])],
                  [AC_MSG_RESULT([yes])],
                  [
                   AC_MSG_RESULT([no])
                   AC_MSG_ERROR([GNU MPFR precision can overflow mp_limb_t])
                  ])

AC_MSG_CHECKING([for mpfr_uexp_t in range of mp_limb_t])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#include <mpfr.h>
int main()
{
	return sizeof(mpfr_uexp_t) > sizeof(mp_limb_t);
}
                  ]])],
                  [AC_MSG_RESULT([yes])],
                  [
                   AC_MSG_RESULT([no])
                   AC_MSG_ERROR([GNU MPFR exponent can overflow mp_limb_t])
                  ])

AC_MSG_CHECKING([for mp_limb_t in range of size_t])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#include <stddef.h>
#include <gmp.h>
int main()
{
	return sizeof(mp_limb_t) > sizeof(size_t);
}
                  ]])],
                  [AC_MSG_RESULT([yes])],
                  [
                   AC_MSG_RESULT([no])
                   AC_MSG_ERROR([flint vector length can overflow size_t])
                  ])

AC_CHECK_FUNC([arb_init], [],
              [AC_MSG_ERROR([FLINT missing Arb sublibrary])])

AC_CHECK_FUNCS([acf_abs acf_arg acf_conj acf_div_ui acf_is_finite acf_is_inf acf_is_nan acf_is_zero acf_nan acf_one acf_zero fmpq_clog_ui mag_div_ui_lower mag_expm1_lower mag_get_d_lower mag_log1p_lower], [], [])

dnl 'd_mul_2exp' availability must be tested specially as it is declared
dnl static in flint/double_extras.h
AC_MSG_CHECKING([for d_mul_2exp])
AC_LINK_IFELSE([AC_LANG_SOURCE([[
#undef _Noreturn
#define _Noreturn
#undef noreturn
#define noreturn
#include <flint/double_extras.h>
int main()
{
	d_mul_2exp(0.0, 0);
	return 0;
}
               ]])],
               [
                AC_MSG_RESULT([yes])
                AC_DEFINE([HAVE_D_MUL_2EXP], [1],
                          [Define to 1 if you have the 'd_mul_2exp' function.])
               ],
               [
                AC_MSG_RESULT([no])
               ])

AC_SUBST(R_FLINT_CPPFLAGS)
AC_SUBST(R_FLINT_CFLAGS)
AC_SUBST(R_FLINT_LDFLAGS)
AC_SUBST(R_FLINT_LIBS)
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
