AC_PREREQ([2.72])
AC_INIT([flint],
        [0.0.1],
        [https://github.com/jaganmn/flint/issues],
        [],
        [https://github.com/jaganmn/flint])
AC_CONFIG_SRCDIR([man/flint-class.Rd])
AC_CONFIG_HEADERS([src/config.h])

AC_DEFINE_UNQUOTED([PACKAGE_VERSION_MAJOR],
                   [`echo "${PACKAGE_VERSION}" | cut -f 1 -d .`],
                   [Define to the first field of the version string.])
AC_DEFINE_UNQUOTED([PACKAGE_VERSION_MINOR],
                   [`echo "${PACKAGE_VERSION}" | cut -f 2 -d .`],
                   [Define to the second field of the version string.])
AC_DEFINE_UNQUOTED([PACKAGE_VERSION_PATCHLEVEL],
                   [`echo "${PACKAGE_VERSION}" | cut -f 3 -d .`],
                   [Define to the third field of the version string.])

AC_ARG_VAR([FLINT_CPPFLAGS],
           [additional flags for the C preprocessor])
AC_ARG_VAR([FLINT_CFLAGS],
           [additional flags for the C compiler])
AC_ARG_VAR([FLINT_LDFLAGS],
           [additional flags for the linker (not -l)])

: ${R_HOME=`R RHOME`}
if test "x${R_HOME}" = "x"; then
	echo "could not determine R_HOME"
	exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS` ${FLINT_CPPFLAGS}
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS` ${FLINT_CFLAGS}
LDFLAGS=`"${R_HOME}/bin/R" CMD config LDFLAGS` ${FLINT_LDFLAGS}

AC_CHECK_LIB([gmp], [__gmpz_init], [],
             [AC_MSG_ERROR([GNU MP not found, see https://gmplib.org/])])
AC_CHECK_LIB([mpfr], [mpfr_init], [],
             [AC_MSG_ERROR([GNU MPFR not found, see https://www.mpfr.org/])])
AC_CHECK_LIB([flint], [arb_init], [],
             [AC_MSG_ERROR([FLINT not found, see https://flintlib.org/])])

AC_CHECK_HEADER([gmp.h], [],
                [AC_MSG_ERROR([GNU MP not found, see https://gmplib.org/])])
AC_CHECK_HEADER([mpfr.h], [],
                [AC_MSG_ERROR([GNU MPFR not found, see https://www.mpfr.org/])])
AC_CHECK_HEADER([flint/flint.h], [],
                [AC_MSG_ERROR([FLINT not found, see https://flintlib.org/])])

FLINT_LIBS=${LIBS}

AC_SUBST(FLINT_CPPFLAGS)
AC_SUBST(FLINT_CFLAGS)
AC_SUBST(FLINT_LDFLAGS)
AC_SUBST(FLINT_LIBS)
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
