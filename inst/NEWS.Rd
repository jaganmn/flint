\name{NEWS}
\title{News for \R{} Package \pkg{flint}}
\newcommand{\GH}{\href{https://github.com/jaganmn/flint/issues/#1}{GH##1}}
\section{Changes in version 0.1.3 (2025-12-15)}{
  \subsection{Description}{
    \itemize{
      \item The maintainer e-mail address is updated.
    }
  }
  \subsection{Compatibility}{
    \itemize{
      \item Tests of \code{.initForeign("Rmpfr")} are skipped if
      \pkg{Rmpfr} is too old.  Versions older than 1.1-2 did not detect
      \code{sizeof(mpfr_exp_t) != sizeof(mp_limb_t)}, so they could
      initialize \code{mpfr1} objects with \code{exp} slot of wrong
      length.
    }
  }
  \subsection{Documentation}{
    \itemize{
      \item Topic \code{flint-class} wrongly stated that the S3 default
      method for \code{diff} works for \code{x} inheriting from virtual
      class \code{flint}.

      \item Topic \code{format-methods} wrongly stated that methods
      for \code{x} inheriting from \code{arb} or \code{acb} required
      \code{rnd} of length 2.
    }
  }
  \subsection{Generic Functions}{
    \itemize{
      \item S4 generic functions \code{isSigned}, \code{isComplex}, and
      \code{isFloating} for testing whether the type or class of the
      argument represents signed, complex, and floating-point numbers,
      respectively.
    }
  }
  \subsection{Methods}{
    \itemize{
      \item S4 methods are defined for generic (or made-generic)
      functions \code{diff}, \code{diffinv}, \code{kronecker}, and
      \code{outer}.

      \item S4 methods for group generic functions \code{Complex} and
      \code{Math} are more completely implemented for classes
      \code{mag}, \code{arf}, and \code{acf}, for parity with classes
      \code{arb} and \code{acb}.

      \item \code{/} is implemented for \code{acf}.  \code{^} is
      implemented for \code{arf} and \code{acf}.  \code{\%\%} and
      \code{\%/\%} are implemented for \code{mag}, \code{arf} and
      \code{arb}.

      \item S4 generic functions \code{Real}, \code{Real<-},
      \code{Imag}, and \code{Imag<-} gain sensible default methods.
    }
  }
  \subsection{New Features}{
    \itemize{
      \item Class generator functions gain arguments \code{prec} and
      \code{rnd} (or \code{rnd.mag}) where appropriate providing users
      with greater control over conversions.  For example, by passing
      non-\code{NULL} \code{prec}, one can convert inexactly even if
      exact conversion is possible.

      \item \R{} function \code{flintRndMag} for getting and setting
      the global default rounding mode for \code{mag}.  S4 methods for
      \code{format} gain a new argument \code{rnd.mag}, parallel to
      \code{digits.mag}.

      \item \R{} functions \code{asMatrix} and \code{asArray}, analogues
      of \code{asVector}.

      \item \R{} functions \code{arb_integrate} and \code{acb_integrate}
      for integration of functions of one variable.

      \item \R{} function \code{arf_rk} for numerical solution of
      systems of ordinary differential equations.

      \item \R{} functions \code{arb_hypgeom_polygamma} and
      \code{arb_dirichlet_lerch_phi} are provided for symmetry with the
      \code{acb_*} ones, which they wrap.  \acronym{FLINT} provides no
      such entry points as of this writing.

      \item The global default precision, which one gets and sets with
      \R{} function \code{flintPrec}, may now be an object of class
      \code{slong}.  Previously, only numeric (of type \code{integer} or
      \code{double}) values were supported.

      \item The limits of \code{ulong} and \code{slong} are available as
      objects \code{ULONG_MAX}, \code{SLONG_MIN}, and \code{SLONG_MAX},
      defined at load time according to the ABI.

      \item Class generator functions support conversion from
      midpoint-radius interval types to point types, taking midpoints
      and discarding radii.

      \item Conversion from complex types to real types warns about
      nonzero imaginary parts.  Conversion from midpoint-radius interval
      types to point types warns about nonzero radii.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item \code{all.equal(x, y, check.class = FALSE)} failed to detect
      numerical differences between \code{x} and \code{y}, returning
      \code{TRUE} incorrectly when it detected no other differences.

      \item \code{asVector(x, strict = FALSE)} did not correctly handle
      data frames \code{x}.

      \item \code{tcrossprod(x, y)} misbehaved when neither \code{x} nor
      \code{y} was a matrix, throwing an error when the lengths were not
      equal and triggering a segfault when they were.

      \item Row and column labels were not printed for arrays of length
      zero.

      \item Segments of \code{names(c(\dots))} corresponding to a tagged
      argument of \code{c} were not constructed correctly when the
      tagged argument was named or when it was unnamed and of unit
      length.

      \item \code{as.character} passes \code{digits.mag = 8L} to
      \code{format}.  One method gave too many digits and others gave
      too few.

      \item \code{Arg(x)} wrongly gave \code{sign(x)} times \eqn{pi}
      for \code{x} of class \code{mag}, \code{arf}, or \code{arb}.

      \item \code{mag(0)/mag(0)} and \code{mag(Inf)/mag(Inf)} wrongly
      returned \code{mag(Inf)}.  Now they signal an error.

      \item Conversion to \code{ulong} or \code{slong} of \code{NaN},
      \code{-Inf}, or \code{Inf} stored as \code{mag} or \code{arf}
      signaled an unhelpful error (\dQuote{caught exception}).

      \item \code{diag<-} assigned to entries in the first row rather
      than to entries in the main diagonal.

      \item Conversion from \code{flint} to atomic withholds warnings
      until after memory is freed, in case warnings are caught by a
      handler.
    }
  }
  \subsection{Internals}{
    \itemize{
      \item \proglang{C} function \code{newObject} is modified so that
      it finds class definitions also when called from \code{.onLoad}.

      \item \proglang{C} function \code{newFlint}, simplifying low level
      instantiation of \code{flint} objects.

      \item Conditional definitions of functions tested by macro
      \code{AC_CHECK_FUNCS} are centralized in \file{src/fallback.c}.
      Declarations are provided by \file{src/fallback.h}.

      \item Enumerated types \code{R_flint_ops2_t} and
      \code{R_flint_ops1_t} designating binary and unary operations,
      used by \code{R_flint_*_op2} and \code{R_flint_*_ops1} as indices
      of \code{switch} statements.
    }
  }
}
\section{Changes in version 0.1.2 (2025-10-10)}{
  \subsection{Compatibility}{
    \itemize{
      \item Installation of \pkg{flint} version 0.1.1 failed under \R{}
      versions 4.[3-4].z because the non-generic function \code{asplit}
      in \pkg{base} did not admit a formal argument \code{drop}.
      Compatibility is restored by conditional definition in
      \code{.onLoad} of the S4 generic function and S4 method with
      signature \code{c(x = "flint")}.
    }
  }
}
\section{Changes in version 0.1.1 (2025-10-08)}{
  \subsection{Description}{
    \itemize{
      \item \code{Enhances: Rmpfr, gmp} as \code{.initForeign} defines
      methods for their classes.
    }
  }
  \subsection{Documentation}{
    \itemize{
      \item There was a typo in \file{man/arb_hypgeom_bessel_j.Rd},
      in the modified Bessel's equation, affecting only HTML and PDF
      output.
    }
  }
  \subsection{Methods}{
    \itemize{
      \item S4 methods are defined for generic (or made-generic)
      functions \code{aperm} and \code{asplit}.

      \item S4 methods for \code{anyDuplicated}, \code{duplicated}, and
      \code{unique} are amended to handle arrays \code{x}.  Accordingly
      they all gain an argument \code{MARGIN} with default value
      \code{1L}.

      \item \code{summary} coerces to data frame and dispatches when its
      argument is a matrix, following \code{summary.matrix}.

      \item \code{print} gains an argument \code{base}, which it passes
      to \code{format}.
    }
  }
  \subsection{New Features}{
    \itemize{
      \item \R{} functions \code{flintBits} and \code{flintBitsAccurate}
      can be used to get the number of bits necessary for exact
      representation of an integer or floating type number and the
      number of bits of an interval midpoint whose accuracy is
      guaranteed given the interval radius.  These are experimental
      and so only partially documented.

      \item \R{} function \code{.initForeign} can be used to define
      coercions between classes in \pkg{flint} and analogous classes in
      external packages, currently \pkg{Rmpfr} or \pkg{gmp}.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item Calls to \code{norm} matching signature
      \code{c(x = "flint", type = "missing")} signaled an error due to
      dispatch ambiguity, for some reason not detected by the call to
      \code{testInheritedMethods} in \file{tests/ambiguity.R}.

      \item Calls to \code{colSums} and friends whose return value would
      have length zero allowed undefined behaviour (divide-by-zero in
      integer division).

      \item Calls to \code{cbind} and \code{rbind} could fill dimension
      names incorrectly when obtaining components from the call.  Only
      the second component of the call (corresponding to the first
      actual argument) was \dQuote{seen}.

      \item \code{as.data.frame} signaled an error when its argument
      was an array due to a \dQuote{too late} assignment.

      \item \code{Summary} transferred attributes from its argument to
      its return value, in methods for classes \code{ulong} and
      \code{slong}.

      \item \code{mtfrm} passes \code{digits.mag = 0L} to \code{format}
      so that \code{mag} vectors are also formatted exactly.
    }
  }
  \subsection{Internals}{
    \itemize{
      \item External pointers are cleared by their finalizers.
    }
  }
}
\section{Changes in version 0.1.0 (2025-09-21)}{
  \subsection{Description}{
    \itemize{
      \item Martin Maechler is a contributor.
    }
  }
  \subsection{Significant User-Visible Changes}{
    \itemize{
      \item See section \sQuote{Classes} for backwards incompatible
      changes to the definition of virtual class \code{flint}.

      \item The class generator functions \code{.ulong}, \code{.slong},
      \ldots are removed and replaced with pairs of class generator
      functions \code{ulong}, \code{slong}, \ldots and
      \code{ulong.array}, \code{slong.array}, \ldots with much nicer
      usage; see, e.g., \code{help("ulong")} for details.

      \item S4 methods for \code{initialize}, which were nonstandard by
      virtue of not calling \code{callNextMethod} in order to dispatch
      the default method, are removed in order to facilitate calls to
      \code{new} by expert users wanting to set slots directly (rather
      than indirectly through class generator functions) or wanting to
      instantiate class extensions with additional slots, e.g.,
      \code{new("ulongWithExtraSlot", \var{ulong}, extra = .)}.  Users
      relying on the earlier behaviour of \code{new} should start using
      the documented class generator functions.

      \item \R{} function \code{flintIdentical} is removed and replaced
      by an S4 method for made-generic function \code{identical}.
    }
  }
  \subsection{Classes}{
    \itemize{
      \item Virtual class \code{flint} gains slots \code{dim} and
      \code{dimnames}, enabling representation of arrays.  Methods for
      \code{flint} and subclasses must handle these slots exactly as
      default methods handle the so-named attributes of traditional
      vectors.

      \item Virtual class \code{flint} accepts \code{NULL} as a valid
      value for its \code{names} slot.  \code{NULL} is used instead of
      \code{character(0L)} to indicate that an object is unnamed.  Now
      \code{character(0L)} is valid only for objects of length zero.

      \item New classes \code{OptionalInteger}, \code{OptionalList}, and
      \code{OptionalCharacter}, defined as unions of \code{NULL} and
      \code{integer}, \code{list}, or \code{character}.  They are used
      for slots \code{dim}, \code{dimnames}, and \code{names} of virtual
      class \code{flint}.
    }
  }
  \subsection{Methods}{
    \itemize{
      \item Many existing S4 methods are amended to handle changes in
      the definition of virtual class \code{flint}, in particular to
      handle arrays.  Notably, S4 methods for \code{[}, \code{[<-},
      \code{[[}, and \code{[[<-} are refactored completely to support
      array subscripts.

      \item S4 methods are defined for generic (or made-generic)
      functions \code{\%*\%}, \code{backsolve}, \code{cbind2},
      \code{chol}, \code{chol2inv}, \code{colMeans}, \code{colSums},
      \code{det}, \code{determinant}, \code{diag}, \code{diag<-},
      \code{dim}, \code{dim<-}, \code{dimnames}, \code{dimnames<-},
      \code{drop}, \code{is.array}, \code{is.matrix},
      \code{isSymmetric}, \code{norm}, \code{rbind2}, \code{rowMeans},
      \code{rowSums}, \code{solve}, \code{t}, and \code{tcrossprod}.
      It is not a coindicence that these are all more or less
      array-specific.

      \item S3 methods are defined for S3-turned-S4 generic functions.
      These methods call the S4 generic function so that S4 dispatch
      occurs if the S3 generic function is called.

      \item S4 methods are defined for generic functions \code{$} and
      \code{$<-}, signaling an informative error.

      \item An S4 method is defined for made-generic function
      \code{identical} so that \code{.xData} slots are handled specially
      when two \code{flint} objects are compared.
    }
  }
  \subsection{New Features}{
    \itemize{
      \item \proglang{C} library functions
      \code{arb_hypgeom_bessel_j}, \code{acb_hypgeom_bessel_j},
      \code{arb_hypgeom_bessel_y}, \code{acb_hypgeom_bessel_y},
      \code{arb_hypgeom_bessel_i}, \code{acb_hypgeom_bessel_i},
      \code{arb_hypgeom_bessel_k}, and \code{acb_hypgeom_bessel_k},
      which implement the Bessel and modified Bessel functions, are
      interfaced as \R{} functions with identical names and prototypes.
      Requested by Martin Maechler.

      \item \R{} function \code{flintClass} for getting the name of the
      direct nonvirtual subclass of virtual class \code{flint} from
      which the argument inherits, e.g., \code{"ulong"} if the class of
      the argument is \code{"ulong"} or \code{"ulongExtension"}.

      \item \R{} function \code{asVector} for coercion to basic vector
      classes and \code{flint} vector classes, optionally preserving
      attributes (at least \code{dim}, \code{dimnames}, \code{names}).
      It handles attributes more uniformly than \code{as.vector} and
      \code{as}, which may or may not drop attributes depending on the
      class of the argument and the target class.

      \item \R{} functions \code{cbind.flint} and \code{rbind.flint},
      similar in purpose to \code{c.flint}, enabling users to bypass
      the internal dispatch mechanism of \code{cbind} and \code{rbind}.

      \item It is now defined how \code{flintLength} and
      \code{flintSize} handle arguments not inheriting from virtual
      class \code{flint}.

      \item \code{Mid} and \code{Mid<-} are implemented for class
      \code{acb}, getting and setting the complex midpoint.  Thanks to
      Martin Maechler for the suggestion and patch (\GH{15}).

      \item S4 methods for \code{format} gain argument \code{digits.mag}
      and use that instead of \code{digits} to format \code{mag} and
      \code{mag} components of \code{arb} and \code{acb}.  The S4 method
      for \code{print} gains the same argument, passing it to
      \code{format}.
    }
  }
  \subsection{Bug Fixes}{
    \itemize{
      \item Due to a copy-paste bug, \code{Mid(x) <- value} never worked
      for \code{x} of class \code{arb} and \code{Imag(z) <- value}
      never worked for \code{z} of class \code{acf} or \code{acb}.
      Thanks to Martin Maechler for the report and patch (\GH{15}).
    }
  }
}
\section{Changes in version 0.0.5 (2025-05-27)}{
  \subsection{Installation}{
    \itemize{
      \item \file{configure} tests for \file{flint/flint.h} from FLINT
      3.1 more explicitly, and \file{src/noreturn.h} uses the result of
      this test.  Thanks to Brian Ripley for checks linking FLINT 3.2.

      \item \file{configure.ac} calls macro \code{AC_RUN_IFELSE} instead
      of macro \code{AC_COMPILE_IFELSE} where the former was always
      intended.
    }
  }
}
\section{Changes in version 0.0.4 (2025-03-23)}{
  \subsection{Documentation}{
    \itemize{
      \item \file{man/flint-class.Rd} contained a cut-paste error
      affecting section \sQuote{Methods}, items \code{[[} and
      \code{[[<-}.
    }
  }
  \subsection{New Features}{
    \itemize{
      \item \proglang{C} library functions
      \code{arb_hypgeom_gamma_lower}, \code{acb_hypgeom_gamma_lower},
      \code{arb_hypgeom_gamma_upper}, \code{acb_hypgeom_gamma_upper},
      \code{arb_hypgeom_beta_lower}, and \code{acb_hypgeom_beta_lower},
      which implement the incomplete gamma and beta functions, are
      interfaced as \R{} functions with identical names and prototypes.
      Requested by Martin Maechler (\GH{5}).

      \item \R{} functions \code{arb_hypgeom_beta} and
      \code{acb_hypgeom_beta} wrapping \code{arb_hypgeom_gamma} and
      \code{acb_hypgeom_gamma} are provided for completeness.
      \acronym{FLINT} provides no such entry points as of this writing.
    }
  }
  \subsection{Internals}{
    \itemize{
      \item \R{} function \code{complex.runif} facilitating
      complex-valued RNG in examples and tests.
    }
  }
}
\section{Changes in version 0.0.3 (2025-02-23)}{
  \subsection{Installation}{
    \itemize{
      \item Function \code{R_flint_abort} is declared with keyword macro
      \code{FLINT_NORETURN} \emph{conditionally}, for compatibility with
      \acronym{FLINT} 3.0.

      \item \file{configure} tests for function \code{d_mul_2exp} and
      \file{src/mag.c} uses \code{d_mul_2exp} \emph{conditionally}, for
      compatibility with \acronym{FLINT} 3.0.

      \item \file{configure} works around broken \file{flint.pc}
      installed by \acronym{FLINT} 3.0.

      \item \file{configure} uses simpler prerequisite code when
      testing whether system header \file{flint/flint.h} can be
      compiled.

      \item \file{src/noreturn.h} is much refined, for compatibility
      with \acronym{FLINT} 3.0.  The original version triggered
      \command{clang} \option{-Wunknown-attributes}.

      \item \file{src/revertnoreturn.h} is included after the
      \acronym{FLINT} headers so that definitions in
      \file{src/noreturn.h} do not infect headers included later.
    }
  }
}
\section{Changes in version 0.0.2 (2025-02-20)}{
  \subsection{Installation}{
    \itemize{
      \item An array declaration in \file{src/flint.h} specified an
      incorrect (too small) array size, triggering \command{gcc}
      \option{-Wlto-type-mismatch}.  Thanks to Brian Ripley for the
      report.

      \item \file{src/noreturn.h} is included before the \acronym{FLINT}
      headers.  \acronym{FLINT} 3.1 installs a broken
      \file{flint/flint.h} whose use of keyword \code{_Noreturn} and
      keyword macro \code{noreturn} is not portable.  Thanks to Brian
      Ripley for the report.

      \item \file{configure} wrongly assumed that \file{Rinternals.h}
      must be located in \file{`R RHOME`/include}.  Thanks to Dirk
      Eddelbuettel for the report (\GH{3}).

      \item More pointers are zero-initialized to appease
      \command{clang} \option{-Wsometimes-uninitialized}.
    }
  }
}
\section{Changes in version 0.0.1 (2025-02-17)}{
  \subsection{Description}{
    \itemize{
      \item The package is titled \dQuote{Fast Library for Number
        Theory}, matching \acronym{FLINT}.

      \item Mikael Jagan is the maintainer and sole author.

      \item \code{Depends: R (>= 4.3)} mainly for KaTeX support when
      rendering Rd to HTML and \samp{amsmath} support when rendering Rd
      to PDF.

      \item \code{Depends: methods} as \pkg{flint} defines S4 classes,
      generic functions, and methods.

      \item \code{Imports: stats} as \pkg{flint} provides S3 and S4
      methods for \code{quantile}.

      \item \code{SystemRequirements: flint (>= 3), mpfr (>= 3.1), gmp}
      as \pkg{flint} links \acronym{FLINT}, \acronym{GNU}
      \acronym{MPFR}, and \acronym{GNU} \acronym{MP}.  The version
      requirements are purely informational, as \file{configure} and
      \file{configure.win} scripts check that features used by the
      package are available.
    }
  }
  \subsection{Installation}{
    \itemize{
      \item A \file{configure} script is provided for installation under
      Unix.  The script uses \command{pkg-config} where available to
      determine options for linking \acronym{FLINT}, \acronym{GNU}
      \acronym{MPFR}, and \acronym{GNU} \acronym{MP}.  The user can set
      environment variables \env{PKG_CONFIG} and \env{PKG_CONFIG_PATH}
      to control how \command{pkg-config} is invoked and where it
      searches for \file{*.pc} files.  The script gives precedence to
      environment variables \env{R_FLINT_CPPFLAGS},
      \env{R_FLINT_CFLAGS}, \env{R_FLINT_LDFLAGS}, and
      \env{R_FLINT_LIBS}: if any of these is set, then the script does
      not use \command{pkg-config}.  In that case, \env{R_FLINT_LIBS}
      gets a default value \option{-lflint} \option{-lmpfr}
      \option{-lgmp}.  The other \env{R_FLINT_*} are empty by default.

      \item A \file{configure.win} script is provided for installation
      under Windows.  Currently, it invokes \file{configure}, passing
      along positional parameters, and does nothing else.
    }
  }
  \subsection{Documentation}{
    \itemize{
      \item S4 classes, generic functions, and methods are documented
      under help topics with keyword \code{classes} or \code{methods}.
      Interfaces to special mathematical functions are documented under
      help topics with keyword \code{math}.  One can use
      \code{help(package = "flint")} or (for example)
      \code{help.search(package = "flint", keyword = "math")} to get a
      list of (non-internal) help topics.
    }
  }
  \subsection{Classes}{
    \itemize{
      \item Virtual class \code{flint} with nonvirtual subclasses
      \code{ulong}, \code{slong}, \code{fmpz}, \code{fmpq}, \code{mag},
      \code{arf}, \code{acf}, \code{arb}, and \code{acb}.  Objects can
      be understood as vectors of length at most \code{2^32-1} or
      \code{2^64-1} (depending on the ABI) of the so-named \proglang{C}
      types.  The implementation is based on external pointers.

      \item \code{ulong} and \code{slong} represent vectors of fixed
      precision unsigned and signed integers.  The integer size is 32 or
      64 bits (depending on the ABI).

      \item \code{fmpz} represents vectors of arbitrary precision signed
      integers.

      \item \code{fmpq} represents vectors of arbitrary precision
      rational numbers, always in \dQuote{canonical} form with numerator
      and positive, coprime denominator

      \item \code{mag} represents vectors of fixed precision error
      bounds.

      \item \code{arf} and \code{acf} represent vectors of arbitrary
      precision floating-point real and complex numbers.

      \item \code{arb} and \code{acb} represent vectors of arbitrary
      precision floating-point real and complex numbers \emph{with error
        bounds}.  They support \dQuote{ball arithmetic}.
    }
  }
  \subsection{Generic Functions}{
    \itemize{
      \item S4 generic functions \code{Num}, \code{Num<-}, \code{Den},
      and \code{Den<-} for getting and setting numerators and
      denominators of quotients.

      \item S4 generic functions \code{Mid}, \code{Mid<-}, \code{Rad},
      and \code{Rad<-} for getting and setting midpoints and radii of
      balls in a metric space.

      \item S4 generic functions \code{Real}, \code{Real<-},
      \code{Imag}, and \code{Imag<-} for getting and setting real and
      imaginary parts of complex numbers.
    }
  }
  \subsection{Methods}{
    \itemize{
      \item S4 methods are defined for generic (or made-generic)
      functions \code{[}, \code{[<-}, \code{[[}, \code{[[<-},
      \code{Complex}, \code{Den}, \code{Den<-}, \code{Imag},
      \code{Imag<-}, \code{Math}, \code{Math2}, \code{Mid},
      \code{Mid<-}, \code{Num}, \code{Num<-}, \code{Ops}, \code{Rad},
      \code{Rad<-}, \code{Real}, \code{Real<-}, \code{Summary},
      \code{all.equal}, \code{anyDuplicated}, \code{anyNA},
      \code{as.raw}, \code{as.logical}, \code{as.integer},
      \code{as.numeric}, \code{as.complex}, \code{as.vector},
      \code{as.matrix}, \code{as.array}, \code{as.data.frame},
      \code{as.Date}, \code{as.POSIXct}, \code{as.POSIXlt}, \code{c},
      \code{coerce}, \code{cut}, \code{duplicated}, \code{findInterval},
      \code{format}, \code{initialize}, \code{is.finite},
      \code{is.infinite}, \code{is.na}, \code{is.na<-}, \code{is.nan},
      \code{is.unsorted}, \code{length}, \code{length<-}, \code{match},
      \code{mean}, \code{mtfrm}, \code{names}, \code{names<-},
      \code{print}, \code{quantile}, \code{rep}, \code{rep.int},
      \code{rep_len}, \code{show}, \code{seq}, \code{sequence},
      \code{summary}, \code{unique}, and \code{xtfrm}.

      \item S3 methods are defined for S3-turned-S4 generic functions.
      These methods call the S4 generic function so that S4 dispatch
      occurs if the S3 generic function is called.
    }
  }
  \subsection{New Features}{
    \itemize{
      \item Generator functions for each of the nonvirtual subclasses of
      virtual class \code{flint}.

      \item \proglang{C} library functions \code{arb_const_pi},
      \code{arb_const_log2}, \code{arb_const_log10}, \code{arb_const_e},
      \code{arb_lambertw}, \code{acb_lambertw},
      \code{arb_dirichlet_zeta}, \code{acb_dirichlet_zeta},
      \code{arb_dirichlet_hurwitz}, \code{acb_dirichlet_hurwitz},
      \code{acb_dirichlet_lerch_phi},
      \code{arb_hypgeom_gamma}, \code{acb_hypgeom_gamma},
      \code{arb_hypgeom_rgamma}, \code{acb_hypgeom_rgamma},
      \code{arb_hypgeom_lgamma}, \code{acb_hypgeom_lgamma},
      \code{acb_hypgeom_polygamma},
      \code{arb_hypgeom_2f1}, and \code{acb_hypgeom_2f1}
      \code{acb_polygamma}, \code{acb_dirichlet_zeta},
      \code{acb_dirichlet_hurwitz}, \code{acb_dirichlet_lerch_phi},
      \code{acb_hypgeom_gamma}, \code{acb_hypgeom_rgamma},
      \code{acb_hypgeom_lgamma}, and \code{acb_hypgeom_2f1}
      are interfaced as \R{} functions with identical names and
      \dQuote{prototypes}.  Arguments are coerced to the correct class
      and recycled internally.

      \item \R{} function \code{flintABI} returning the size in bits of
      \proglang{C} type \code{mp_limb_t}, either 32 or 64.  The value is
      determined at configure time.

      \item \R{} function \code{flintIdentical} for testing whether
      two \code{flint} vectors have identical class, length, and
      \dQuote{content}.  This test is less strict than the test for
      pointer equality done by \code{identical(x, y)} when \code{x}
      and \code{y} are external pointers.

      \item \R{} function \code{flintLength} for getting the length of a
      \code{flint} vector as a \code{ulong} vector or, optionally, as a
      traditional numeric vector subject to rounding error.

      \item \R{} functions \code{flintPrec} and \code{flintRnd} for
      getting and setting the global default precision and rounding
      mode.

      \item \R{} function \code{flintSize} for calculating the number of
      bytes used by a \code{flint} vector.

      \item \R{} function \code{flintTriple} returning a character
      vector of length 3 of the form \code{c(class, length, address)},
      useful if details about a \code{flint} vector must be printed.

      \item \R{} function \code{flintVersion} returning a list
      containing header and library versions for \acronym{FLINT},
      \acronym{GNU} \acronym{MPFR}, and \acronym{GNU} \acronym{MP}.
      Header versions are determined at compile time.  Library versions
      are determined at run time.

      \item \R{} function \code{c.flint}, the workhorse of the S4 method
      for \code{c}.  It is exported to enable users to bypass the
      internal dispatch mechanism of \code{c}, which dispatches on only
      the first positional argument.
    }
  }
}

%% \section{Changes in version x.y.z (yyyy-mm-dd)}{
%%   \subsection{Description}{
%%     \itemize{
%%       \item .
%%     }
%%   }
%%   \subsection{Compatibility}{
%%     \itemize{
%%       \item .
%%     }
%%   }
%%   \subsection{Installation}{
%%     \itemize{
%%       \item .
%%     }
%%   }
%%   \subsection{Documentation}{
%%     \itemize{
%%       \item .
%%     }
%%   }
%%   \subsection{Significant User-Visible Changes}{
%%     \itemize{
%%       \item .
%%     }
%%   }
%%   \subsection{Deprecated and Defunct}{
%%     \itemize{
%%       \item .
%%     }
%%   }
%%   \subsection{Classes}{
%%     \itemize{
%%       \item .
%%     }
%%   }
%%   \subsection{Generic Functions}{
%%     \itemize{
%%       \item .
%%     }
%%   }
%%   \subsection{Methods}{
%%     \itemize{
%%       \item .
%%     }
%%   }
%%   \subsection{New Features}{
%%     \itemize{
%%       \item .
%%     }
%%   }
%%   \subsection{Bug Fixes}{
%%     \itemize{
%%       \item .
%%     }
%%   }
%%   \subsection{Internals}{
%%     \itemize{
%%       \item .
%%     }
%%   }
%% }
