\name{NEWS}
\title{News for \R{} Package \pkg{flint}}
\newcommand{\GH}{\href{https://github.com/jaganmn/flint/issues/#1}{GH##1}}
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
      \item S4 methods are defined for generic functions \code{[},
      \code{[<-}, \code{[[}, \code{[[<-}, \code{Complex}, \code{Den},
      \code{Den<-}, \code{Imag}, \code{Imag<-}, \code{Math},
      \code{Math2}, \code{Mid}, \code{Mid<-}, \code{Num}, \code{Num<-},
      \code{Ops}, \code{Rad}, \code{Rad<-}, \code{Real}, \code{Real<-},
      \code{Summary}, \code{all.equal}, \code{anyDuplicated},
      \code{anyNA}, \code{as.raw}, \code{as.logical}, \code{as.integer},
      \code{as.numeric}, \code{as.complex}, \code{as.vector},
      \code{as.matrix}, \code{as.array}, \code{as.data.frame},
      \code{as.Date}, \code{as.POSIXct}, \code{as.POSIXlt},
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
      modes.

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
