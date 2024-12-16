\name{NEWS}
\title{News for \R{} Package \pkg{flint}}
\newcommand{\GH}{\href{https://github.com/jaganmn/flint/issues/#1}{GH##1}}
\section{Changes in version 0.0.1 (2024-12-15)}{
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

      \item \code{SystemRequirements: gmp, mpfr (>= 3.1), flint (>= 3)}
      as \pkg{flint} links \acronym{GNU} \acronym{MP},
      \acronym{GNU} \acronym{MPFR}, and \acronym{FLINT}.  The version
      requirements are purely informational: on Unix, \file{configure}
      checks that features used by the package are available; on
      Windows, \samp{Rtools} provides sufficiently new versions.
    }
  }
  \subsection{Installation}{
    \itemize{
      \item A \file{configure} script is provided for installation under
      Unix.  The script uses \command{pkg-config} where available to
      determine options for linking \acronym{GNU} \acronym{MP},
      \acronym{GNU} \acronym{MPFR}, and \acronym{FLINT}.  The user can
      set environment variables \env{PKG_CONFIG} and
      \env{PKG_CONFIG_PATH} to control how \command{pkg-config} is
      invoked and where it searches for \file{*.pc} files.  The script
      gives precedence to environment variables \env{R_FLINT_CPPFLAGS},
      \env{R_FLINT_CFLAGS}, \env{R_FLINT_LDFLAGS}, and
      \env{R_FLINT_LIBS}: if any of these is set, then the script does
      not use \command{pkg-config}.  In that case, \env{R_FLINT_LIBS}
      gets a default value \option{-lflint} \option{-lmpfr}
      \option{-lgmp}.  The other \env{R_FLINT_*} are empty by default.

      \item A \file{configure.win} script is provided for installation
      under Windows.
    }
  }
  \subsection{Documentation}{
    \itemize{
      \item Classes and methods for generic functions are documented
      under help topics named \code{*-class}.  Functions that are not
      methods are documented separately.  The package index provides a
      complete list of help topics: see \code{help(package = "flint")}.
    }
  }
  \subsection{Classes}{
    \itemize{
      \item Virtual class \code{flint} with nonvirtual subclasses
      \code{slong}, \code{ulong}, \code{fmpz}, \code{fmpq}, \code{mag},
      \code{arf}, \code{acf}, \code{arb}, and \code{acb}.  Objects can
      be understood as vectors of length at most \code{2^64-1} of the
      so-named \proglang{C} types.  The implementation is based on
      external pointers.

      \item \code{slong} and \code{ulong} represent vectors of fixed
      precision signed and unsigned integers.  The integer size is 32 or
      64 bits, depending on the configuration of the linked
      \acronym{FLINT} library.

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
      \item Generic functions \code{Num}, \code{Num<-}, \code{Den}, and
      \code{Den<-} for getting and setting numerators and denominators
      of quotients.

      \item Generic functions \code{Mid}, \code{Mid<-}, \code{Rad}, and
      \code{Rad<-} for getting and setting midpoints and radii of balls
      in a metric space.

      \item Generic functions \code{Real}, \code{Real<-}, \code{Imag},
      and \code{Imag<-} for getting and setting real and imaginary parts
      of complex numbers.
    }
  }
  \subsection{Methods}{
    \itemize{
      \item Methods are defined for generic functions \code{[},
      \code{[<-}, \code{[[}, \code{[[<-}, \code{Complex}, \code{Den},
      \code{Den<-}, \code{Imag}, \code{Imag<-}, \code{Math},
      \code{Math2}, \code{Mid}, \code{Mid<-}, \code{Num}, \code{Num<-},
      \code{Ops}, \code{Rad}, \code{Rad<-}, \code{Real}, \code{Real<-},
      \code{Summary}, \code{all.equal}, \code{anyNA}, \code{as.raw},
      \code{as.logical}, \code{as.integer}, \code{as.numeric},
      \code{as.complex}, \code{as.character}, \code{as.list},
      \code{as.expression}, \code{as.vector}, \code{coerce},
      \code{format}, \code{initialize}, \code{is.finite},
      \code{is.infinite}, \code{is.na}, \code{is.nan}, \code{length},
      \code{length<-}, \code{mean}, \code{print}, \code{rep}, and
      \code{show}.
    }
  }
  \subsection{New Features}{
    \itemize{
      \item Generator functions for each of the nonvirtual subclasses of
      virtual class \code{flint}.

      \item \proglang{C} library functions \code{acb_lambertw},
      \code{acb_polygamma}, \code{acb_dirichlet_zeta},
      \code{acb_dirichlet_hurwitz}, \code{acb_dirichlet_lerch_phi},
      \code{acb_hypgeom_gamma}, \code{acb_hypgeom_rgamma},
      \code{acb_hypgeom_lgamma}, and \code{acb_hypgeom_2f1}, and their
      \code{arb_*} counterparts, are interfaced as \R{} functions with
      identical names and \dQuote{prototypes}.  Arguments are coerced to
      the correct class and recycled internally.

      \item \R{} function \code{flintBits} returning the limb width as a
      number of bits, either 32 or 64.  The result is determined at
      compile time.

      \item \R{} function \code{flintIdentical} for testing whether
      two \code{flint} vectors have identical class, length, and
      \dQuote{content}.  This test is less strict than the test for
      pointer equality done by \code{identical(x, y)} when \code{x}
      and \code{y} are external pointers.

      \item \R{} functions \code{flintPrec} and \code{flintRnd} for
      getting and setting the global default precision and rounding
      modes.

      \item \R{} function \code{flintVersion} returning a list
      containing header and library versions for \acronym{GNU}
      \acronym{MP}, \acronym{GNU} \acronym{MPFR}, and \acronym{FLINT}.
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
