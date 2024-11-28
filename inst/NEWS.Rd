%% \section{Changes in version x.y.z (yyyy-mm-dd)}{
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
%%   \subsection{Bug Fixes}{
%%     \itemize{
%%       \item .
%%     }
%%   }
%%   \subsection{New Features}{
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
%%   \subsection{Internals}{
%%     \itemize{
%%       \item .
%%     }
%%   }
%% }

\name{NEWS}
\title{News for \R{} Package \pkg{flint}}
\newcommand{\GH}{\href{https://github.com/jaganmn/flint/issues/#1}{GH##1}}
\section{Changes in version 0.0.1 (2024-11-28)}{
  \subsection{New Features}{
    \itemize{
      \item Virtual class \code{flint} with nonvirtual subclasses
      \code{slong}, \code{ulong}, \code{fmpz}, \code{fmpq}, \code{arf},
      \code{mag}, \code{arb}, and \code{acb}.  Objects can be understood
      as vectors of length at most \code{2^64-1} of the so-named
      \proglang{C} types.  The implementation is based on external
      pointers.

      \item Methods are defined for \code{[}, \code{[<-}, \code{[[},
      \code{[[<-}, \code{Complex}, \code{Math}, \code{Math2},
      \code{Ops}, \code{Summary}, \code{as.raw}, \code{as.logical},
      \code{as.integer}, \code{as.numeric}, \code{as.complex},
      \code{as.list}, \code{as.expression}, \code{as.vector},
      \code{coerce}, \code{format}, \code{initialize}, \code{length},
      \code{print}, and \code{show}.

      \item Virtual class \code{nflint} with nonvirtual subclasses
      \code{nslong}, \code{nulong}, \code{nfmpz}, \code{nfmpq},
      \code{narf}, \code{nmag}, \code{narb}, and \code{nacb}.  These
      are \dQuote{native} analogues of the unprefixed classes based on
      integer and double vectors instead of external pointers.  They are
      mainly didactic as they poorly approximate the (often arbitrary)
      range of the corresponding \proglang{C} types.  That methods are
      provided only for \code{coerce} and \code{length} is intended.

      \item \proglang{C} library functions \code{acb_lambertw},
      \code{acb_polygamma}, \code{acb_dirichlet_zeta},
      \code{acb_dirichlet_hurwitz}, \code{acb_dirichlet_lerch_phi},
      \code{acb_hypgeom_gamma}, \code{acb_hypgeom_rgamma},
      \code{acb_hypgeom_lgamma}, and \code{acb_hypgeom_2f1} are
      interfaced as \R{} functions with identical names and signatures.
      Arguments are coerced to the correct class and recycled
      internally.

      \item \R{} function \code{flintBits} returning the limb width as
      a number of bits, either 32 or 64.  Currently, the result is
      determined at compile time.

      \item \R{} function \code{flintVersion} returning a list
      containing header and library versions for \acronym{GNU}
      \acronym{MP}, \acronym{GNU} \acronym{MPFR}, and \acronym{FLINT}.
      Header versions are determined at compile time.  Library versions
      are determined at run time.
    }
  }
  \subsection{Installation}{
    \itemize{
      \item Provide a \file{configure} script for installation under
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

      \item Provide a minimal \file{configure.win} script for
      installation under Windows.
    }
  }
  \subsection{Documentation}{
    \itemize{
      \item Classes, methods for imported generic functions, and methods
      for generic functions in \pkg{base} are documented under topics
      \code{*-class}.  Functions that are not methods are documented
      separately.
    }
  }
}
