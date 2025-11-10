\name{arf_rk}
\alias{arf_rk}
\title{Numerical Solution of Systems of Ordinary Differential Equations}
\description{
  Solves numerically the initial value problem
  \deqn{y'(t) = f(t, y(t)), y(0) = y_{0}}{y'(t) = f(t, y(t)), y(0) = y_0}
  using an explicit, adaptive or non-adaptive Runge-Kutta method,
  by default the Dormand-Prince method.

  The main difference between this function and function \code{rk} in
  package \CRANpkg{deSolve} is the use of \code{\linkS4class{arf}}
  instead of \code{double}, enabling computation of times, solution
  values, and solution derivatives in arbitrary precision.  A corollary
  is that users can choose arbitrarily small \code{rtol} and \code{atol}
  provided that the working precision is sufficiently high.

  Interpolation is not yet implemented.
}
\usage{
arf_rk(func, t, y0, param = NULL, rtol = NULL, atol = NULL,
       hmin = 0, hmax = Inf, hini = NULL, smax = NULL,
       method = .rk.method.dormand.prince(), prec = flintPrec())
}
\arguments{
  \item{func}{
    a function of the form \code{function (t, y, param, prec)} computing
    derivatives.  Unused trailing arguments can omitted.}
  \item{t}{
    an increasing numeric or \code{\linkS4class{arf}} vector storing
    time points.}
  \item{y0}{
    a numeric or \code{\linkS4class{arf}} vector storing the initial
    value.}
  \item{param}{
    an \R{} object typically specifying parameters of the system,
    passed to \code{func}.}
  \item{rtol}{
    a positive number less than 1 controlling external step adaptation
    in adaptive methods.  \code{2^-prec} by default, unused by
    non-adaptive methods.}
  \item{atol}{
    a non-negative number less than 1 controlling external step
    adaptation in in adaptive methods.  \code{2^-prec} by default,
    unused by non-adaptive methods.}
  \item{hmin}{
    a non-negative number indicating a minimum external step size in
    adaptive methods.  Early termination results if it seems that a
    smaller step size is needed to achieve sufficiently small error.
    The default value is \code{0}, indicating that the step size can
    become arbitrarily small.  Unused by non-adaptive methods.}
  \item{hmax}{
    a positive number indicating a maximum external step size in
    adaptive methods.  The default value is \code{Inf}, indicating that
    the step size is bounded above by \code{diff(t)} and nothing else.
    Unused by non-adaptive methods.}
  \item{hini}{
    a positive number indicating the initial external step size in
    adaptive methods and the fixed external step size in non-adaptive
    methods.  \code{min(diff(t))} by default.}
  \item{smax}{
    a non-negative integer indicating a maximum number of internal steps
    per external step.  Early termination results after
    \code{smax * (length(t) - 1)} internal steps.  \code{256 * prec} by
    default.}
  \item{method}{
    a list specifying a Runge-Kutta method.}
  \item{prec}{
    a positive integer indicating the working precision as a number of
    bits, passed to \code{func}.}
}
\value{
  A list with components:
  \item{t}{
    an increasing \code{\linkS4class{arf}} vector storing time points.}
  \item{y}{
    an \code{\linkS4class{arf}} matrix with \code{length(t)} rows and
    \code{length(y0)} columns storing the numerical solution.  In the
    event of early termination, trailing rows are filled with
    \code{NaN}.}
}
\details{
  TODO.
}
\seealso{
  Class \code{\linkS4class{arf}}; function \code{rk} in
  \CRANpkg{deSolve}.
}
\examples{
## TODO
}
\keyword{math}
