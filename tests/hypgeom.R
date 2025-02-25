library(flint)

h2f1 <- acb_hypgeom_2f1
hg <- acb_hypgeom_gamma

incl.unit.circle <- FALSE # 'a', 'b', 'c' must meet convergence criteria
debugging <- tolower(Sys.getenv("R_FLINT_CHECK_EXTRA")) == "true"

r <- 10L
n <- r * if (incl.unit.circle) 3L else 2L
eps <- 0x1p-4

set.seed(0xabcdL)
a    <- flint:::complex.runif(n, modulus = c(    0, 1/eps))
b    <- flint:::complex.runif(n, modulus = c(    0, 1/eps))
c    <- flint:::complex.runif(n, modulus = c(    0, 1/eps))
z.l1 <- flint:::complex.runif(r, modulus = c(    0, 1-eps))
z.e1 <- flint:::complex.runif(r, modulus = c(    1,     1))
z.g1 <- flint:::complex.runif(r, modulus = c(1+eps, 1/eps))
z <- .acb(x = c(z.l1, if (incl.unit.circle) z.e1, z.g1))


## http://dlmf.nist.gov/15.4.E1
stopifnot(all.equal(h2f1(1, 1, 2, z),
                    -log(1 - z)/z),
## http://dlmf.nist.gov/15.4.E2
          all.equal(h2f1(0.5, 1, 1.5, z^2),
                    0.5 * (log(1 + z) - log(1 - z))/z),
## http://dlmf.nist.gov/15.4.E3
          all.equal(h2f1(0.5, 1, 1.5, -z^2),
                    atan(z)/z),
## http://dlmf.nist.gov/15.4.E4
          all.equal(h2f1(0.5, 0.5, 1.5, z^2),
                    asin(z)/z),
## http://dlmf.nist.gov/15.4.E5
          all.equal(h2f1(0.5, 0.5, 1.5, -z^2),
                    log(z + sqrt(1 + z^2))/z),
## http://dlmf.nist.gov/15.4.E6
          all.equal(h2f1(a, b, a, z),
                    (1 - z)^-b),
          all.equal(h2f1(a, b, b, z),
                    (1 - z)^-a),
## http://dlmf.nist.gov/15.4.E7
          all.equal(h2f1(a, 0.5 + a, 0.5, z^2),
                    0.5 * ((1 + z)^(-2 * a) + (1 - z)^(-2 * a))),
## http://dlmf.nist.gov/15.4.E8
          {
          i. <- Mod(z) < pi/4
          z. <- z[i.]; a. <- a[i.]
          all.equal(h2f1(a., 0.5 + a., 0.5, -tan(z.)^2),
                    cos(z.)^(2 * a.) * cos(2 * a. * z.))
          },
## http://dlmf.nist.gov/15.4.E9
          all.equal(h2f1(a, 0.5 + a, 1.5, z^2),
                    0.5 * ((1 + z)^(1 - 2 * a) - (1 - z)^(1 - 2 * a))/(1 - 2 * a)/z),
## http://dlmf.nist.gov/15.4.E10
          {
          i. <- Mod(z) < pi/4
          z. <- z[i.]; a. <- a[i.]
          all.equal(h2f1(a., 0.5 + a., 1.5, -tan(z.)^2),
                    cos(z.)^(2 * a.) * sin((1 - 2 * a.) * z.)/(1 - 2 * a.)/sin(z.))
          },
## http://dlmf.nist.gov/15.4.E11
          all.equal(h2f1(-a, a, 0.5, -z^2),
                    0.5 * ((sqrt(1 + z^2) + z)^(2 * a) + (sqrt(1 + z^2) - z)^(2 * a))),
## http://dlmf.nist.gov/15.4.E12
          {
          i. <- Mod(z) < pi/2
          z. <- z[i.]; a. <- a[i.]
          all.equal(h2f1(-a., a., 0.5, sin(z.)^2),
                    cos(2 * a. * z.))
          },
## http://dlmf.nist.gov/15.4.E13
          all.equal(h2f1(a, 1 - a, 0.5, -z^2),
                    0.5 * ((sqrt(1 + z^2) + z)^(2 * a - 1) + (sqrt(1 + z^2) - z)^(2 * a - 1))/sqrt(1 + z^2)),
## http://dlmf.nist.gov/15.4.E14
          {
          i. <- Mod(z) < pi/2
          z. <- z[i.]; a. <- a[i.]
          all.equal(h2f1(a., 1 - a., 0.5, sin(z.)^2),
                    cos((2 * a. - 1) * z.)/cos(z.))
          },
## http://dlmf.nist.gov/15.4.E15
          all.equal(h2f1(a, 1 - a, 1.5, -z^2),
                    0.5 * ((sqrt(1 + z^2) + z)^(1 - 2 * a) - (sqrt(1 + z^2) - z)^(1 - 2 * a))/(1 - 2 * a)/z),
## http://dlmf.nist.gov/15.4.E16
          {
          i. <- Mod(z) < pi/2
          z. <- z[i.]; a. <- a[i.]
          all.equal(h2f1(a., 1 - a., 1.5, sin(z.)^2),
                    sin((2 * a. - 1) * z.)/(2 * a. - 1)/sin(z.))
          },
## http://dlmf.nist.gov/15.4.E17
          {
          i. <- if (debugging) TRUE else -10L
          z. <- z[i.]; a. <- a[i.]
          all.equal(h2f1(a., 0.5 + a., 1 + 2 * a., z.),
                    (0.5 + 0.5 * sqrt(1 - z.))^(-2 * a.))
          },
## http://dlmf.nist.gov/15.4.E18
          {
          i. <- if (debugging) TRUE else -10L
          z. <- z[i.]; a. <- a[i.]
          all.equal(h2f1(a., 0.5 + a., 2 * a., z.),
                    (0.5 + 0.5 * sqrt(1 - z.))^(1 - 2 * a.)/sqrt(1 - z.))
          },
## http://dlmf.nist.gov/15.4.E19
          all.equal(h2f1(a + 1, b, a, z),
                    (1 - (1 - b/a) * z) * (1 - z)^(-1 - b)),
## http://dlmf.nist.gov/15.4.E20
          {
          i. <- Re(c - a - b) > 0
          a. <- a[i.]; b. <- b[i.]; c. <- c[i.]
          all.equal(h2f1(a., b., c., 1),
                    hg(c.) * hg(c. - a. - b.)/hg(c. - a.)/hg(c. - b.))
          })
