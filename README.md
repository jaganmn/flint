# R package **flint**

R package **flint** is an R interface to FLINT (https://flintlib.org/),
a C library for number theory.  FLINT implements arithmetic in rings,
including midpoint-radius interval arithmetic, also known as ball
arithmetic, in the real and complex numbers, enabling computation in
arbitrary precision with rigorous propagation of errors.  Notably, FLINT
provides ball arithmetic implementations of many special mathematical
functions not previously supported by R or any R package available on
CRAN (or, where supported, not defined over the complex numbers, to
arbitrary precision, or beyond the radius of convergence of a power
series representation).

**flint** uses S4 classes based on external pointers to represent
vectors of numbers of a given C type.  There is a virtual class `flint`
with nonvirtual subclasses named after a corresponding C type:

  Class | Description
   ---: | :---
`slong` | fixed precision (32-bit or 64-bit) signed integers.
`ulong` | fixed precision (32-bit or 64-bit) unsigned integers.
 `fmpz` | arbitrary precision signed integers.
 `fmpq` | rational numbers with `fmpz` numerator and denominator.
  `mag` | unsigned floating-point real numbers with fixed precision (30-bit) significand and `fmpz` exponent.
  `arf` | floating-point real numbers with arbitrary precision significand and `fmpz` exponent.
  `acf` | floating-point complex numbers with `arf` real and imaginary parts.
  `arb` | real balls with `arf` midpoint and `mag` radius, of the form `(a +/- b)`.
  `acb` | complex balls with `arb` real and imaginary parts, of the form `(a +/- b)+(c +/- d)i`.

S4 methods are defined for most generic functions in **base**, partly
by way of group generic functions in **methods**, so that, typically,
one can handle `flint` vectors just as one would handle traditional
numeric and complex vectors.  By design, `flint` vectors and atomic
vectors are interoperable, and methods promote arguments as needed.

```
> library(flint)
> p <- as.environment("package:flint")
> getClasses(p)
 [1] "acb"   "acf"   "slong" "mag"   "ulong" "arb"   "arf"
 [8] "flint" "fmpq"  "fmpz"
> getDataPart(getGenerics(p))
 [1] "!"             "+"             "-"
 [4] "Complex"       "Den"           "Den<-"
 [7] "Imag"          "Imag<-"        "Math2"
[10] "Math"          "Mid"           "Mid<-"
[13] "Num"           "Num<-"         "Ops"
[16] "Rad"           "Rad<-"         "Real"
[19] "Real<-"        "Summary"       "["
[22] "[<-"           "[["            "[[<-"
[25] "all.equal"     "anyDuplicated" "anyNA"
[28] "as.Date"       "as.POSIXct"    "as.POSIXlt"
[31] "as.complex"    "as.data.frame" "as.integer"
[34] "as.logical"    "as.numeric"    "as.raw"
[37] "as.vector"     "c"             "coerce"
[40] "cut"           "duplicated"    "findInterval"
[43] "format"        "initialize"    "is.finite"
[46] "is.infinite"   "is.na"         "is.na<-"
[49] "is.nan"        "is.unsorted"   "length"
[52] "length<-"      "log"           "mean"
[55] "mtfrm"         "names"         "names<-"
[58] "print"         "quantile"      "rep.int"
[61] "rep"           "rep_len"       "seq"
[64] "sequence"      "show"          "summary"
[67] "unique"        "xtfrm"
> oprec <- flintPrec(0x1p+12L) # 2^12 = 4096 bits
> (x <- 0:3 * acos(.arb(x = -1)) / 2) # acos(-1) = pi
class 'arb', length 4, address 0x600002715ec0
[1] (0.000000e+0 +/- 0.000000e+0000)
[2] (1.570796e+0 +/- 9.574978e-1234)
[3] (3.141593e+0 +/- 1.914996e-1233)
[4] (4.712389e+0 +/- 1.053248e-1232)
> (y <- exp(1i * x))
class 'acb', length 4, address 0x11eff9f80
[1] ( 1.000000e+0000 +/- 0.000000e+0000)+( 0.000000e+0000 +/- 0.000000e+0000)i
[2] (-4.080025e-1234 +/- 9.574978e-1234)+( 1.000000e+0000 +/- 9.574978e-1234)i
[3] (-1.000000e+0000 +/- 1.914996e-1233)+(-8.160051e-1234 +/- 1.914996e-1233)i
[4] (-6.909879e-1234 +/- 1.053248e-1232)+(-1.000000e+0000 +/- 1.053248e-1232)i
> all.equal(y, c(1, 1i, -1, -1i), tolerance = 0x1p-1000, check.class = FALSE)
[1] TRUE
> flintPrec(oprec) # restoring the original precision
[1] 4096
>
```

## Installation

R package **flint** links C libraries FLINT, GNU MPFR, and GNU MP.
Headers and libraries must be found on the preprocessor and linker
search paths when installing **flint** from sources.

If you obtained R from your operating system's package manager, then
use the same package manager to install FLINT:

```
$ sudo pacman -S flint
$ sudo apt install libflint-dev
$ sudo dnf install flint-devel
$ sudo yum install flint-devel
$ sudo port install flint
$ brew install flint
```

GNU MPFR and GNU MP are dependencies of FLINT, so they should be
installed (if not already) automatically when you install FLINT.

If you use macOS and you obtained R from a CRAN binary, then follow the
instructions [here](https://mac.r-project.org/bin/) to install
architecture-specific binaries for packages `flint`, `mpfr`, and `gmp`.

If you use Windows and you obtained R from a CRAN binary, then install
[Rtools](https://cran.r-project.org/bin/windows/Rtools/).  The headers
and libraries that you need are available as of Rtools44 (r6346).

Once the system dependencies are satisfied, change to the top level
directory of the R package sources and do:

```
$ R CMD build .
$ R CMD INSTALL flint_0.0.1.tar.gz
```

Once the R package is published by CRAN (in early March, I expect),
you can also do:

```
> install.packages("flint") # maybe passing type = "source"
```

## Documentation

```
> news(package = "flint") # the change log
> help(package = "flint") # the index
> help.search(package = "flint", keyword = "classes") # S4 classes
> help.search(package = "flint", keyword = "math") # special mathematical functions
```

## Bug reports and feature requests

Please use the issue tracker to report bugs and request features:
https://github.com/jaganmn/flint.

The focus of version 0.0.1 has been the implementation of S4 classes,
generic functions, and methods, rather than extensive coverage of entry
points in the FLINT C library.  Nevertheless, as a starting point,
version 0.0.1 provides an R interface to certain entry points so that
users can already compute (analytically continued) zeta, gamma, and
hypergeometric functions as well as Lambert's W.

If there are entry points to which you need an R interface, then just
let me know, ideally including in your request links to the online
documentation: https://flintlib.org/doc/.
