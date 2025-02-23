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
`ulong` | fixed precision (32-bit or 64-bit) unsigned integers.
`slong` | fixed precision (32-bit or 64-bit) signed integers.
 `fmpz` | arbitrary precision signed integers.
 `fmpq` | rational numbers with `fmpz` numerator and denominator.
  `mag` | unsigned floating-point real numbers with fixed precision (30-bit) significand and `fmpz` exponent.
  `arf` | floating-point real numbers with arbitrary precision significand and `fmpz` exponent.
  `acf` | floating-point complex numbers with `arf` real and imaginary parts.
  `arb` | real balls with `arf` midpoint and `mag` radius, of the form `(a +/- b)`.
  `acb` | complex balls with `arb` real and imaginary parts, of the form `(a +/- b)+(c +/- d)i`.

S4 and S3 methods are defined with the aim of complete interoperability
of `flint` vectors and traditional (atomic or recursive) vectors.  To 
this end, methods employ rigorously defined and consistent rules for 
argument promotion, extending R's basic hierarchy.

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
[31] "as.array"      "as.complex"    "as.data.frame"
[34] "as.integer"    "as.logical"    "as.matrix"    
[37] "as.numeric"    "as.raw"        "as.vector"    
[40] "c"             "coerce"        "cut"          
[43] "duplicated"    "findInterval"  "format"       
[46] "initialize"    "is.finite"     "is.infinite"  
[49] "is.na"         "is.na<-"       "is.nan"       
[52] "is.unsorted"   "length"        "length<-"     
[55] "log"           "match"         "mean"         
[58] "mtfrm"         "names"         "names<-"      
[61] "print"         "quantile"      "rep.int"      
[64] "rep"           "rep_len"       "seq"          
[67] "sequence"      "show"          "summary"      
[70] "unique"        "xtfrm"        
> oprec <- flintPrec(0x1p+12L) # 2^12 = 4096 bits
> (x <- 0:3 * arb_const_pi() / 2)
class 'arb', length 4, address 0x6000001c7c00
[1] (0.000000e+0 +/- 0.000000e+0000)
[2] (1.570796e+0 +/- 9.574978e-1234)
[3] (3.141593e+0 +/- 1.914996e-1233)
[4] (4.712389e+0 +/- 1.053248e-1232)
> (y <- exp(1i * x))
class 'acb', length 4, address 0x1177085a0
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

R package **flint** is
[published by CRAN](https://cran.r-project.org/package=flint).
To install the latest release version, use `install.packages`.

```
> install.packages("flint")
```

To install the latest development version, clone the GitHub repository,
change to the top level directory, build a source tarball, and install.

```
$ git clone https://github.com/jaganmn/flint.git
$ cd flint
$ VERSION=`sed -e '/^Version: /!d' -e 's/Version: //' DESCRIPTION`
$ R CMD build .
$ R CMD INSTALL flint_${VERSION}.tar.gz
```

Since **flint** links C libraries FLINT, GNU MPFR, and GNU MP, headers
and libraries must be found on the preprocessor and linker search paths
when you install **flint** from sources.

If you obtained R from your operating system's package manager, then use
the same package manager to install FLINT:

```
$ pacman -S flint
$ apt install libflint-dev
$ dnf install flint-devel
$ pkg install flint
$ port install flint
$ brew install flint
```

GNU MPFR and GNU MP are dependencies of FLINT, so they should be
installed (if not already) automatically when you install FLINT.
Repositories for older systems may provide too old (< 3.0.0) versions of
FLINT.  On such systems, one can download a recent source tarball from
[here](https://github.com/flintlib/flint/releases) and build FLINT from
the sources.

If you use macOS and you obtained R from a CRAN binary, then follow the
instructions [here](https://mac.r-project.org/bin/) to install
architecture-specific binaries for packages `flint`, `mpfr`, and `gmp`.

If you use Windows and you obtained R from a CRAN binary, then install
[Rtools](https://cran.r-project.org/bin/windows/Rtools/).  The headers
and libraries that you need are available as of Rtools44 (r6346).

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
