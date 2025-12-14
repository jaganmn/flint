R package **flint** is an R interface to FLINT (https://flintlib.org/),
a C library for number theory.  FLINT implements arithmetic in rings,
including midpoint-radius interval arithmetic, also known as ball
arithmetic, in the real and complex numbers, enabling computation in
arbitrary precision with rigorous propagation of rounding and other
errors.  Notably, FLINT provides ball arithmetic implementations of many
special mathematical functions not previously supported by R or any R
package available on CRAN (or, where supported, not defined to arbitrary
precision, over the complex numbers, or beyond the radius of convergence
of a power series representation).

**flint** uses S4 classes based on external pointers to represent
vectors (including matrices and more general arrays) of numbers of a
given C type.  There is a virtual class `flint` with nonvirtual
subclasses named after a corresponding C type:

  Class | Description
   ---: | :---
`ulong` | fixed precision (32-bit or 64-bit) unsigned integers.
`slong` | fixed precision (32-bit or 64-bit) signed integers.
 `fmpz` | arbitrary precision signed integers.
 `fmpq` | rational numbers with `fmpz` numerator and denominator.
  `mag` | unsigned floating-point real numbers with fixed precision (30-bit) significand and `fmpz` exponent.
  `arf` | floating-point real numbers with arbitrary precision significand and `fmpz` exponent.
  `acf` | floating-point complex numbers with `arf` real and imaginary parts.
  `arb` | real balls with `arf` midpoint and `mag` radius, of the form `(a +/- r)`.
  `acb` | complex balls with `arb` real and imaginary parts, of the form `(a +/- r)+(b +/- s)i`.

S4 and S3 methods are defined with the aim of complete interoperability
of `flint` vectors and traditional (atomic or recursive) vectors.  To 
this end, methods employ rigorously defined and consistent rules for 
argument promotion, extending R's basic hierarchy.

```
> library(flint)
> ns <- getNamespace("flint")
> sort(getClasses(ns))
 [1] "OptionalCharacter" "OptionalInteger"   "OptionalList"     
 [4] "acb"               "acf"               "arb"              
 [7] "arf"               "flint"             "fmpq"             
[10] "fmpz"              "mag"               "slong"            
[13] "ulong"            
> getDataPart(getGenerics(ns)) # of length 114!
  [1] "!"             "$"             "$<-"           "%*%"          
  [5] "+"             "-"             "Complex"       "Den"          
  [9] "Den<-"         "Imag"          "Imag<-"        "Math2"        
 [13] "Math"          "Mid"           "Mid<-"         "Num"          
 [17] "Num<-"         "Ops"           "Rad"           "Rad<-"        
 [21] "Real"          "Real<-"        "Summary"       "["            
 [25] "[<-"           "[["            "[[<-"          "all.equal"    
 [29] "anyDuplicated" "anyNA"         "aperm"         "as.Date"      
 [33] "as.POSIXct"    "as.POSIXlt"    "as.array"      "as.complex"   
 [37] "as.data.frame" "as.integer"    "as.logical"    "as.matrix"    
 [41] "as.numeric"    "as.raw"        "as.vector"     "asplit"       
 [45] "backsolve"     "c"             "cbind2"        "chol2inv"     
 [49] "chol"          "coerce"        "colMeans"      "colSums"      
 [53] "crossprod"     "cut"           "det"           "determinant"  
 [57] "diag"          "diag<-"        "diff"          "diffinv"      
 [61] "dim"           "dim<-"         "dimnames"      "dimnames<-"   
 [65] "drop"          "duplicated"    "eigen"         "findInterval" 
 [69] "format"        "identical"     "is.array"      "is.finite"    
 [73] "is.infinite"   "is.matrix"     "is.na"         "is.na<-"      
 [77] "is.nan"        "is.unsorted"   "isComplex"     "isFloating"   
 [81] "isSigned"      "isSymmetric"   "kronecker"     "length"       
 [85] "length<-"      "log"           "match"         "mean"         
 [89] "mtfrm"         "names"         "names<-"       "norm"         
 [93] "outer"         "print"         "qr"            "quantile"     
 [97] "rbind2"        "rcond"         "rep.int"       "rep"          
[101] "rep_len"       "rowMeans"      "rowSums"       "scale"        
[105] "seq"           "sequence"      "show"          "solve"        
[109] "summary"       "svd"           "t"             "tcrossprod"   
[113] "unique"        "xtfrm"        
> oprec <- flintPrec(0x1p+12L) # 2^12 = 4096 bits
> (x <- 0:3 * arb_const_pi() / 2)
class "arb", length 4, address 0x600002d52280
[1] (0.000000e+0 +/- 0.000e+0000) (1.570796e+0 +/- 9.575e-1234)
[3] (3.141593e+0 +/- 1.915e-1233) (4.712389e+0 +/- 1.054e-1232)
> (y <- exp(1i * x))
class "acb", length 4, address 0x137e39e20
[1] ( 1.000000e+0000 +/- 0.000e+0000)+( 0.000000e+0000 +/- 0.000e+0000)i
[2] (-4.080025e-1234 +/- 9.575e-1234)+( 1.000000e+0000 +/- 9.575e-1234)i
[3] (-1.000000e+0000 +/- 1.915e-1233)+(-8.160051e-1234 +/- 1.915e-1233)i
[4] (-6.909879e-1234 +/- 1.054e-1232)+(-1.000000e+0000 +/- 1.054e-1232)i
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
installed (if not already) automatically when you install FLINT.  On
older systems, repositories may provide too old (< 3.0.0) versions of
FLINT.  In that case, one can download a recent (>= 3.0.0) source
tarball from [here](https://github.com/flintlib/flint/releases) and
build FLINT locally from the sources.

If you use macOS and obtained R from a CRAN binary, then follow the
instructions [here](https://mac.r-project.org/bin/) to install
architecture-specific binaries for packages `flint`, `mpfr`, and `gmp`.

If you use Windows and obtained R from a CRAN binary, then install
[Rtools](https://cran.r-project.org/bin/windows/Rtools/).  The headers
and libraries that you need are available as of Rtools44 (r6414).

## Documentation

```
> news(package = "flint") # the change log
> help(package = "flint") # the index
> help.search(package = "flint", keyword = "classes") # S4 classes
> help.search(package = "flint", keyword = "math") # special mathematical functions
```

## Bug reports and feature requests

Please use the issue tracker to report bugs and request features:
https://github.com/jaganmn/flint/issues/.

The focus of versions 0.x.y has been the implementation of S4 classes,
generic functions, and methods, rather than extensive coverage of C
entry points provided by FLINT.  Nevertheless, as a starting point,
versions 0.x.y provide an R interface to certain C entry points so that
users can already compute several analytically continued special
mathematical functions (as of this writing, these include Bessel and
modified Bessel functions, zeta functions, complete and incomplete gamma
and beta functions, hypergeometric functions, and Lambert's *W*).

If there are entry points to which you need an R interface, then just
let me know, ideally including in your request links to the online
documentation: https://flintlib.org/doc/.
