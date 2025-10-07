.onLoad <-
function (libname, pkgname) {
### library.dynam("flint", pkgname, libname)
    abi <- flintABI()
    if (flintLongLongLimb()) {
    bpl <- 8L * .Machine[["sizeof.longlong"]]
    if (abi != bpl)
        stop(gettextf("package '%s' was configured for a %d-bit long long int; R was configured for a %d-bit long long int",
                      "flint", abi, bpl),
             domain = NA)
    } else {
    bpl <- 8L * .Machine[["sizeof.long"]]
    if (abi != bpl)
        stop(gettextf("package '%s' was configured for a %d-bit long int; R was configured for a %d-bit long int",
                      "flint", abi, bpl),
             domain = NA)
    }
    bpl <- flintBitsPerLimb()
    if (abi != bpl)
        stop(gettextf("package '%s' was configured for a %d-bit limb; GNU MP was configured for a %d-bit limb",
                      "flint", abi, bpl),
             domain = NA)
    return(invisible(NULL))
}

.onUnload <-
function (libpath)
    library.dynam.unload("flint", libpath)

.initBasic <-
function (where = topenv(parent.frame())) {
	if (is.null(getClassDef("pairlist")))
        setClass("pairlist", where = where)
    if (is.null(selectMethod(coerce, c(from = "ANY", to = "pairlist"),
                             optional = TRUE,
                             useInherited = c(from = TRUE, to = FALSE))))
        setAs("ANY", "pairlist", function (from) as.pairlist(from),
              where = where)
    if (is.null(selectMethod(coerce, c(from = "ANY", to = "raw"),
                             optional = TRUE,
                             useInherited = c(from = TRUE, to = FALSE))))
        setAs("ANY", "raw", function (from) as.raw(from),
              where = where)
    invisible(NULL)
}

.initForeign <-
function (which = c("Rmpfr", "gmp"),
          where = topenv(parent.frame())) {
    which <- match.arg(which, several.ok = TRUE)
    if (any(which == "Rmpfr")) {
        loadNamespace("Rmpfr")
        setAs(`packageSlot<-`("mpfr", "Rmpfr"), "arf",
              function (from) {
                  ans <- .Call(R_flint_coerce_mpfr_arf, from)
                  if (!is.null(a <- dim(from))) {
                      dim(ans) <- a
                      dimnames(ans) <- dimnames(from)
                  }
                  if (!is.null(a <- names(from)))
                      names(ans) <- a
                  ans
              },
              where = where)
        setAs("arf", `packageSlot<-`("mpfr", "Rmpfr"),
              function (from) {
                  ans <- .Call(R_flint_coerce_arf_mpfr, from)
                  if (!is.null(a <- dim(from))) {
                      dim(ans) <- a
                      if (!is.null(a <- dimnames(from)))
                          dimnames(ans) <- a
                  }
                  if (!is.null(a <- names(from)))
                      names(ans) <- a
                  ans
              },
              where = where)
    }
    if (any(which == "gmp")) {
        loadNamespace("gmp")
        setAs(`packageSlot<-`("bigz", "gmp"), "fmpz",
              function (from) {
                  if (!is.null(gmp::modulus(from)))
                      warning("modulus discarded in coercion")
                  ans <- .Call(R_flint_coerce_bigz_fmpz, from)
                  if (!is.null(d <- dim(from)))
                      dim(ans) <- d
                  ans
              },
              where = where)
        setAs("fmpz", `packageSlot<-`("bigz", "gmp"),
              function (from) {
                  ans <- .Call(R_flint_coerce_fmpz_bigz, from)
                  if (!is.null(d <- dim(from))) {
                      if (length(d) != 2L || (d[1L] == 0L && d[2L] != 0L))
                          warning("dimensions discarded in coercion")
                      else {
                          dim(ans) <- d
                          if (!is.null(dimnames(from)))
                              warning("dimension names discarded in coercion")
                      }
                  }
                  if (!is.null(names(from)))
                      warning("names discarded in coercion")
                  ans
              },
              where = where)
        setAs(`packageSlot<-`("bigq", "gmp"), "fmpq",
              function (from) {
                  ans <- .Call(R_flint_coerce_bigq_fmpq, from)
                  if (!is.null(d <- dim(from)))
                      dim(ans) <- d
                  ans
              },
              where = where)
        setAs("fmpq", `packageSlot<-`("bigq", "gmp"),
              function (from) {
                  ans <- .Call(R_flint_coerce_fmpq_bigq, from)
                  if (!is.null(d <- dim(from))) {
                      if (length(d) != 2L || (d[1L] == 0L && d[2L] != 0L))
                          warning("dimensions discarded in coercion")
                      else {
                          dim(ans) <- d
                          if (!is.null(dimnames(from)))
                              warning("dimension names discarded in coercion")
                      }
                  }
                  if (!is.null(names(from)))
                      warning("names discarded in coercion")
                  ans
              },
              where = where)
    }
    invisible(NULL)
}
