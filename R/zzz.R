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
