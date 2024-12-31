.onLoad <-
function (libname, pkgname) {
### library.dynam("flint", pkgname, libname)
    abi <- flintABI()
    sizeof.long <- .Machine[["sizeof.long"]]
    if (abi != 8L * sizeof.long)
        stop(gettextf("package '%s' was configured for a %d-bit ABI; R was configured for a %d-bit ABI",
                      "flint", abi, 8L * sizeof.long),
             domain = NA)
    bpl <- flintBitsPerLimb()
    if (abi != bpl)
        stop(gettextf("package '%s' was configured for a %d-bit ABI; GNU MP was configured for a %d-bit ABI",
                      "flint", abi, bpl),
             domain = NA)
    return(invisible(NULL))
}

.onUnload <-
function (libpath)
    library.dynam.unload("flint", libpath)
