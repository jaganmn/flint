setMatrixOpsMethod <-
function (signature, definition, where = topenv(parent.frame())) {
    formals(definition) <- formals(function (x, y) NULL)
    setMethod(       "%*%", signature, definition, where)
    formals(definition) <- formals(function (x, y = NULL, ...) NULL)
    setMethod( "crossprod", signature, definition, where)
    setMethod("tcrossprod", signature, definition, where)
    invisible("matrixOps")
}
