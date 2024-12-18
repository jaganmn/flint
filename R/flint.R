.subscript.class <-
function (i)
    switch(type. <- typeof(i),
           "NULL" =, "logical" =, "integer" =, "double" =
               if (anyNA(i))
                   stop("subscript NA not supported")
               else type.,
           "S4" =
               if (is.na(flintClass(i)))
                   stop(gettextf("invalid subscript class '%s'",
                                 class(i)),
                        domain = NA)
               else
                   stop(gettextf("subscripts inheriting from virtual class '%s' are not yet supported",
                                 "flint"),
                        domain = NA),
           stop(gettextf("invalid subscript type '%s'",
                         type.),
                domain = NA))

.subassign.class <-
function (value)
    switch(type. <- typeof(value),
           "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
               type.,
           "S4" =
               if (is.na(class. <- flintClass(value)))
                   stop(gettextf("invalid subassignment value class '%s'",
                                 class(value)),
                        domain = NA)
               else class.,
           stop(gettextf("invalid subassignment value type '%s'",
                         type.),
                domain = NA))

.c.class <-
function (x)
    switch(type. <- typeof(x),
           "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =
                                                                                       type.,
           "S4" =
               if (is.na(class. <- flintClass(x)))
                   stop(gettextf("invalid argument class '%s'",
                                 class(x)),
                        domain = NA)
               else class.,
           stop(gettextf("invalid argument type '%s'",
                         type.),
                domain = NA))

setMethod("[",
          c(x = "flint", i = "ANY", j = "missing", drop = "missing"),
          function (x, i, j, ..., drop = TRUE) {
              if (...length())
                  stop("incorrect number of dimensions")
              if (missing(i))
                  return(x)
              nx <- length(x)
              ci <- .subscript.class(i)
              ni <- length(i)
              if (ni == 0L)
              i <- integer(0L)
              else
              i <-
              switch(ci,
                     "logical" =
                         {
                             nw <- length(w <- which(i))
                             if (nw == 0L)
                                 w
                             else if (w[[nw]] > nx)
                                 stop("subscript out of bounds")
                             else {
                                 if (ni < nx) {
                                     w <- w + rep(seq.int(from = 0L, by = ni, length.out = nx %/% ni + (nx %% ni > 0L)), each = nw)
                                     if (w[length(w)] > nx)
                                         w <- w[w <= nx]
                                 }
                                 w
                             }
                         },
                     "integer" =,
                     "double" =,
                         {
                             r <- max(0L, i, na.rm = TRUE)
                             if (r - 1L >= nx)
                                 stop("subscript out of bounds")
                             r <- min(1L, i, na.rm = TRUE)
                             if (r >= 1L)
                                 i
                             else if (r > -1L)
                                 i[i >= 1L]
                             else seq_len(nx)[i]
                         })
              .Call(R_flint_subscript, x, i)
          })

setMethod("[<-",
          c(x = "flint", i = "ANY", j = "missing", value = "ANY"),
          function (x, i, j, ..., value) {
              if (...length())
                  stop("incorrect number of dimensions")
              nx <- length(x)
              if (missing(i)) {
              ni <- nx
              i <- NULL
              } else {
              ci <- .subscript.class(i)
              ni <- length(i)
              if (ni == 0L)
              i <- integer(0L)
              else {
              i <-
              switch(ci,
                     "logical" =
                         {
                             nw <- length(w <- which(i))
                             if (nw == 0L)
                                 w
                             else if (w[[nw]] > nx)
                                 stop("subscript out of bounds")
                             else {
                                 if (ni < nx) {
                                     w <- w + rep(seq.int(from = 0L, by = ni, length.out = nx %/% ni + (nx %% ni > 0L)), each = nw)
                                     if (w[length(w)] > nx)
                                         w <- w[w <= nx]
                                 }
                                 w
                             }
                         },
                     "integer" =,
                     "double" =
                         {
                             r <- max(0L, i, na.rm = TRUE)
                             if (r - 1L >= nx)
                                 stop("subscript out of bounds")
                             r <- min(1L, i, na.rm = TRUE)
                             if (r >= 1L)
                                 i
                             else if (r > -1L)
                                 i[i >= 1L]
                             else seq_len(nx)[i]
                         })
              ni <- length(i)
              }
              }
              if (missing(value))
                  stop("missing subassignment value")
              cv <- .subassign.class(value)
              nv <- length(value)
              if (ni > 0L) {
              if (nv == 0L)
                  stop("replacement has length zero")
              else if (nv > ni || ni %% nv != 0L)
                  warning("number of items to replace is not a multiple of replacement length")
              }
              m <- logical(length(flintLike))
              names(m) <- flintLike
              m[c(flintClass(x), cv)] <- TRUE
              common <- flintClassCommon(m)
              x <- as(x, common)
              value <- as(value, common)
              .Call(R_flint_subassign, x, i, value)
          })

setMethod("[[",
          c(x = "flint", i = "ANY", j = "missing"),
          function (x, i, j, ...) {
              if (...length())
                  stop("incorrect number of dimensions")
              if (missing(i))
                  stop("missing subscript")
              nx <- length(x)
              ci <- .subscript.class(i)
              ni <- length(i)
              if (ni > 0L) {
              i <-
              switch(ci,
                     "logical" =
                         {
                             nw <- length(w <- which(i))
                             if (nw != 1L)
                                 w
                             else if (w > nx)
                                 stop("subscript out of bounds")
                             else if (ni < nx && w <= nx - ni)
                                 stop("attempt to select more than one element")
                             else w
                         },
                     "integer" =,
                     "double" =
                         {
                             r <- max(0L, i, na.rm = TRUE)
                             if (r - 1L >= nx)
                                 stop("subscript out of bounds")
                             r <- min(1L, i, na.rm = TRUE)
                             if (r >= 1L)
                                 i
                             else if (r > -1L)
                                 i[i >= 1L]
                             else seq_len(nx)[i]
                         })
              ni <- length(i)
              }
              if (ni < 1L)
                  stop("attempt to select less than one element")
              else if (ni > 1L)
                  stop("attempt to select more than one element")
              .Call(R_flint_subscript, x, i)
          })

setMethod("[[<-",
          c(x = "flint", i = "ANY", j = "missing", value = "ANY"),
          function (x, i, j, ..., value) {
              if (...length())
                  stop("incorrect number of dimensions")
              if (missing(i))
                  stop("missing subscript")
              nx <- length(x)
              ci <- .subscript.class(i)
              ni <- length(i)
              if (ni > 0L) {
              i <-
              switch(ci,
                     "logical" =
                         {
                             nw <- length(w <- which(i))
                             if (nw != 1L)
                                 w
                             else if (w > nx)
                                 stop("subscript out of bounds")
                             else if (ni < nx && w <= nx - ni)
                                 stop("attempt to select more than one element")
                             else w
                         },
                     "integer" =,
                     "double" =
                         {
                             r <- max(0L, i, na.rm = TRUE)
                             if (r - 1L >= nx)
                                 stop("subscript out of bounds")
                             r <- min(1L, i, na.rm = TRUE)
                             if (r >= 1L)
                                 i
                             else if (r > -1L)
                                 i[i >= 1L]
                             else seq_len(nx)[i]
                         })
              ni <- length(i)
              }
              if (ni < 1L)
                  stop("attempt to select less than one element")
              else if (ni > 1L)
                  stop("attempt to select more than one element")
              if (missing(value))
                  stop("missing subassignment value")
              cv <- .subassign.class(value)
              nv <- length(value)
              if (nv == 0L)
                  stop("replacement has length zero")
              else if (nv > 1L)
                  warning("number of items to replace is not a multiple of replacement length")
              m <- logical(length(flintLike))
              names(m) <- flintLike
              m[c(flintClass(x), cv)] <- TRUE
              common <- flintClassCommon(m)
              x <- as(x, common)
              value <- as(value, common)
              .Call(R_flint_subassign, x, i, value)
          })

setMethod("anyDuplicated",
          c(x = "flint"),
          function (x, incomparables = FALSE, ...)
              anyDuplicated(mtfrm(x), incomparables = incomparables, ...))

setMethod("as.raw",
          c(x = "flint"),
          function (x     ) as.vector(x, "raw"))

setMethod("as.logical",
          c(x = "flint"),
          function (x, ...) as.vector(x, "logical"))

setMethod("as.integer",
          c(x = "flint"),
          function (x, ...) as.vector(x, "integer"))

if (FALSE)
setMethod("as.double",
          c(x = "flint"),
          function (x, ...) as.vector(x, "double"))

setMethod("as.numeric",
          c(x = "flint"),
          function (x, ...) as.vector(x, "numeric"))

setMethod("as.complex",
          c(x = "flint"),
          function (x, ...) as.vector(x, "complex"))

setMethod("as.character",
          c(x = "flint"),
          function (x, ...) format(x, digits = 15L, rnd = "N"))

setMethod("as.list",
          c(x = "flint"),
          function (x, ...) as.vector(x, "list"))

setMethod("as.expression",
          c(x = "flint"),
          function (x, ...) as.vector(x, "expression"))

## MJ: we export this function and curse the author of DispatchAnyOrEval
c.flint <-
function (...) {
    n <- nargs()
    if (n == 0L)
        return(NULL)
    args <- list(...)
    m <- match(flintLike, vapply(args, .c.class, ""), 0L)
    if (!any(m[7L:length(m)]))
        return(c(...))
    names(m) <- flintLike
    args <- lapply(args, as, flintClassCommon(m))
    .Call(R_flint_bind, args)
}

setMethod("c",
          c(x = "flint"),
          function (x, ...)
              c.flint(x, ...))

setAs("ANY", "flint",
      function (from)
          switch(type. <- typeof(from),
                 "raw" = .ulong(x = from),
                 "logical" =, "integer" = .slong(x = from),
                 "double" = .arf(x = from),
                 "complex" = .acf(x = from),
                 "character" =
                 stop(gettextf("coercion from '%s' to '%s' is not yet implemented; consider coercing to a nonvirtual subclass of '%s'",
                               type., "flint", "flint"),
                      domain = NA),
                 stop(gettextf("coercion from '%s' to '%s' is not yet implemented",
                               type., "flint"),
                      domain = NA)))

setMethod("duplicated",
          c(x = "flint"),
          function (x, incomparables = FALSE, ...)
              duplicated(mtfrm(x), incomparables = incomparables, ...))

setMethod("length",
          c(x = "flint"),
          function (x)
              flintLength(x))

setMethod("length<-",
          c(x = "flint"),
          function (x, value)
              .Call(R_flint_realloc, x, as(value, "ulong")))

setMethod("mtfrm",
          c(x = "flint"),
          function (x)
              format(x, base = 62L, digits = 0L))

setMethod("print",
          c(x = "flint"),
          function (x, quote = FALSE, max = NULL, ...) {
              s <- flintTriple(x)
              cat(gettextf("class '%s', length %s, address %s",
                           s[1L], s[2L], s[3L]),
                  "\n", sep = "")
              len <- length(x)
              if (len > 0L) {
                  if (is.null(max))
                      max <- getOption("max.print", 99999L)
                  if (len <= max)
                      print.default(format(x), quote = quote, max = max, ...)
                  else {
                      print.default(format(x[seq_len(max)]), quote = quote, max = max, ...)
                      cat(gettextf(" [ reached '%s' / getOption(\"%s\") -- omitted %f entries ]",
                                   "max", "max.print", len - trunc(max)),
                          "\n", sep = "")
                  }
              }
              invisible(x)
          })

## FIXME: clearly suboptimal for 32-bit 'ulong'
setMethod("rep",
          c(x = "flint"),
          function (x, times, length.out, each, ...) {
              if (!missing(each))
                  x <- .Call(R_flint_rep_each, x, as(each, "ulong"))
              if (!missing(length.out))
                  x <- .Call(R_flint_rep_lengthout, x, as(length.out, "ulong"))
              else if (!missing(times))
                  x <- .Call(R_flint_rep_times, x, as(times, "ulong"))
              x
          })

setMethod("show",
          c(object = "flint"),
          function (object) {
              print(object, quote = FALSE)
              invisible(NULL)
          })

setMethod("unique",
          c(x = "flint"),
          function (x, incomparables = FALSE, ...)
              x[!duplicated(x, incomparables = incomparables, ...)])
