setMethod("[",
          c(x = "flint", i = "ANY", j = "missing", drop = "missing"),
          function (x, i, j, ..., drop = TRUE) {
              if (...length())
                  stop("incorrect number of dimensions")
              if (missing(i))
                  return(x)
              switch(typeof(i), `NULL` =, logical =, integer =, double = NULL,
                     stop(gettextf("invalid subscript type \"%s\"", typeof(i)),
                          domain = NA))
              nx <- length(x)
              ni <- length(i)
              if (ni == 0L)
                  i <- integer(0L)
              else {
                  if (anyNA(i))
                      stop("NA subscripts not supported")
                  i <-
                  switch(typeof(i),
                         logical =
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
                         integer =,
                         double =,
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
              }
              .Call(R_flint_subscript, x, i)
          })

setMethod("[<-",
          c(x = "flint", i = "ANY", j = "missing", value = "ANY"),
          function (x, i, j, ..., value) {
              if (...length())
                  stop("incorrect number of dimensions")
              nx <- length(x)
              if (missing(i)) {
              i <- NULL
              ni <- nx
              } else {
              switch(typeof(i), `NULL` =, logical =, integer =, double = NULL,
                     stop(gettextf("invalid subscript type \"%s\"", typeof(i)),
                          domain = NA))
              ni <- length(i)
              if (ni == 0L)
                  i <- integer(0L)
              else {
                  if (anyNA(i))
                      stop("NA subscripts not supported")
                  i <-
                  switch(typeof(i),
                         logical =
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
                         integer =,
                         double =
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
              }
              }
              if (missing(value))
                  stop("missing subassignment value")
              value <- as(value, flintClass(x))
              nv <- length(value)
              if (ni > 0L) {
              if (nv == 0L)
                  stop("replacement has length zero")
              else if (nv > ni || ni %% nv != 0L)
                  warning("number of items to replace is not a multiple of replacement length")
              }
              .Call(R_flint_subassign, x, i, value)
          })

setMethod("[[",
          c(x = "flint", i = "ANY", j = "missing"),
          function (x, i, j, ...) {
              if (...length())
                  stop("incorrect number of dimensions")
              if (missing(i))
                  stop("missing subscript")
              switch(typeof(i), `NULL` =, logical =, integer =, double = NULL,
                     stop(gettextf("invalid subscript type \"%s\"", typeof(i)),
                          domain = NA))
              nx <- length(x)
              ni <- length(i)
              if (ni > 0L) {
                  if (anyNA(i))
                      stop("NA subscripts not supported")
                  i <-
                  switch(typeof(i),
                         logical =
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
                         integer =,
                         double =
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
              switch(typeof(i), `NULL` =, logical =, integer =, double = NULL,
                     stop(gettextf("invalid subscript type \"%s\"", typeof(i)),
                          domain = NA))
              nx <- length(x)
              ni <- length(i)
              if (ni > 0L) {
                  if (anyNA(i))
                      stop("NA subscripts not supported")
                  i <-
                  switch(typeof(i),
                         logical =
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
                         integer =,
                         double =
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
              value <- as(value, flintClass(x))
              nv <- length(value)
              if (nv == 0L)
                  stop("replacement has length zero")
              else if (nv > 1L)
                  warning("number of items to replace is not a multiple of replacement length")
              .Call(R_flint_subassign, x, i, value)
          })

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

setMethod("as.list",
          c(x = "flint"),
          function (x, ...) as.vector(x, "list"))

setMethod("as.expression",
          c(x = "flint"),
          function (x, ...) as.vector(x, "expression"))

setMethod("length",
          c(x = "flint"),
          function (x) flintLength(x))

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
          function(x, times, length.out, each, ...) {
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
