.subscript.class <-
function (object, mode)
    switch(mode,
           ## x
           switch(type. <- typeof(object),
                  "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =, "character" =, "pairlist" =, "list" =, "expression" =
                                                                                              type.,
                  "S4" =
                      if (is.na(class. <- flintClass(object)))
                          stop(.error.notSubsettable(object))
                      else class.,
                  stop(.error.notSubsettable(object))),
           ## i
           switch(type. <- typeof(object),
                  "NULL" =, "logical" =, "integer" =, "double" =, "character" =
                      if (anyNA(object))
                          stop("NA subscripts not supported")
                      else type.,
                  "S4" =
                      switch(class. <- flintClass(object),
                             "ulong" =, "slong" =, "fmpz" =, "fmpq" =
                                 class.,
                             stop(.error.invalidSubscriptClass(object))),
                  stop(.error.invalidSubscriptType(object))),
           ## value
           switch(type. <- typeof(object),
                  "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =, "character" =, "list" =, "expression" =
                      type.,
                  "S4" =
                      if (is.na(class. <- flintClass(object)))
                          stop(.error.invalidSubassignValueClass(object))
                      else class.,
                  stop(.error.invalidSubassignValueType(object))),
           stop("should never happen ..."))

setMethod("[",
          c(x = "ANY", i = "flint", j = "missing", drop = "missing"),
          function (x, i, j, ..., drop = TRUE) {
              cx <- .subscript.class(x, 1L)
              if (...length())
                  stop(.error.invalidArity())
              ci <- .subscript.class(i, 2L)
              ni <- flintLength(i)
              nx <- length(x)
              if (ni == 0L)
              i <- integer(0L)
              else {
              a <- min(i)
              b <- max(i)
              i <-
              if (a > -1L) {
                  if (b - 1L >= nx)
                      stop(.error.subscriptOutOfBounds(x, 0L, i))
                  else if (a >= 1L)
                      i
                  else i[i >= 1L]
              } else {
                  if (b >= 1L)
                      stop(.error.subscriptNegativePositive())
                  else if (a + 1L <= -nx)
                      stop(.error.subscriptOutOfBounds(x, 0L, i))
                  else .Call(R_flint_ulong_complement, .ulong(x = -i), nx, TRUE)
              }
              i <-
              if (is.double(nx))
                  as.double(i)
              else as.integer(i)
              }
              x[i]
          })

setMethod("[",
          c(x = "flint", i = "ANY", j = "missing", drop = "missing"),
          function (x, i, j, ..., drop = TRUE) {
              if (...length())
                  stop(.error.invalidArity())
              if (missing(i))
                  return(x)
              ci <- .subscript.class(i, 2L)
              ni <- length(i)
              nx <- flintLength(x)
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
                                 stop(.error.subscriptOutOfBounds(x, 0L, i))
                             else {
                                 ni <- .ulong(x = ni)
                                 if (ni < nx) {
                                     q <- nx %/% ni
                                     r <- nx %% ni
                                     w <- w. <- .ulong(x = w)
                                     if (q >= 1L)
                                         w <- w + rep(seq(from = .ulong(x = 0L), by = ni, length.out = q), each = flintLength(w))
                                     if (r >= w[1L])
                                         w <- c(w, flintLength(w) + w.[w. <= r])
                                 }
                                 w
                             }
                         },
                     "integer" =,
                     "double" =
                         {
                             a <- min(i)
                             b <- max(i)
                             if (a + 1L <= -nx || b - 1L >= nx)
                                 stop(.error.subscriptOutOfBounds(x, 0L, i))
                             else if (a >= 1L)
                                 i
                             else if (a > -1L)
                                 i[i >= 1L]
                             else if (b >= 1L)
                                 stop(.error.subscriptNegativePositive())
                             else if (nx <= .Machine[["integer.max"]])
                                 seq_len(as.integer(nx))[as.integer(i)]
                             else .Call(R_flint_ulong_complement, .ulong(x = -i), nx, TRUE)
                         },
                     "character" =
                         {
                             if (is.null(nms <- names(x)) ||
                                 anyNA(m <- match(i, nms)))
                                 stop(.error.subscriptOutOfBounds(x, 0L, i))
                             m
                         })
              .Call(R_flint_subscript, x, i, TRUE)
          })

setMethod("[",
          c(x = "flint", i = "flint", j = "missing", drop = "missing"),
          function (x, i, j, ..., drop = TRUE) {
              if (...length())
                  stop(.error.invalidArity())
              ci <- .subscript.class(i, 2L)
              ni <- flintLength(i)
              nx <- flintLength(x)
              if (ni == 0L)
              i <- integer(0L)
              else {
              a <- min(i)
              b <- max(i)
              i <-
              if (a > -1L) {
                  if (b - 1L >= nx)
                      stop(.error.subscriptOutOfBounds(x, 0L, i))
                  else if (a >= 1L)
                      i
                  else i[i >= 1L]
              } else {
                  if (b >= 1L)
                      stop(.error.subscriptNegativePositive())
                  else if (a + 1L <= -nx)
                      stop(.error.subscriptOutOfBounds(x, 0L, i))
                  else .Call(R_flint_ulong_complement, .ulong(x = -i), nx, TRUE)
              }
              }
              .Call(R_flint_subscript, x, i, TRUE)
          })

setMethod("[<-",
          c(x = "ANY", i = "ANY", j = "missing", value = "flint"),
          function (x, i, j, ..., value) {
              cx <- .subscript.class(x, 1L)
              if (...length())
                  stop(.error.invalidArity())
              cv <- flintClass(value)
              common <- flintClassCommon(c(cx, cv))
              x <- as(x, common)
              value <- as(value, common)
              if (missing(i))
              x[ ] <- value
              else
              x[i] <- value
              x
          })

setMethod("[<-",
          c(x = "ANY", i = "flint", j = "missing", value = "ANY"),
          function (x, i, j, ..., value) {
              cx <- .subscript.class(x, 1L)
              if (...length())
                  stop(.error.invalidArity())
              if (missing(value))
                  stop(.error.missingSubassignValue())
              ci <- .subscript.class(i, 2L)
              ni <- flintLength(i)
              nx <- length(x)
              if (ni == 0L)
              i <- integer(0L)
              else {
              a <- min(i)
              b <- max(i)
              i <-
              if (a > -1L) {
                  if (b - 1L >= nx)
                      stop(.error.subscriptOutOfBounds(x, 0L, i))
                  else if (a >= 1L)
                      i
                  else i[i >= 1L]
              } else {
                  if (b >= 1L)
                      stop(.error.subscriptNegativePositive())
                  else if (a + 1L <= -nx)
                      stop(.error.subscriptOutOfBounds(x, 0L, i))
                  else .Call(R_flint_ulong_complement, .ulong(x = -i), nx, TRUE)
              }
              i <-
              if (is.double(nx))
                  as.double(i)
              else as.integer(i)
              }
              x[i] <- value
              x
          })

setMethod("[<-",
          c(x = "ANY", i = "flint", j = "missing", value = "flint"),
          function (x, i, j, ..., value) {
              cx <- .subscript.class(x, 1L)
              if (...length())
                  stop(.error.invalidArity())
              cv <- flintClass(value)
              common <- flintClassCommon(c(cx, cv))
              x <- as(x, common)
              value <- as(value, common)
              x[i] <- value
              x
          })

setMethod("[<-",
          c(x = "flint", i = "ANY", j = "missing", value = "ANY"),
          function (x, i, j, ..., value) {
              cx <- flintClass(x)
              if (...length())
                  stop(.error.invalidArity())
              if (missing(value))
                  stop(.error.missingSubassignValue())
              cv <- .subscript.class(value, 3L)
              common <- flintClassCommon(c(cx, cv))
              x <- as(x, common)
              value <- as(value, common)
              if (missing(i))
              x[ ] <- value
              else
              x[i] <- value
              x
          })

setMethod("[<-",
          c(x = "flint", i = "ANY", j = "missing", value = "flint"),
          function (x, i, j, ..., value) {
              cx <- flintClass(x)
              if (...length())
                  stop(.error.invalidArity())
              if (missing(i)) {
              ni <- nx <- flintLength(x)
              i <- NULL
              } else {
              ci <- .subscript.class(i, 2L)
              ni <- length(i)
              nx <- flintLength(x)
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
                                 stop(.error.subscriptOutOfBounds(x, 0L, i))
                             else {
                                 ni <- .ulong(x = ni)
                                 if (ni < nx) {
                                     q <- nx %/% ni
                                     r <- nx %% ni
                                     w <- w. <- .ulong(x = w)
                                     if (q >= 1L)
                                         w <- w + rep(seq(from = .ulong(x = 0L), by = ni, length.out = q), each = flintLength(w))
                                     if (r >= w[1L])
                                         w <- c(w, flintLength(w) + w.[w. <= r])
                                 }
                                 w
                             }
                         },
                     "integer" =,
                     "double" =
                         {
                             a <- min(i)
                             b <- max(i)
                             if (a > -1L) {
                                 if (b - 1L >= nx)
                                     stop(.error.subscriptOutOfBounds(x, 0L, i))
                                 else if (a >= 1L)
                                     i
                                 else i[i >= 1L]
                             } else {
                                 if (b >= 1L)
                                     stop(.error.subscriptNegativePositive())
                                 else if (a + 1L <= -nx)
                                     stop(.error.subscriptOutOfBounds(x, 0L, i))
                                 else .Call(R_flint_ulong_complement, .ulong(x = -i), nx, TRUE)
                             }
                         },
                     "character" =
                         {
                             if (is.null(nms <- names(x)) ||
                                 anyNA(m <- match(i, nms)))
                                 stop(.error.subscriptOutOfBounds(x, 0L, i))
                             m
                         })
              ni <- flintLengthAny(i)
              }
              }
              cv <- flintClass(value)
              nv <- flintLength(value)
              if (ni > 0L) {
              if (nv == 0L)
                  stop(.error.emptyReplace())
              else if (nv > ni || ni %% nv > 0L)
                  warning(.warning.remainderInReplace())
              }
              common <- flintClassCommon(c(cx, cv))
              x <- as(x, common)
              value <- as(value, common)
              .Call(R_flint_subassign, x, i, value)
          })

setMethod("[<-",
          c(x = "flint", i = "flint", j = "missing", value = "ANY"),
          function (x, i, j, ..., value) {
              cx <- flintClass(x)
              if (...length())
                  stop(.error.invalidArity())
              if (missing(value))
                  stop(.error.missingSubassignValue())
              cv <- .subscript.class(value, 3L)
              common <- flintClassCommon(c(cx, cv))
              x <- as(x, common)
              value <- as(value, common)
              x[i] <- value
              x
          })

setMethod("[<-",
          c(x = "flint", i = "flint", j = "missing", value = "flint"),
          function (x, i, j, ..., value) {
              cx <- flintClass(x)
              if (...length())
                  stop(.error.invalidArity())
              ci <- .subscript.class(i, 2L)
              ni <- flintLength(i)
              nx <- flintLength(x)
              if (ni == 0L)
              i <- integer(0L)
              else {
              a <- min(i)
              b <- max(i)
              i <-
              if (a > -1L) {
                  if (b - 1L >= nx)
                      stop(.error.subscriptOutOfBounds(x, 0L, i))
                  else if (a >= 1L)
                      i
                  else i[i >= 1L]
              } else {
                  if (b >= 1L)
                      stop(.error.subscriptNegativePositive())
                  else if (a + 1L <= -nx)
                      stop(.error.subscriptOutOfBounds(x, 0L, i))
                  else .Call(R_flint_ulong_complement, .ulong(x = -i), nx, TRUE)
              }
              ni <- flintLength(i)
              }
              cv <- flintClass(value)
              nv <- flintLength(value)
              if (ni > 0L) {
              if (nv == 0L)
                  stop(.error.emptyReplace())
              else if (nv > ni || ni %% nv > 0L)
                  warning(.warning.remainderInReplace())
              }
              common <- flintClassCommon(c(cx, cv))
              x <- as(x, common)
              value <- as(value, common)
              .Call(R_flint_subassign, x, i, value)
          })

setMethod("[[",
          c(x = "ANY", i = "flint", j = "missing"),
          function (x, i, j, ...) {
              cx <- .subscript.class(x, 1L)
              if (...length())
                  stop(.error.invalidArity())
              ci <- .subscript.class(i, 2L)
              ni <- flintLength(i)
              nx <- length(x)
              if (ni == 0L)
              i <- integer(0L)
              else {
              a <- min(i)
              b <- max(i)
              i <-
              if (a > -1L) {
                  if (b - 1L >= nx)
                      stop(.error.subscriptOutOfBounds(x, 0L, i))
                  else if (a >= 1L)
                      i
                  else i[i >= 1L]
              } else {
                  if (b >= 1L)
                      stop(.error.subscriptNegativePositive())
                  else if (a + 1L <= -nx)
                      stop(.error.subscriptOutOfBounds(x, 0L, i))
                  else .Call(R_flint_ulong_complement, .ulong(x = -i), nx, TRUE)
              }
              i <-
              if (is.double(nx))
                  as.double(i)
              else as.integer(i)
              }
              x[[i]]
          })

setMethod("[[",
          c(x = "flint", i = "ANY", j = "missing"),
          function (x, i, j, ...) {
              if (...length())
                  stop(.error.invalidArity())
              if (missing(i))
                  stop(.error.missingSubscript())
              ci <- .subscript.class(i, 2L)
              ni <- length(i)
              nx <- flintLength(x)
              if (ni > 0L) {
              i <-
              switch(ci,
                     "logical" =
                         {
                             nw <- length(w <- which(i))
                             if (nw != 1L)
                                 w
                             else if (w > nx)
                                 stop(.error.subscriptOutOfBounds(x, 0L, i))
                             else if (ni < nx && w <= nx - ni)
                                 stop(.error.subscriptTooMany())
                             else w
                         },
                     "integer" =,
                     "double" =
                         {
                             a <- min(i)
                             b <- max(i)
                             if (a > -1L) {
                                 if (b - 1L >= nx)
                                     stop(.error.subscriptOutOfBounds(x, 0L, i))
                                 else if (a >= 1L)
                                     i
                                 else i[i >= 1L]
                             } else {
                                 if (b >= 1L)
                                     stop(.error.subscriptNegativePositive())
                                 else if (a + 1L <= -nx)
                                     stop(.error.subscriptOutOfBounds(x, 0L, i))
                                 else .Call(R_flint_ulong_complement, .ulong(x = -i), nx, TRUE)
                             }
                         },
                     "character" =
                         {
                             if (is.null(nms <- names(x)) ||
                                 anyNA(m <- match(i, nms)))
                                 stop(.error.subscriptOutOfBounds(x, 0L, i))
                             m
                         })
              ni <- flintLengthAny(i)
              }
              if (ni < 1L)
                  stop(.error.subscriptTooFew())
              else if (ni > 1L)
                  stop(.error.subscriptTooMany())
              .Call(R_flint_subscript, x, i, FALSE)
          })

setMethod("[[",
          c(x = "flint", i = "flint", j = "missing"),
          function (x, i, j, ...) {
              if (...length())
                  stop(.error.invalidArity())
              ci <- .subscript.class(i, 2L)
              ni <- flintLength(i)
              nx <- flintLength(x)
              if (ni > 0L) {
              a <- min(i)
              b <- max(i)
              i <-
              if (a > -1L) {
                  if (b - 1L >= nx)
                      stop(.error.subscriptOutOfBounds(x, 0L, i))
                  else if (a >= 1L)
                      i
                  else i[i >= 1L]
              } else {
                  if (b >= 1L)
                      stop(.error.subscriptNegativePositive())
                  else if (a + 1L <= -nx)
                      stop(.error.subscriptOutOfBounds(x, 0L, i))
                  else .Call(R_flint_ulong_complement, .ulong(x = -i), nx, TRUE)
              }
              ni <- flintLength(i)
              }
              if (ni < 1L)
                  stop(.error.subscriptTooFew())
              else if (ni > 1L)
                  stop(.error.subscriptTooMany())
              .Call(R_flint_subscript, x, i, FALSE)
          })

setMethod("[[<-",
          c(x = "ANY", i = "ANY", j = "missing", value = "flint"),
          function (x, i, j, ..., value) {
              cx <- .subscript.class(x, 1L)
              if (...length())
                  stop(.error.invalidArity())
              cv <- flintClass(value)
              common <- flintClassCommon(c(cx, cv))
              x <- as(x, common)
              value <- as(value, common)
              if (missing(i))
              x[[ ]] <- value
              else
              x[[i]] <- value
              x
          })

setMethod("[[<-",
          c(x = "ANY", i = "flint", j = "missing", value = "ANY"),
          function (x, i, j, ..., value) {
              cx <- .subscript.class(x, 1L)
              if (...length())
                  stop(.error.invalidArity())
              if (missing(value))
                  stop(.error.missingSubassignValue())
              ci <- .subscript.class(i, 2L)
              ni <- flintLength(i)
              nx <- length(x)
              if (ni == 0L)
              i <- integer(0L)
              else {
              a <- min(i)
              b <- max(i)
              i <-
              if (a > -1L) {
                  if (b - 1L >= nx)
                      stop(.error.subscriptOutOfBounds(x, 0L, i))
                  else if (a >= 1L)
                      i
                  else i[i >= 1L]
              } else {
                  if (b >= 1L)
                      stop(.error.subscriptNegativePositive())
                  else if (a + 1L <= -nx)
                      stop(.error.subscriptOutOfBounds(x, 0L, i))
                  else .Call(R_flint_ulong_complement, .ulong(x = -i), nx, TRUE)
              }
              i <-
              if (is.double(nx))
                  as.double(i)
              else as.integer(i)
              }
              x[[i]] <- value
              x
          })

setMethod("[[<-",
          c(x = "ANY", i = "flint", j = "missing", value = "flint"),
          function (x, i, j, ..., value) {
              cx <- .subscript.class(x, 1L)
              if (...length())
                  stop(.error.invalidArity())
              cv <- flintClass(value)
              common <- flintClassCommon(c(cx, cv))
              x <- as(x, common)
              value <- as(value, common)
              x[[i]] <- value
              x
          })

setMethod("[[<-",
          c(x = "flint", i = "ANY", j = "missing", value = "ANY"),
          function (x, i, j, ..., value) {
              cx <- flintClass(x)
              if (...length())
                  stop(.error.invalidArity())
              if (missing(value))
                  stop(.error.missingSubassignValue())
              cv <- .subscript.class(value, 3L)
              common <- flintClassCommon(c(cx, cv))
              x <- as(x, common)
              value <- as(value, common)
              if (missing(i))
              x[[ ]] <- value
              else
              x[[i]] <- value
              x
          })

setMethod("[[<-",
          c(x = "flint", i = "ANY", j = "missing", value = "flint"),
          function (x, i, j, ..., value) {
              cx <- flintClass(x)
              if (...length())
                  stop(.error.invalidArity())
              if (missing(i))
                  stop(.error.missingSubscript())
              ci <- .subscript.class(i, 2L)
              ni <- length(i)
              nx <- flintLength(x)
              if (ni > 0L) {
              i <-
              switch(ci,
                     "logical" =
                         {
                             nw <- length(w <- which(i))
                             if (nw != 1L)
                                 w
                             else if (w > nx)
                                 stop(.error.subscriptOutOfBounds(x, 0L, i))
                             else if (ni < nx && w <= nx - ni)
                                 stop(.error.subscriptTooMany())
                             else w
                         },
                     "integer" =,
                     "double" =
                         {
                             a <- min(i)
                             b <- max(i)
                             if (a > -1L) {
                                 if (b - 1L >= nx)
                                     stop(.error.subscriptOutOfBounds(x, 0L, i))
                                 else if (a >= 1L)
                                     i
                                 else i[i >= 1L]
                             } else {
                                 if (b >= 1L)
                                     stop(.error.subscriptNegativePositive())
                                 else if (a + 1L <= -nx)
                                     stop(.error.subscriptOutOfBounds(x, 0L, i))
                                 else .Call(R_flint_ulong_complement, .ulong(x = -i), nx, TRUE)
                             }
                         },
                     "character" =
                         {
                             if (is.null(nms <- names(x)) ||
                                 anyNA(m <- match(i, nms)))
                                 stop(.error.subscriptOutOfBounds(x, 0L, i))
                             m
                         })
              ni <- flintLengthAny(i)
              }
              if (ni < 1L)
                  stop(.error.subscriptTooFew())
              else if (ni > 1L)
                  stop(.error.subscriptTooMany())
              if (missing(value))
                  stop(.error.missingSubassignValue())
              cv <- flintClass(value)
              nv <- flintLength(value)
              if (nv == 0L)
                  stop(.error.emptyReplace())
              else if (nv > 1L)
                  warning(.warning.remainderInReplace())
              common <- flintClassCommon(c(cx, cv))
              x <- as(x, common)
              value <- as(value, common)
              .Call(R_flint_subassign, x, i, value)
          })

setMethod("[[<-",
          c(x = "flint", i = "flint", j = "missing", value = "ANY"),
          function (x, i, j, ..., value) {
              cx <- flintClass(x)
              if (...length())
                  stop(.error.invalidArity())
              if (missing(value))
                  stop(.error.missingSubassignValue())
              cv <- .subscript.class(value, 3L)
              common <- flintClassCommon(c(cx, cv))
              x <- as(x, common)
              value <- as(value, common)
              x[[i]] <- value
              x
          })

setMethod("[[<-",
          c(x = "flint", i = "flint", j = "missing", value = "flint"),
          function (x, i, j, ..., value) {
              cx <- flintClass(x)
              if (...length())
                  stop(.error.invalidArity())
              if (missing(value))
                  stop(.error.missingSubassignValue())
              ci <- .subscript.class(i, 2L)
              ni <- flintLength(i)
              nx <- flintLength(x)
              if (ni > 0L) {
              a <- min(i)
              b <- max(i)
              i <-
              if (a > -1L) {
                  if (b - 1L >= nx)
                      stop(.error.subscriptOutOfBounds(x, 0L, i))
                  else if (a >= 1L)
                      i
                  else i[i >= 1L]
              } else {
                  if (b >= 1L)
                      stop(.error.subscriptNegativePositive())
                  else if (a + 1L <= -nx)
                      stop(.error.subscriptOutOfBounds(x, 0L, i))
                  else .Call(R_flint_ulong_complement, .ulong(x = -i), nx, TRUE)
              }
              ni <- flintLength(i)
              }
              if (ni < 1L)
                  stop(.error.subscriptTooFew())
              else if (ni > 1L)
                  stop(.error.subscriptTooMany())
              cv <- flintClass(value)
              nv <- flintLength(value)
              if (nv == 0L)
                  stop(.error.emptyReplace())
              else if (nv > 1L)
                  warning(.warning.remainderInReplace())
              common <- flintClassCommon(c(cx, cv))
              x <- as(x, common)
              value <- as(value, common)
              .Call(R_flint_subassign, x, i, value)
          })

.all.equal <-
function (target, current,
          tolerance = sqrt(.Machine[["double.eps"]]),
          scale = NULL,
          countEQ = FALSE,
          formatFUN = function (err, what) format(err),
          ...,
          check.attributes = TRUE,
          check.names = TRUE,
          check.class = TRUE,
          giveErr = FALSE) {
    msg <- NULL
    if (check.attributes) {
        if (check.names) {
            nt <- names(target)
            nc <- names(current)
            if (length(nt) && length(nc)) {
                if (is.character(ae <- all.equal.character(nt, nc)))
                    msg <- c(msg, paste0("Names: ", ae))
            }
            else if (length(nt))
                msg <- c(msg, "names for target but not for current")
            else if (length(nc))
                msg <- c(msg, "names for current but not for target")
        }
        at <- attributes(target)
        ac <- attributes(current)
        at[["class"]] <- ac[["class"]] <- NULL
        at[["names"]] <- ac[["names"]] <- NULL
        if (!is.na(flintClass(target)))
        at[[".xData"]] <- NULL
        if (!is.na(flintClass(current)))
        ac[[".xData"]] <- NULL
        if (length(at) || length(ac)) {
            at <- if (length(nt <- names(at))) at[order(nt)]
            ac <- if (length(nc <- names(ac))) ac[order(nc)]
            if (is.character(ae <- all.equal(at, ac, tolerance = tolerance, scale = scale, ...)))
                msg <- c(msg, paste0("Attributes: < ", ae, " >"))
        }
    }
    if (!(is.atomic( target) && !is.character( target)) &&
        !(is.atomic(current) && !is.character(current))) {
        if ((tt <- typeof(target)) != (tc <- typeof(current)))
        return(c(msg, gettextf("target type is \"%s\", current type is \"%s\"",
                               tt, tc)))
    }
    if (check.class) {
        if (!identical(ct <- class(target), cc <- class(current)))
        return(c(msg, gettextf("target class is %s, current class is %s",
                               deparse(ct), deparse(cc))))
        common <- cc
    } else {
        target <- as(target, "flint")
        current <- as(current, "flint")
        common <- flintClassCommon(c(flintClass(target), flintClass(current)))
        if (common == "mag")
            common <- "arf"
        target <- as(target, common)
        current <- as(target, common)
    }
    if ((nt <- flintLengthAny(target)) != (nc <- flintLengthAny(current)))
        return(c(msg, gettextf("target length is %s, current length is %s",
                               format(nt), format(nc))))
    if (any(common == c("arf", "acf", "arb", "acb"))) {
        out <- is.na(target)
        if (any(d <- out != is.na(current)))
        return(c(msg, gettextf("NaN mismatch at index %.0f",
                               which.max(d))))
        out <- out | target == current
    }
    else
        out <- target == current
    if (all(out))
        return(if (is.null(msg)) TRUE else msg)
    if (scale.default <- is.null(scale)) {
        if (countEQ)
            scale <- mean(abs(target))
    }
    else if (length(scale) != 1L && length(scale) != nt)
        stop(gettextf("length of '%s' is not 1 or length(%s)",
                      "scale", "target"),
             domain = NA)
    else if (is.na(m <- min(scale)) || !(m > 0))
        stop(gettextf("'%s' is not positive",
                      "scale"),
             domain = NA)
    else scale.unit <- all(scale == 1)
    if (any(out)) {
        w <- which(!out)
        target <- target[w]
        current <- current[w]
        if (!scale.default && length(scale) == nt)
            scale <- scale[w]
    }
    if (is.null(scale))
        scale <- mean(abs(target))
    if (scale.default)
        scale.unit <- is.na(scale) || scale <= tolerance
    err <-
    if (scale.unit)
        mean(abs(target - current))
    else mean(abs(target - current)/scale)
    ans <-
    if (err <= tolerance) {
        if (is.null(msg))
            TRUE
        else msg
    }
    else if (scale.unit) {
        what <- "absolute"
        c(msg, gettextf("mean absolute difference is %s",
                        formatFUN(err, what)))
    }
    else if (scale.default) {
        what <- "relative"
        c(msg, gettextf("mean relative difference is %s",
                        formatFUN(err, what)))
    }
    else {
        what <- "scaled"
        c(msg, gettextf("mean scaled difference is %s",
                        formatFUN(err, what)))
    }
    if (giveErr) {
        attr(ans, "err") <- err
        attr(ans, "what") <- what
    }
    ans
}

setMethod("all.equal",
          c(target =   "ANY", current = "flint"),
          .all.equal)

setMethod("all.equal",
          c(target = "flint", current =   "ANY"),
          .all.equal)

setMethod("all.equal",
          c(target = "flint", current = "flint"),
          .all.equal)

rm(.all.equal)

setMethod("anyDuplicated",
          c(x = "flint"),
          function (x, incomparables = FALSE, ...) {
              if (!(missing(incomparables) ||
                    (is.logical(incomparables) &&
                     length(incomparables) == 1L &&
                     !(is.na(incomparables) || incomparables))))
                  incomparables <- mtfrm(as(incomparables, flintClass(x)))
              anyDuplicated(mtfrm(x), incomparables = incomparables, ...)
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

setMethod("as.matrix",
          c(x = "flint"),
          function (x, ...) as.matrix(as.vector(x), ...))

setMethod("as.array",
          c(x = "flint"),
          function (x, ...) as.array (as.vector(x), ...))

setMethod("as.data.frame",
          c(x = "flint"),
          as.data.frame.vector)

setMethod("as.Date",
          c(x = "flint"),
          function (x, ...)
              as.Date   (`names<-`(as.vector(x), names(x)),          ...))

setMethod("as.POSIXct",
          c(x = "flint"),
          function (x, tz = "", ...)
              as.POSIXct(`names<-`(as.vector(x), names(x)), tz = tz, ...))

setMethod("as.POSIXlt",
          c(x = "flint"),
          function (x, tz = "", ...)
              as.POSIXlt(`names<-`(as.vector(x), names(x)), tz = tz, ...))

.c.class <-
function (x)
    switch(type. <- typeof(x),
           "NULL" =, "raw" =, "logical" =, "integer" =, "double" =, "complex" =, "character" =, "symbol" =, "pairlist" =, "list" =, "expression" =
                                                                                       type.,
           "S4" =
               if (is.na(class. <- flintClass(x)))
                   stop(.error.invalidArgumentClass(x))
               else class.,
           stop(.error.invalidArgumentType(x)))

c.flint <-
function (..., recursive = FALSE, use.names = TRUE) {
    if (nargs() == 0L)
        return(NULL)
    if (recursive) {
        args <- c(NULL, ..., recursive = TRUE, use.names = use.names)
        if (!is.recursive(args))
            return(args)
    }
    else
        args <- list(...)
    classes <- vapply(args, .c.class, "")
    common <- flintClassCommon(classes, strict = FALSE)
    if (any(common == c("NULL", "raw", "logical", "integer", "double", "complex")))
        return(c(NULL, ..., recursive = FALSE, use.names = use.names))
    args <- lapply(args, as, common)
    if (any(common == c("character", "list", "expression")))
        unlist(args, recursive = FALSE, use.names = use.names)
    else .Call(R_flint_bind, args, as.logical(use.names))
}

setMethod("c",
          c(x = "flint"),
          function (x, ...)
              c.flint(x, ...))

setMethod("cut",
          c(x = "flint"),
          function (x, breaks,
                    include.lowest = FALSE, right = TRUE, ...)
              findInterval(x,
                           vec = breaks,
                           left.open = right,
                           rightmost.closed = include.lowest))

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
          function (x, incomparables = FALSE, ...) {
              if (!(missing(incomparables) ||
                    (is.logical(incomparables) &&
                     length(incomparables) == 1L &&
                     !(is.na(incomparables) || incomparables))))
                  incomparables <- mtfrm(as(incomparables, flintClass(x)))
              duplicated(mtfrm(x), incomparables = incomparables, ...)
          })

setMethod("findInterval",
          c(x = "flint"),
          function (x, vec,
                    rightmost.closed = FALSE,
                    all.inside = FALSE,
                    left.open = FALSE) {
              common <- flintClassCommon(c(flintClass(x), flintClassAny(vec)))
              if (any(common == c("acf", "acb")))
                  stop(gettextf("'%s' and '%s' are not both real",
                                "x", "vec"),
                       domain = NA)
              if (anyNA(x))
                  stop(gettextf("'%s' contains NaN",
                                "x"),
                       domain = NA)
              if (length(vec) < 2L)
                  stop(gettextf("length of '%s' is not greater than or equal to 2",
                                "vec"),
                       domain = NA)
              if (is.na(u <- is.unsorted(vec)) || u)
                  stop(gettextf("'%s' is not nondecreasing",
                                "vec"),
                       domain = NA)
              x <- as(x, common)
              vec <- as(vec, common)
              .Call(R_flint_find_interval, x, vec,
                    as.logical(left.open),
                    as.logical(rightmost.closed),
                    as.logical(all.inside))
          })

setMethod("is.na<-",
          c(x = "flint"),
          function (x, value) {
              x[value] <- switch(flintClass(x), "ulong" =, "slong" =, "fmpz" =, "fmpq" = NA_integer_, "mag" =, "arf" =, "arb" = NA_real_, "acf" =, "acb" = NA_complex_)
              x
          })

setMethod("length",
          c(x = "flint"),
          function (x)
              flintLength(x, exact = FALSE))

setMethod("length<-",
          c(x = "flint"),
          function (x, value)
              .Call(R_flint_realloc, x, as(value, "ulong")))

.match <-
function (x, table, nomatch = NA_integer_, incomparables = NULL) {
    common <- flintClassCommon(c(flintClassAny(x), flintClassAny(table)))
    if (!(is.null(incomparables) ||
          (is.logical(incomparables) &&
           length(incomparables) == 1L &&
           !(is.na(incomparables) || incomparables))))
        incomparables <- mtfrm(as(incomparables, common))
    match(mtfrm(as(x, common)), mtfrm(as(table, common)),
          nomatch = nomatch, incomparables = incomparables)
}

setMethod("match",
          c(x =   "ANY", table = "flint"),
          .match)

setMethod("match",
          c(x = "flint", table =   "ANY"),
          .match)

setMethod("match",
          c(x = "flint", table = "flint"),
          .match)

rm(.match)

setMethod("mtfrm",
          c(x = "flint"),
          function (x)
              format(`names<-`(x, NULL), base = 62L, digits = 0L))

setMethod("names",
          c(x = "flint"),
          function (x)
              if (length(value <- x@names)) value)

setMethod("names<-",
          c(x = "flint", value = "NULL"),
          function (x, value) {
              if (length(x@names))
                  x@names <- character(0L)
              x
          })

setMethod("names<-",
          c(x = "flint", value = "character"),
          function (x, value) {
              nx <- length(x)
              if (nx > 0x1p+52) {
                  warning(gettextf("length of '%s' exceeds maximum character vector length; ignoring non-NULL '%s'",
                                   "x", "value"),
                          domain = NA)
                  return(x)
              }
              nv <- length(value <- as.character(value))
              x@names <-
              if (nv == nx)
                  value
              else {
                  if (nv > nx)
                  stop(gettextf("length of '%s' exceeds length of '%s'",
                                "value", "x"),
                       domain = NA)
                  c(value, character(nx - nv))
              }
              x
          })

setMethod("print",
          c(x = "flint"),
          function (x, digits = NULL, max = NULL, Rdiff = NULL, ...) {
              s <- flintTriple(x)
              if (is.null(Rdiff))
                  Rdiff <- getOption("flint.Rdiff", FALSE)
              Rdiff <- as.logical(Rdiff)
              if (Rdiff)
              cat(gettextf("class \"%s\", length %s, address <pointer: %s>",
                           s[1L], s[2L], s[3L]),
                  "\n", sep = "")
              else
              cat(gettextf("class \"%s\", length %s, address %s",
                           s[1L], s[2L], s[3L]),
                  "\n", sep = "")
              len <- length(x)
              if (len > 0L) {
                  if (is.null(max))
                      max <- getOption("max.print", 99999L)
                  max <- as.integer(max)
                  if (len <= max)
                      print.default(format(x, digits = digits), quote = FALSE, max = max, ...)
                  else {
                      print.default(format(x[seq_len(max)], digits = digits), quote = FALSE, max = max, ...)
                      cat(gettextf(" [ reached '%s' / getOption(\"%s\") -- omitted %.0f entries ]",
                                   "max", "max.print", len - max),
                          "\n", sep = "")
                  }
              }
              invisible(x)
          })

setMethod("quantile",
          c(x = "flint"),
          function (x, probs = .fmpq(num = 0L:4L, den = 4L), type = 7L,
                    ...) {
              class. <-
              switch(flintClass(x),
                     "ulong" =, "slong" =, "fmpz" =, "fmpq" = "fmpq",
                     "mag" =, "arf" = "arf",
                     "acf" = "acf",
                     stop(.error.notTotalOrder()))
              if (anyNA(x))
                  stop(gettextf("'%s' contains NaN",
                                "x"),
                       domain = NA)
              x <- as(x, class.)
              n <- flintLength(x)
              if (n == 0L)
                  stop(gettextf("'%s' of length zero are not yet supported",
                                "x"),
                       domain = NA)
              if (!missing(probs)) {
                  if (anyNA(rp <- range(probs)) || rp[1L] < 0 || rp[2L] > 1)
                      stop(gettextf("'%s' is not in [%.0f,%.0f]",
                                    "probs", 0, 1),
                           domain = NA)
                  probs <- as(probs, "fmpq")
              }
              if (!missing(type)) {
                  if (type < 1L || type >= 10L)
                      stop(gettextf("'%s' is not in %d:%d",
                                    "type", 1L, 9L),
                           domain = NA)
                  type <- as.integer(type)
              }
              ## Adapting stats:::quantile.default ...
              if (type == 7L) {
                  index <- 1L + (n - 1L) * probs
                  lo <- floor(index)
                  hi <- ceiling(index)
                  x <- sort(x, partial = as.double(unique(c(lo, hi))))
                  qs <- x[lo]
                  i <- which(index > lo & x[hi] != qs)
                  h <- (index - lo)[i]
                  qs[i] <- (1L - h) * qs[i] + h * x[hi[i]]
              } else {
                  if (type <= 3L) {
                      nppm <- n * probs
                      if (type == 3L)
                          nppm <- nppm - .fmpq(num = 1L, den = 2L)
                      j <- floor(nppm)
                      h <- switch(type,
                                  nppm > j,
                                  .fmpq(num = (nppm > j) + 1L, den = 2L),
                                  nppm != j | j%%2L == 1L)
                  } else {
                      switch(type - 3L,
                             {
                                 a <- 0L
                                 b <- 1L
                             },
                             a <- b <- .fmpq(num = 1L, den = 2L),
                             a <- b <- 0L,
                             a <- b <- 1L,
                             a <- b <- .fmpq(num = 1L, den = 3L),
                             a <- b <- .fmpq(num = 3L, den = 8L))
                      nppm <- a + (n + 1L - a - b) * probs
                      j <- floor(nppm)
                      h <- nppm - j
                  }
                  x <- sort(x, partial = as.double(unique(c(1L, j[j > 0L & j <= n], (j + 1L)[j > 0L & j < n], n))))
                  x <- c(x[c(1L, 1L)], x, x[c(n, n)])
                  qs <- x[j + 2L]
                  qs[h == 1L] <- x[j + 3L][h == 1L]
                  other <- 0L < h & h < 1L & x[j + 2L] != x[j + 3L]
                  if (is.na(a <- any(other)) || a) {
                      if (is.na(a))
                          other[is.na(other)] <- TRUE
                      qs[other] <- ((1L - h) * x[j + 2L] + h * x[j + 3L])[other]
                  }
              }
              qs
          })

setMethod("rep",
          c(x = "flint"),
          function (x, times, length.out, each, ...) {
              if (!missing(each))
                  x <- .Call(R_flint_rep_each, x, as(each, "ulong"), TRUE)
              if (!missing(length.out))
                  x <- .Call(R_flint_rep_lengthout, x, as(length.out, "ulong"), TRUE)
              else if (!missing(times))
                  x <- .Call(R_flint_rep_times, x, as(times, "ulong"), TRUE)
              x
          })

setMethod("rep.int",
          c(x = "flint"),
          function (x, times)
              .Call(R_flint_rep_times, x, as(times, "ulong"), FALSE))

setMethod("rep_len",
          c(x = "flint"),
          function (x, length.out)
              .Call(R_flint_rep_lengthout, x, as(length.out, "ulong"), FALSE))

setMethod("seq",
          c("..." = "flint"),
          function (from, to, by, length.out, along.with, ...) {
               if (!missing(from)) {
               if (length(from) != 1L)
                   stop(gettextf("length of '%s' is not %d",
                                 "from", 1L),
                        domain = NA)
               else if (!is.finite(from))
                   stop(gettextf("'%s' is not a finite number",
                                 "from"),
                        domain = NA)
               }
               if (!missing(to)) {
               if (length(to) != 1L)
                   stop(gettextf("length of '%s' is not %d",
                                 "to", 1L),
                        domain = NA)
               else if (!is.finite(to))
                   stop(gettextf("'%s' is not a finite number",
                                 "to"),
                        domain = NA)
               }
               if (!missing(by)) {
               if (length(by) != 1L)
                   stop(gettextf("length of '%s' is not %d",
                                 "by", 1L),
                        domain = NA)
               else if (!is.finite(by))
                   stop(gettextf("'%s' is not a finite number",
                                 "by"),
                        domain = NA)
               else if (!missing(from) && !missing(to) && from != to &&
                        (from < to) == (by < 0L))
                   stop(gettextf("sign of to-from and sign of '%s' are not equal",
                                 "by"),
                        domain = NA)
               }
               if (!missing(length.out)) {
               if (length(length.out) != 1L)
                   stop(gettextf("length of '%s' is not %d",
                                 "length.out", 1L),
                        domain = NA)
               else if (is.na(length.out) || length.out < 0L)
                   stop(gettextf("'%s' is not a nonnegative number",
                                 "length.out"),
                        domain = NA)
               else if (length.out >= if (flintABI() == 64L) 0x1p+64 else 0x1p+32)
                   stop(gettextf("value length would exceed maximum 2^%d-1",
                                 flintABI()),
                        domain = NA)
               length.out <- as(length.out, "ulong")
               }
               if (!missing(along.with)) {
               if (!missing(length.out))
                   stop(gettextf("one of '%s' and '%s' must be missing",
                                 "length.out", "along.with"),
                        domain = NA)
               length.out <- flintLengthAny(along.with)
               }
               .seq <-
               function (from, length.out, reverse = FALSE)
                   .Call(R_flint_ulong_seq, from, length.out, reverse)
               zero <- .ulong(x = 0L)
               unit <- .ulong(x = 1L)
               switch(nargs() - ...length(),
               {
                   if (missing(length.out))
                       stop(gettextf("usage seq(%s=) is not yet implemented",
                                     if (missing(from)) if (missing(to)) "by" else "to" else "from"),
                            domain = NA)
                   .seq(.ulong(x = 1L), length.out)
               },
               {
                   if (missing(length.out) != missing(by))
                       stop(gettextf("usage seq(%s=, %s=) is not yet implemented",
                                     if (missing(from)) "to" else "from", if (missing(by)) "length.out" else "by"),
                            domain = NA)
                   if (missing(length.out)) {
                       d <- if (from <= to) { op <- `+`; to - from } else { op <- `-`; from - to }
                       d. <- as(d, "fmpz")
                       if (d. == d)
                           d. <- d. + unit
                       if (d. >= if (flintABI() == 64L) 0x1p+64 else 0x1p+32)
                           stop(gettextf("value length would exceed maximum 2^%d-1",
                                         flintABI()),
                                domain = NA)
                       op(from, .seq(zero, as(d., "ulong")))
                   }
                   else unit + by * .seq(zero, length.out)
               },
               {
                   if (missing(length.out)) {
                       d <- if (from == to) 0L else (to - from)/by
                       d. <- as(d, "fmpz")
                       if (d. == d)
                           d. <- d. + unit
                       if (d. >= if (flintABI() == 64L) 0x1p+64 else 0x1p+32)
                           stop(gettextf("value length would exceed maximum 2^%d-1",
                                         flintABI()),
                                domain = NA)
                       from + by * .seq(zero, as(d., "ulong"))
                   }
                   else if (missing(by)) {
                       by <- if (length.out <= unit) zero else (to - from)/(length.out - unit)
                       from + by * .seq(zero, length.out)
                   }
                   else if (missing(to))
                       from + by * .seq(zero, length.out)
                   else to - by * .seq(zero, length.out, reverse = TRUE)
               },
               stop(gettextf("usage seq(%s=, %s=, %s=, %s=) is not yet implemented",
                             "from", "to", "by", if (missing(along.with)) "length.out" else "along.with"),
                    domain = NA))
          })

setMethod("sequence",
          c(nvec = "flint"),
          function (nvec, from = .ulong(x = 1L), by = .ulong(x = 1L), ...) {
              n. <- c(flintLengthAny(nvec),
                      flintLengthAny(from),
                      flintLengthAny(by))
              if (max(n.) > .Machine[["integer.max"]])
                  stop("too many subsequences")
              n <- max(n. <- as.integer(n.))
              if (n.[1L] < n)
                  nvec <- rep_len(nvec, n)
              if (n.[2L] < n)
                  from <- rep_len(from, n)
              if (n.[3L] < n)
                  by   <- rep_len(by  , n)
              l <- vector("list", n)
              for (i in seq_len(n))
                  l[[i]] <- seq(from = from[i], by = by[i], length.out = nvec[i])
              do.call(c.flint, l)
          })

setMethod("show",
          c(object = "flint"),
          function (object) {
              print(object)
              invisible(NULL)
          })

setMethod("summary",
          c(object = "flint"),
          function (object, triple = FALSE, quantile.type = 7L, ...) {
              if (triple || any(flintClass(object) == c("acf", "arb", "acb")))
                  return(`class<-`(`names<-`(flintTriple(object), c("class", "length", "address")), "noquote"))
              if (anyna <- anyNA(object))
                  object <- object[!(isna <- is.na(object))]
              qq <- quantile(object, type = quantile.type)
              qq <- c(qq[1L:3L], mean(object), qq[4L:5L])
              names(qq) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
              if (anyna) c(qq, "NaN" = sum(isna)) else qq
          })

setMethod("unique",
          c(x = "flint"),
          function (x, incomparables = FALSE, ...) {
              names(x) <- NULL
              x[!duplicated(x, incomparables = incomparables, ...)]
          })
