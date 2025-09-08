.subscript.missing <- substitute()

.subscript.class <-
function (object, mode)
    switch(mode,
           ## x
           switch(type. <- typeof(object),
                  "raw" =, "logical" =, "integer" =, "double" =, "complex" =, "character" =, "NULL" =, "pairlist" =, "language" =, "list" =, "expression" =
                                                                                              type.,
                  "S4" =
                      if (is.na(class. <- flintClass(object)))
                          stop(.error.notSubsettable(object))
                      else class.,
                  stop(.error.notSubsettable(object))),
           ## i, j, ...
           switch(type. <- typeof(object),
                  "NULL" =, "logical" =, "integer" =, "double" =, "character" =
                      if (anyNA(object))
                          stop("NA subscripts not supported")
                      else type.,
                  "symbol" =
                      if (identical(object, .subscript.missing))
                          "missing"
                      else stop(.error.invalidSubscriptClass(object)),
                  "S4" =
                      switch(class. <- flintClass(object),
                             "ulong" =, "slong" =, "fmpz" =, "fmpq" =
                                 class.,
                             stop(.error.invalidSubscriptClass(object))),
                  stop(.error.invalidSubscriptType(object))),
           ## value
           switch(type. <- typeof(object),
                  "raw" =, "logical" =, "integer" =, "double" =, "complex" =, "character" =, "NULL" =, "pairlist" =, "language" =, "list" =, "expression" =
                      type.,
                  "S4" =
                      if (is.na(class. <- flintClass(object)))
                          stop(.error.invalidSubassignValueClass(object))
                      else class.,
                  stop(.error.invalidSubassignValueType(object))),
           stop("should never happen ..."))

.subscript.list <-
function (i, j, ..., ns, dx, call, exps) {
    mi <- missing(i)
    mj <- missing(j)
    nd <- ...length()
    if ((ns == 1L && !(nd == 0L && mj && is.null(call$j))) ||
        (ns >= 2L && ns != nd + 2L))
        stop(gettextf("actual arguments of '%s' call match formal argument '%s' but none match formal argument '%s'",
                      "[",
                      if (nd >= 1L) "..." else "j",
                      if (ns >= 2L && mj && is.null(call$j)) "j" else "i")
             domain = NA)
    if (ns == 0L)
        list()
    else if (ns == 1L)
        list(if (mi) .subscript.missing else i)
    else {
        if (ns != length(dx))
            stop(.error.invalidArity())
        ans <-
        list(if (mi) .subscript.missing else i,
             if (mj) .subscript.missing else j)
        if (ns >= 3L) {
            ans <- `[<-`(vector("list", ns), 1L:2L, ans)
            for (k in 3L:ns)
                if (identical(i <- exps[[k]], .subscript.missing) ||
                    !is.null(i <- ...elt(k - 2L)))
                    ans[[k]] <- i
        }
        ans
    }
}

.subscript.1 <-
function (x, i, j, ..., drop = TRUE) {
    cx <- .subscript.class(x, 1L)
    dx <- dim(x)
    call <- sys.call(sys.nframe())
    exps <- substitute(i(j, ...))
    ns <- nargs() - 2L + (missing(drop) && is.null(call$drop))
    s <- .subscript.list(i, j, ..., ns = ns, dx = dx,
                         call = call, exps = exps)
    if (ns == 0L)
        x
    else if (ns != length(dx)) {
        i <- s[[1L]]
        ci <- .subscript.class(i, 2L)
        if (ci == "missing")
            return(x)
        nx <- flintLengthAny(x)
        ni <- flintLengthAny(i)
        i <- if (ni == 0L) integer(0L) else
        switch(ci,
               "logical" =
               {
                   nw <- length(w <- which(i))
                   if (nw > 0L) {
                       if (w[[nw]] > nx)
                           stop(.error.subscriptOutOfBounds(x, 0L, i))
                       else if (ni < nx) {
                           q <- nx %/% ni
                           r <- nx %% ni
                           w <- w. <- .ulong(w)
                           if (q >= 1L)
                               w <- w + rep(seq(from = .ulong(0L), by = ni, length.out = q), each = flintLength(w))
                           if (r >= w[1L])
                               w <- c(w, flintLength(w) + w.[w. <= r])
                       }
                   }
                   w
               },
               "integer" =, "double" =,
               "ulong" =, "slong" =, "fmpz" =, "fmpq" =
               {
                   a <- min(i)
                   b <- max(i)
                   if (!is.null(dx) && !is.null(di <- dim(i)) &&
                       length(di) == 2L && di[[2L]] == length(dx)) {
                       if (a <= -1L || b - 1L >= .Machine[["integer.max"]] ||
                           any((m <- t(`dim<-`(as.integer(i) - 1L, di))) >= dx))
                           stop(.error.subscriptOutOfBounds(x, 0L, i))
                       else {
                           m. <- colSums(.ulong(m) * cumprod(.ulong(c(1L, dx[-length(dx)])))) + .ulong(1L)
                           if (a < 1L)
                               m.[colSums(m < 1L) == 0L]
                           else m.
                       }
                   } else {
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
                           else .Call(R_flint_ulong_complement, .ulong(-i), nx, TRUE)
                       }
                   }
               },
               "character" =
               {
                   if (!is.null(dx) && !is.null(di <- dim(i)) &&
                       length(di) == 2L && di[[2L]] == length(dx)) {
                       if (is.null(nms <- dimnames(x)) ||
                           {
                               m <- array(0L, di[2L:1L])
                               for (k in seq_along(dx))
                                   m[k, ] <- match(i[, k], nms[[k]]) - 1L
                               anyNA(m)
                           })
                           stop(.error.subscriptOutOfBounds(x, 0L, i))
                       else colSums(.ulong(m) * cumprod(.ulong(c(1L, dx[-length(dx)])))) + .ulong(1L)
                   } else {
                       if (is.null(nms <- names(x)) ||
                           anyNA(m <- match(i, nms)))
                           stop(.error.subscriptOutOfBounds(x, 0L, i))
                       else m
                   }
               },
               stop("should never happen ..."))
        if (typeof(x) == "S4") {
            if (typeof(i) == "S4" && flintClass(i) != "ulong")
                i <- .ulong(i)
            Call(R_flint_subscript, x, i, TRUE)
        } else {
            if (typeof(i) == "S4")
                i <- if (nx <= .Machine[["integer.max"]]) as.integer(i) else as.double(i)
            x[i]
        }
    } # if (ns != length(dx))
    else {
        nms <- dimnames(x)
        for (k in seq_len(ns)) {
            i <- s[[k]]
            ci <- .subscript.class(i, 2L)
            if (ci == "missing")
                next
            nx <- dx[[k]]
            ni <- length(i)
            i <- if (ni == 0L) integer(0L) else
            switch(ci,
                   "logical" =
                   {
                       nw <- length(w <- which(i))
                       if (nw > 0L) {
                           if (w[[nw]] > nx)
                               stop(.error.subscriptOutOfBounds(x, k, i))
                           else if (ni < nx) {
                               q <- nx %/% ni
                               r <- nx %% ni
                               w. <- w
                               if (q >= 1L)
                                   w <- w + rep(seq(from = 0L, by = ni, length.out = q), each = length(w))
                               if (r >= w[1L])
                                   w <- c(w, length(w) + w.[w. <= r])
                           }
                       }
                       w
                   },
                   "integer" =, "double" =,
                   "ulong" =, "slong" =, "fmpz" =, "fmpq" =
                   {
                       a <- min(i)
                       b <- max(i)
                       if (a > -1L) {
                           if (b - 1L >= nx)
                               stop(.error.subscriptOutOfBounds(x, k, i))
                           else if (a >= 1L)
                               i
                           else i[i >= 1L]
                       } else {
                           if (b >= 1L)
                               stop(.error.subscriptNegativePositive())
                           else if (a + 1L <= -nx)
                               stop(.error.subscriptOutOfBounds(x, k, i))
                           else .Call(R_flint_ulong_complement, .ulong(-i), .ulong(nx), TRUE)
                       }
                   },
                   "character" =
                   {
                       if (is.null(nms[[k]]) ||
                           anyNA(m <- match(i, nms[[k]])))
                           stop(.error.subscriptOutOfBounds(x, k, i))
                       else m
                   },
                   stop("should never happen ..."))
            if (typeof(x) == "S4") {
                if (typeof(i) == "S4" && flintClass(i) != "ulong")
                    i <- .ulong(i)
            } else {
                if (typeof(i) == "S4")
                    i <- as.integer(i)
            }
            s[[k]] <- i
        } # for (k in seq_len(ns))
        if (typeof(x) == "S4") {
            ans <- .Call(R_flint_subscript, x, s, TRUE)
            if (drop) drop(ans) else ans
        } else
            do.call(`[`, c(list(x), s, list(drop = if (drop) TRUE else FALSE)))
    } # if (ns == length(dx))
}

.subscript.2 <-
function (x, i, j, ...) {
    cx <- .subscript.class(x, 1L)
    dx <- dim(x)
    call <- sys.call(sys.nframe())
    exps <- substitute(i(j, ...))
    ns <- nargs() - 1L
    s <- .subscript.list(i, j, ..., ns = ns, dx = dx,
                         call = call, exps = exps)
    if (ns == 0L)
        stop(.error.missingSubscript())
    else if (ns != length(dx)) {
        i <- s[[1L]]
        ci <- .subscript.class(i, 2L)
        if (ci == "missing")
            stop(.error.missingSubscript())
        nx <- flintLengthAny(x)
        ni <- flintLengthAny(i)
        if (ni == 0L)
            stop(.error.subscriptTooFew())
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
               "integer" =, "double" =,
               "ulong" =, "slong" =, "fmpz" =, "fmpq" =
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
                       else .Call(R_flint_ulong_complement, .ulong(-i), nx, TRUE)
                   }
               },
               "character" =
               {
                   if (is.null(nms <- names(x)) ||
                       anyNA(m <- match(i, nms)))
                       stop(.error.subscriptOutOfBounds(x, 0L, i))
                   else m
               },
               stop("should never happen ..."))
        ni <- flintLengthAny(i)
        if (ni < 1L)
            stop(.error.subscriptTooFew())
        if (ni > 1L)
            stop(.error.subscriptTooMany())
        if (typeof(x) == "S4") {
            if (typeof(i) == "S4" && flintClass(i) != "ulong")
                i <- .ulong(i)
            .Call(R_flint_subscript, x, i, FALSE)
        }
        else {
            if (typeof(i) == "S4")
                i <- if (nx <= .Machine[["integer.max"]]) as.integer(i) else as.double(i)
            x[[i]]
        }
    } # if (ns != length(dx))
    else {
        nms <- dimnames(x)
        for (k in seq_len(ns)) {
            i <- s[[k]]
            ci <- .subscript.class(i, 2L)
            if (ci == "missing")
                stop(.error.missingSubscript())
            nx <- dx[[k]]
            ni <- length(i)
            if (ni == 0L)
                stop(.error.subscriptTooFew())
            i <-
            switch(ci,
                   "logical" =
                   {
                       nw <- length(w <- which(i))
                       if (nw != 1L)
                           w
                       else if (w > nx)
                           stop(.error.subscriptOutOfBounds(x, k, i))
                       else if (ni < nx && w <= nx - ni)
                           stop(.error.subscriptTooMany())
                       else w
                   },
                   "integer" =, "double" =,
                   "ulong" =, "slong" =, "fmpz" =, "fmpq" =
                   {
                       a <- min(i)
                       b <- max(i)
                       if (a > -1L) {
                           if (b - 1L >= nx)
                               stop(.error.subscriptOutOfBounds(x, k, i))
                           else if (a >= 1L)
                               i
                           else i[i >= 1L]
                       } else {
                           if (b >= 1L)
                               stop(.error.subscriptNegativePositive())
                           else if (a + 1L <= -nx)
                               stop(.error.subscriptOutOfBounds(x, k, i))
                           else .Call(R_flint_ulong_complement, .ulong(-i), .ulong(nx), TRUE)
                       }
                   },
                   "character" =
                   {
                       if (is.null(nms[[k]]) ||
                           anyNA(m <- match(i, nms[[k]])))
                           stop(.error.subscriptOutOfBounds(x, k, i))
                       else m
                   },
                   stop("should never happen ..."))
            ni <- flintLengthAny(i)
            if (ni < 1L)
                stop(.error.subscriptTooFew())
            if (ni > 1L)
                stop(.error.subscriptTooMany()) # well, most likely
            if (typeof(x) == "S4") {
                if (typeof(i) == "S4" && flintClass(i) != "ulong")
                    i <- .ulong(i)
            }
            else {
                if (typeof(i) == "S4")
                    i <- as.integer(i)
            }
            s[[k]] <- i
        } # for (k in seq_len(ns))
        if (typeof(x) == "S4")
            .Call(R_flint_subscript, x, s, TRUE)
        else do.call(`[[`, c(list(x), s))
    } # if (ns == length(dx))
}

.subassign.1 <-
function (x, i, j, ..., value) {
    cx <- .subscript.class(x, 1L)
    dx <- dim(x)
    call <- sys.call(sys.nframe())
    exps <- substitute(i(j, ...))
    ns <- nargs() - 2L + (missing(value) && is.null(call$value))
    s <- .subscript.list(i, j, ..., ns = ns, dx = dx,
                         call = call, exps = exps)
    if (missing(value))
        stop(.error.missingSubassignValue())
    cv <- .subscript.class(value, 3L)

}

.subassign.2 <-
function (x, i, j, ..., value) {
    if (is.null(x))
        x <- list()
    cx <- .subscript.class(x, 1L)
    dx <- dim(x)
    call <- sys.call(sys.nframe())
    exps <- substitute(i(j, ...))
    ns <- nargs() - 2L + (missing(value) && is.null(call$value))
    s <- .subscript.list(i, j, ..., ns = ns, dx = dx,
                         call = call, exps = exps)
    if (missing(value))
        stop(.error.missingSubassignValue())
    cv <- .subscript.class(value, 3L)

}






setMethod("[",
          c(x = "ANY", i = "ANY", j = "flint", drop = "ANY"),
          function (x, i, j, ..., drop = TRUE) {

          })

setMethod("[",
          c(x = "ANY", i = "flint", j = "ANY", drop = "ANY"),
          function (x, i, j, ..., drop = TRUE) {

          })

setMethod("[",
          c(x = "ANY", i = "flint", j = "flint", drop = "ANY"),
          function (x, i, j, ..., drop = TRUE) {

          })

setMethod("[",
          c(x = "flint", i = "ANY", j = "ANY", drop = "ANY"),
          function (x, i, j, ..., drop = TRUE) {

          })

setMethod("[",
          c(x = "flint", i = "flint", j = "missing", drop = "missing"),
          function (x, i, j, ..., drop = TRUE) {

          })

setMethod("[<-",
          c(x = "ANY", i = "ANY", j = "missing", value = "flint"),
          function (x, i, j, ..., value) {

          })

setMethod("[<-",
          c(x = "ANY", i = "flint", j = "missing", value = "ANY"),
          function (x, i, j, ..., value) {

          })

setMethod("[<-",
          c(x = "ANY", i = "flint", j = "missing", value = "flint"),
          function (x, i, j, ..., value) {

          })

setMethod("[<-",
          c(x = "flint", i = "ANY", j = "missing", value = "ANY"),
          function (x, i, j, ..., value) {

          })

setMethod("[<-",
          c(x = "flint", i = "ANY", j = "missing", value = "flint"),
          function (x, i, j, ..., value) {

          })

setMethod("[<-",
          c(x = "flint", i = "flint", j = "missing", value = "ANY"),
          function (x, i, j, ..., value) {

          })

setMethod("[<-",
          c(x = "flint", i = "flint", j = "missing", value = "flint"),
          function (x, i, j, ..., value) {

          })

setMethod("[[",
          c(x = "ANY", i = "flint", j = "missing"),
          function (x, i, j, ...) {

          })

setMethod("[[",
          c(x = "flint", i = "ANY", j = "missing"),
          function (x, i, j, ...) {

          })

setMethod("[[",
          c(x = "flint", i = "flint", j = "missing"),
          function (x, i, j, ...) {

          })

setMethod("[[<-",
          c(x = "ANY", i = "ANY", j = "missing", value = "flint"),
          function (x, i, j, ..., value) {

          })

setMethod("[[<-",
          c(x = "ANY", i = "flint", j = "missing", value = "ANY"),
          function (x, i, j, ..., value) {

          })

setMethod("[[<-",
          c(x = "ANY", i = "flint", j = "missing", value = "flint"),
          function (x, i, j, ..., value) {

          })

setMethod("[[<-",
          c(x = "flint", i = "ANY", j = "missing", value = "ANY"),
          function (x, i, j, ..., value) {

          })

setMethod("[[<-",
          c(x = "flint", i = "ANY", j = "missing", value = "flint"),
          function (x, i, j, ..., value) {

          })

setMethod("[[<-",
          c(x = "flint", i = "flint", j = "missing", value = "ANY"),
          function (x, i, j, ..., value) {

          })

setMethod("[[<-",
          c(x = "flint", i = "flint", j = "missing", value = "flint"),
          function (x, i, j, ..., value) {

          })
