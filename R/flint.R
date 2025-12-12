.subscript.missing <- as.name(".__WAS_MISSING__.")

.subscript.class <-
function (object, mode) {
    type. <- typeof(object)
    switch(mode,
           ## x
           switch(type.,
                  "raw" =, "logical" =, "integer" =, "double" =, "complex" =, "character" =, "NULL" =, "pairlist" =, "language" =, "list" =, "expression" =
                                                                                              type.,
                  "S4" =
                      if (is.na(class. <- flintClass(object)))
                          stop(.error.notSubsettable(object))
                      else class.,
                  stop(.error.notSubsettable(object))),
           ## i, j, ...
           switch(type.,
                  "NULL" =, "logical" =, "integer" =, "double" =, "character" =
                      if (anyNA(object))
                          stop("NA subscripts not supported")
                      else type.,
                  "symbol" =
                      if (identical(object, .subscript.missing))
                          "missing"
                      else stop(.error.subscriptInvalidClass(object)),
                  "S4" =
                      switch(class. <- flintClass(object),
                             "ulong" =, "slong" =, "fmpz" =, "fmpq" =
                                 class.,
                             stop(.error.subscriptInvalidClass(object))),
                  stop(.error.subscriptInvalidType(object))),
           ## value
           switch(type.,
                  "raw" =, "logical" =, "integer" =, "double" =, "complex" =, "character" =, "NULL" =, "pairlist" =, "language" =, "list" =, "expression" =
                      type.,
                  "S4" =
                      if (is.na(class. <- flintClass(object)))
                          stop(.error.subassignInvalidClass(object))
                      else class.,
                  stop(.error.subassignInvalidType(object))),
           stop("should never happen ..."))
}

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
                      if (ns >= 2L && mj && is.null(call$j)) "j" else "i"),
             domain = NA)
    if (ns == 0L)
        list()
    else if (ns == 1L)
        list(if (mi) .subscript.missing else i)
    else {
        if (ns != length(dx))
            stop(.error.subscriptInvalidArity())
        ans <-
        list(if (mi) .subscript.missing else i,
             if (mj) .subscript.missing else j)
        if (ns >= 3L) {
            ans <- `[<-`(vector("list", ns), 1L:2L, ans)
            for (k in 3L:ns) {
                if (identical(exps[[k]], substitute()))
                    ans[[k]] <- .subscript.missing
                else if (!is.null(i <- ...elt(k - 2L)))
                    ans[[k]] <- i
            }
        }
        ans
    }
}

.subscript.1 <-
function (x, i, j, ..., drop = TRUE) {
    if (is.null(x))
        return(NULL)
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
        if (ni == 0L)
        i <- integer(0L)
        else
        i <-
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
                           w <- w. <- ulong(w)
                           if (q >= 1L)
                               w <- w + rep(seq(from = ulong(0L), by = ni, length.out = q), each = flintLength(w))
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
                           m. <- colSums(ulong(m) * cumprod(ulong(c(1L, dx[-length(dx)])))) + ulong(1L)
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
                           else .Call(R_flint_ulong_complement, ulong(-i), nx, TRUE)
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
                       else colSums(ulong(m) * cumprod(ulong(c(1L, dx[-length(dx)])))) + ulong(1L)
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
                i <- ulong(i)
            .Call(R_flint_subscript, x, i, 1L)
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
            if (ni == 0L)
            i <- integer(0L)
            else {
            i <-
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
                           else .Call(R_flint_ulong_complement, ulong(-i), ulong(nx), TRUE)
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
                    i <- ulong(i)
            } else {
                if (typeof(i) == "S4")
                    i <- as.integer(i)
            }
            }
            s[[k]] <- i
        } # for (k in seq_len(ns))
        if (typeof(x) == "S4") {
            ans <- .Call(R_flint_subscript, x, s, 1L)
            if (drop) drop(ans) else ans
        } else
            do.call(`[`, c(list(x), s, list(drop = if (drop) TRUE else FALSE)))
    } # if (ns == length(dx))
}

.subscript.2 <-
function (x, i, j, ...) {
    if (is.null(x))
        return(NULL)
    if (is.environment(x))
        return(callNextMethod())
    cx <- .subscript.class(x, 1L)
    dx <- dim(x)
    call <- sys.call(sys.nframe())
    exps <- substitute(i(j, ...))
    ns <- nargs() - 1L
    s <- .subscript.list(i, j, ..., ns = ns, dx = dx,
                         call = call, exps = exps)
    if (ns == 0L)
        stop(.error.subscriptMissing())
    else if (ns != length(dx)) {
        i <- s[[1L]]
        ci <- .subscript.class(i, 2L)
        if (ci == "missing")
            stop(.error.subscriptMissing())
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
                       else .Call(R_flint_ulong_complement, ulong(-i), nx, TRUE)
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
        if (ni != 1L)
            stop(if (ni == 0L) .error.subscriptTooFew() else .error.subscriptTooMany())
        if (typeof(x) == "S4") {
            if (typeof(i) == "S4" && flintClass(i) != "ulong")
                i <- ulong(i)
            .Call(R_flint_subscript, x, i, 2L)
        } else {
            if (typeof(i) == "S4")
                i <- if (nx <= .Machine[["integer.max"]]) as.integer(i) else as.double(i)
            x[[i]]
        }
    } # if (ns != length(dx))
    else {
        nms <- dimnames(x)
        anyLong <- FALSE
        for (k in seq_len(ns)) {
            i <- s[[k]]
            ci <- .subscript.class(i, 2L)
            if (ci == "missing")
                stop(.error.subscriptMissing())
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
                           else .Call(R_flint_ulong_complement, ulong(-i), ulong(nx), TRUE)
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
            ni <- length(i)
            if (ni != 1L) {
                if (ni == 0L)
                    stop(.error.subscriptTooFew())
                anyLong <- TRUE
            }
            if (typeof(x) == "S4") {
                if (typeof(i) == "S4" && flintClass(i) != "ulong")
                    i <- ulong(i)
            } else {
                if (typeof(i) == "S4")
                    i <- as.integer(i)
            }
            s[[k]] <- i
        } # for (k in seq_len(ns))
        if (anyLong)
            stop(.error.subscriptTooMany())
        if (typeof(x) == "S4")
            .Call(R_flint_subscript, x, s, 2L)
        else do.call(`[[`, c(list(x), s))
    } # if (ns == length(dx))
}

.subassign.1 <-
function (x, i, j, ..., value) {
    if (missing(value))
        stop(.error.subassignMissing())
    if (is.null(x) && is.null(value))
        return(NULL)
    cx <- .subscript.class(x, 1L)
    cv <- .subscript.class(value, 3L)
    common <- flintClassCommon(c(cx, cv), strict = FALSE)
    x <- asVector(x, common, strict = FALSE)
    value <- as(value, common)
    dx <- dim(x)
    call <- sys.call(sys.nframe())
    exps <- substitute(i(j, ...))
    ns <- nargs() - 2L
    s <- .subscript.list(i, j, ..., ns = ns, dx = dx,
                         call = call, exps = exps)
    if (ns == 0L) {
        nx <- flintLengthAny(x)
        if (nx > 0L && flintLengthAny(value) == 0L)
            stop(.error.subassignTooFew())
        if (typeof(x) == "S4")
            .Call(R_flint_subassign, x, .subscript.missing, 1L, value)
        else {
            x[] <- value
            x
        }
    }
    else if (ns != length(dx)) {
        i <- s[[1L]]
        ci <- .subscript.class(i, 2L)
        if (ci == "missing") {
            nx <- flintLengthAny(x)
            if (nx > 0L && flintLengthAny(value) == 0L)
                stop(.error.subassignTooFew())
            return(
            if (typeof(x) == "S4")
                .Call(R_flint_subassign, x, .subscript.missing, 1L, value)
            else {
                x[] <- value
                x
            }
            )
        }
        nx <- flintLengthAny(x)
        ni <- flintLengthAny(i)
        if (ni == 0L)
        i <- integer(0L)
        else {
        i <-
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
                           w <- w. <- ulong(w)
                           if (q >= 1L)
                               w <- w + rep(seq(from = ulong(0L), by = ni, length.out = q), each = flintLength(w))
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
                           m. <- colSums(ulong(m) * cumprod(ulong(c(1L, dx[-length(dx)])))) + ulong(1L)
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
                           else .Call(R_flint_ulong_complement, ulong(-i), nx, TRUE)
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
                       else colSums(ulong(m) * cumprod(ulong(c(1L, dx[-length(dx)])))) + ulong(1L)
                   } else {
                       if (is.null(nms <- names(x)) ||
                           anyNA(m <- match(i, nms)))
                           stop(.error.subscriptOutOfBounds(x, 0L, i))
                       else m
                   }
               },
               stop("should never happen ..."))
        ni <- flintLengthAny(i)
        if (ni > 0L && flintLengthAny(value) == 0L)
            stop(.error.subassignTooFew())
        }
        if (typeof(x) == "S4") {
            if (typeof(i) == "S4" && flintClass(i) != "ulong")
                i <- ulong(i)
            .Call(R_flint_subassign, x, i, 1L, value)
        } else {
            if (typeof(i) == "S4")
                i <- if (nx <= .Machine[["integer.max"]]) as.integer(i) else as.double(i)
            x[i] <- value
            x
        }
    } # if (ns != length(dx))
    else {
        nms <- dimnames(x)
        anyEmpty <- FALSE
        for (k in seq_len(ns)) {
            i <- s[[k]]
            ci <- .subscript.class(i, 2L)
            if (ci == "missing")
                next
            nx <- dx[[k]]
            ni <- length(i)
            if (ni == 0L) {
            i <- integer(0L)
            anyEmpty <- TRUE
            } else {
            i <-
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
                           else .Call(R_flint_ulong_complement, ulong(-i), ulong(nx), TRUE)
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
            ni <- length(i)
            if (ni == 0L)
                anyEmpty <- TRUE
            if (typeof(x) == "S4") {
                if (typeof(i) == "S4" && flintClass(i) != "ulong")
                    i <- ulong(i)
            } else {
                if (typeof(i) == "S4")
                    i <- as.integer(i)
            }
            }
            s[[k]] <- i
        } # for (k in seq_len(ns))
        if (!anyEmpty && flintLengthAny(value) == 0L)
            stop(.error.subassignTooFew())
        if (typeof(x) == "S4")
            .Call(R_flint_subassign, x, s, 1L, value)
        else do.call(`[<-`, c(list(x), s, list(value)))
    } # if (ns == length(dx))
}

.subassign.2 <-
function (x, i, j, ..., value) {
    if (missing(value))
        stop(.error.subassignMissing())
    if (is.null(x))
        return(if (is.null(value)) NULL else { x <- list(); callGeneric() })
    if (is.environment(x))
        return(callNextMethod())
    cx <- .subscript.class(x, 1L)
    if (!is.recursive(x)) {
    if (!is.null(value) && !is.atomic(value) && is.na(flintClass(value))) {
        warning(gettextf("coercing left hand side of '%s' assignment to type \"%s\"",
                         "[[<-", "list"),
                domain = NA)
        x <- asVector(x, "list", strict = FALSE)
        return(callGeneric())
    }
    cv <- .subscript.class(value, 3L)
    common <- flintClassCommon(c(cx, cv), strict = FALSE)
    x <- asVector(x, common, strict = FALSE)
    value <- as(value, common)
    }
    dx <- dim(x)
    call <- sys.call(sys.nframe())
    exps <- substitute(i(j, ...))
    ns <- nargs() - 2L
    s <- .subscript.list(i, j, ..., ns = ns, dx = dx,
                         call = call, exps = exps)
    if (ns == 0L)
        stop(.error.subscriptMissing())
    else if (ns != length(dx)) {
        i <- s[[1L]]
        ci <- .subscript.class(i, 2L)
        if (ci == "missing")
            stop(.error.subscriptMissing())
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
                       else .Call(R_flint_ulong_complement, ulong(-i), nx, TRUE)
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
        if (ni != 1L)
            stop(if (ni < 1L) .error.subscriptTooFew() else .error.subscriptTooMany())
        if (!is.recursive(x)) {
        nv <- flintLengthAny(value)
        if (nv != 1L)
            stop(if (ni < 1L) .error.subassignTooFew() else .error.subassignTooMany())
        }
        if (typeof(x) == "S4") {
            if (typeof(i) == "S4" && flintClass(i) != "ulong")
                i <- ulong(i)
            .Call(R_flint_subassign, x, i, 2L, value)
        } else {
            if (typeof(i) == "S4")
                i <- if (nx <= .Machine[["integer.max"]]) as.integer(i) else as.double(i)
            x[[i]] <- value
            x
        }
    } # if (ns != length(dx))
    else {
        nms <- dimnames(x)
        anyLong <- FALSE
        for (k in seq_len(ns)) {
            i <- s[[k]]
            ci <- .subscript.class(i, 2L)
            if (ci == "missing")
                stop(.error.subscriptMissing())
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
                           else .Call(R_flint_ulong_complement, ulong(-i), ulong(nx), TRUE)
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
            ni <- length(i)
            if (ni != 1L) {
                if (ni == 0L)
                    stop(.error.subscriptTooFew())
                anyLong <- TRUE
            }
            if (typeof(x) == "S4") {
                if (typeof(i) == "S4" && flintClass(i) != "ulong")
                    i <- ulong(i)
            } else {
                if (typeof(i) == "S4")
                    i <- as.integer(i)
            }
            s[[k]] <- i
        } # for (k in seq_len(ns))
        if (anyLong)
            stop(.error.subscriptTooMany())
        if (!is.recursive(x)) {
        nv <- flintLengthAny(value)
        if (nv != 1L)
            stop(if (nv < 1L) .error.subassignTooFew() else .error.subassignTooMany())
        }
        if (typeof(x) == "S4")
            .Call(R_flint_subassign, x, s, 2L, value)
        else do.call(`[[<-`, c(list(x), s, list(value)))
    } # if (ns == length(dx))
}

setMethod("$",
          c(x = "flint"),
          function (x, name)
              stop(gettextf("'%s' operator is invalid for atomic-like vectors",
                            "$"),
                   domain = NA))

setMethod("$<-",
          c(x = "flint"),
          function (x, name, value)
              stop(gettextf("'%s' operator is invalid for atomic-like vectors",
                            "$<-"),
                   domain = NA))

. <- c("ANY", "flint")
.G <- as.matrix(expand.grid(x = ., i = ., j = .))
for (.I in 2L:nrow(.G)) {
    setMethod( "["  , .G[.I, ], .subscript.1)
    setMethod("[["  , .G[.I, ], .subscript.2)
}
.G <- as.matrix(expand.grid(x = ., i = ., j = ., value = .))
for (.I in 2L:nrow(.G)) {
    setMethod( "[<-", .G[.I, ], .subassign.1)
    setMethod("[[<-", .G[.I, ], .subassign.2)
}
rm(., .G, .I)

rm(.subscript.1, .subscript.2, .subassign.1, .subassign.2)

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
        attributes4 <-
        function(x) {
            a <- attributes(x)
            if (!is.null(a) && isS4(x))
                for (s in .slotNames(x))
                    if (identical(a[[s]], quote(`\001NULL\001`)))
                        a[[s]] <- NULL
            a
        }
        at <- attributes4(target)
        ac <- attributes4(current)
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
        current <- as(current, common)
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
        stop(gettextf("length of '%s' is not %d or %s",
                      "scale", 1L, "length(target)"),
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
          function (x, incomparables = FALSE, MARGIN = 1L, ...) {
              if (is.null(d <- x@dim) ||
                  !(is.character(MARGIN) || any(MARGIN <- as.integer(MARGIN)))) {
                  if (!isFALSE(incomparables))
                      incomparables <- mtfrm(as(incomparables, flintClass(x)))
                  anyDuplicated.default(mtfrm(x), incomparables = incomparables, ...)
              } else {
                  if (!isFALSE(incomparables))
                      .NotYetUsed("incomparables")
                  anyDuplicated.default(asplit3(mtfrm(x), MARGIN = MARGIN, drop = TRUE), ...)
              }
          })

setMethod("aperm",
          c(a = "flint"),
          function (a, perm, resize = TRUE, ...) {
              perm <-
              if (missing(perm))
                  NULL
              else if (is.character(perm))
                  match(perm, names(a@dimnames), 0L)
              else as.integer(perm)
              .Call(R_flint_aperm, a, perm, as.logical(resize))
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
          function (x, ...) {
              d <- x@dim
              dn <- x@dimnames
              n <- x@names
              if (length(d) != 2L) {
                  m <- length(x)
                  if (!is.integer(m))
                      stop(gettextf("%s[[%d]] would exceed maximum %d",
                                    "dim", 1L, .Machine[["integer.max"]]),
                           domain = NA)
                  d <- c(m, 1L)
                  dn <- if (!is.null(n)) list(n, NULL)
                  n <- NULL
              }
              `names<-`(`dimnames<-`(`dim<-`(as.vector(x), d), dn), n)
          })

setMethod("as.array",
          c(x = "flint"),
          function (x, ...) {
              d <- x@dim
              dn <- x@dimnames
              n <- x@names
              if (length(d) == 0L) {
                  m <- length(x)
                  if (!is.integer(m))
                      stop(gettextf("%s[[%d]] would exceed maximum %d",
                                    "dim", 1L, .Machine[["integer.max"]]),
                           domain = NA)
                  d <- m
                  dn <- if (!is.null(n)) list(n)
                  n <- NULL
              }
              `names<-`(`dimnames<-`(`dim<-`(as.vector(x), d), dn), n)
          })

setMethod("as.data.frame",
          c(x = "flint"),
          function (x, row.names = NULL, optional = FALSE,
                    make.names = TRUE, ..., nm = deparse(substitute(x))) {
              force(nm)
              d <- x@dim
              dn <- x@dimnames
              n <- x@names
              x@dim <- x@dimnames <- x@names <- NULL
              if (is.null(d)) {
                  ans <- list(x)
                  r <- length(x)
                  if (!is.integer(r))
                      stop(gettext("number of rows would exceed maximum %d",
                                   .Machine[["integer.max"]]),
                           domain = NA)
                  if (is.null(row.names))
                      row.names <- n
                  names <- if (!optional) nm[[1L]]
              } else {
                  ans <- vector("list", prod(d[-1L]))
                  r <- d[[1L]]
                  for (i in seq_along(ans))
                      ans[[i]] <- x[seq.int(to = i * r, length.out = r)]
                  if (is.null(row.names))
                      row.names <- dn[[1L]]
                  names <-
                  if (!is.null(dn) && length(d) > 1L) {
                      if (length(d) == 2L) {
                          if (length(w <- which(!nzchar(dn[[2L]]))))
                              dn[[2L]][w] <- paste0("V", w)
                          dn[[2L]]
                      } else {
                          for (i in 2L:length(d))
                              if (is.null(dn[[i]]))
                                  dn[[i]] <- seq_len(d[[i]])
                          interaction(expand.grid(dn[-1L]))
                      }
                  }
                  else if (!optional) paste0("V", seq_along(ans))
              }
              oldClass(ans) <- "data.frame"
              names(ans) <- names
              if (is.null(row.names))
                  attr(ans, "row.names") <- .set_row_names(r)
              else .rowNamesDF(ans, make.names) <- row.names
              ans
          })

setMethod("as.Date",
          c(x = "flint"),
          function (x, ...)
              as.Date   (asVector(x, "vector", FALSE),          ...))

setMethod("as.POSIXct",
          c(x = "flint"),
          function (x, tz = "", ...)
              as.POSIXct(asVector(x, "vector", FALSE), tz = tz, ...))

setMethod("as.POSIXlt",
          c(x = "flint"),
          function (x, tz = "", ...)
              as.POSIXlt(asVector(x, "vector", FALSE), tz = tz, ...))

.initAsplit <-
function (where = topenv(parent.frame())) {
suppressMessages(setGeneric("asplit"))
if (getRversion() >= "4.5.0") {
setMethod("asplit",
          c(x = "flint"),
          function (x, MARGIN, drop = FALSE) {
              MARGIN <-
              if (is.character(MARGIN))
                  match(MARGIN, names(x@dimnames), 0L)
              else as.integer(MARGIN)
              .Call(R_flint_asplit, x, MARGIN, as.logical(drop))
          },
          where = where)
assign("asplit3", envir = where, inherits = FALSE,
       asplit)
} else {
setMethod("asplit",
          c(x = "flint"),
          function (x, MARGIN) {
              MARGIN <-
              if (is.character(MARGIN))
                  match(MARGIN, names(x@dimnames), 0L)
              else as.integer(MARGIN)
              .Call(R_flint_asplit, x, MARGIN, FALSE)
          },
          where = where)
assign("asplit3", envir = where, inherits = FALSE,
       function (x, MARGIN, drop = FALSE) {
           ans <- asplit(x, MARGIN)
           if (drop)
               lapply(ans, `dim<-`, NULL)
           else ans
       })
}
invisible(NULL)
}

.bind.class <-
function (x)
    switch(type. <- typeof(x),
           "raw" =, "logical" =, "integer" =, "double" =, "complex" =, "character" =, "NULL" =, "pairlist" =, "symbol" =, "language" =, "list" =, "expression" =
                                                                                       type.,
           "S4" =
               if (is.na(class. <- flintClass(x)))
                   stop(.error.argumentInvalidClass(x))
               else class.,
           stop(.error.argumentInvalidType(x)))

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
    classes <- vapply(args, .bind.class, "")
    common <- flintClassCommon(classes, strict = FALSE)
    if (any(common == c("NULL", "raw", "logical", "integer", "double", "complex")))
        return(c(NULL, ..., recursive = FALSE, use.names = use.names))
    args <- lapply(args, as, common)
    if (any(common == c("character", "list", "expression")))
        unlist(args, recursive = FALSE, use.names = use.names)
    else {
        exps <- substitute(list(...))
        .Call(R_flint_bind, 2L, as.logical(use.names), args, exps)
    }
}

setMethod("c",
          c(x = "flint"),
          function (x, ...)
              c.flint(x, ...))

cbind.flint <-
function (..., deparse.level = 1) {
    if (nargs() == 0L)
        return(NULL)
    args <- list(...)
    classes <- vapply(args, .bind.class, "")
    common <- flintClassCommon(classes, strict = FALSE)
    if (any(common == c("NULL", "raw", "logical", "integer", "double", "complex")))
        return(cbind(NULL, ..., deparse.level = deparse.level))
    args <- lapply(args, asVector, common, strict = FALSE)
    if (any(common == c("character", "list", "expression")))
        do.call(cbind, c(args, list(deparse.level = deparse.level)))
    else {
        exps <- substitute(list(...))
        .Call(R_flint_bind, 1L, as.integer(deparse.level), args, exps)
    }
}

setMethod("cbind2",
          c(x =   "ANY", y = "flint"),
          function (x, y, ...)
              cbind.flint(x, y, deparse.level = 0L))

setMethod("cbind2",
          c(x = "flint", y =   "ANY"),
          function (x, y, ...)
              cbind.flint(x, y, deparse.level = 0L))

setMethod("cbind2",
          c(x = "flint", y = "flint"),
          function (x, y, ...)
              cbind.flint(x, y, deparse.level = 0L))

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
                 "raw" = ulong(from),
                 "logical" =, "integer" = slong(from),
                 "double" = arf(from),
                 "complex" = acf(from),
                 "character" =
                 stop(gettextf("coercion from \"%s\" to \"%s\" is not yet implemented; consider coercing to a nonvirtual subclass of \"%s\"",
                               type., "flint", "flint"),
                      domain = NA),
                 stop(gettextf("coercion from \"%s\" to \"%s\" is not yet implemented",
                               type., "flint"),
                      domain = NA)))

setMethod("diag",
          c(x = "flint"),
          function (x = 1, nrow, ncol, names = TRUE) {
              if (is.null(d <- x@dim)) {
                  if (missing(nrow)) nrow <- length(x)
                  if (missing(ncol)) ncol <- nrow
                  .Call(R_flint_diag, x, as.integer(nrow), as.integer(ncol))
              } else {
                  if (length(d) != 2L)
                      stop(gettextf("'%s' is an array but not a matrix",
                                    "x"),
                           domain = NA)
                  if (!(missing(nrow) && missing(ncol)))
                      stop(gettextf("attempt to set '%s' when '%s' is a matrix",
                                    if (missing(nrow)) "ncol" else "nrow", "x"),
                           domain = NA)
                  ans <- .Call(R_flint_diag, x, NULL, NULL)
                  if (names &&
                      !(is.null(dn <- x@dimnames) ||
                        is.null(rn <- dn[[1L]]) ||
                        is.null(cn <- dn[[2L]])) &&
                      {
                          h <- seq_len(min(d))
                          identical(an <- rn[h], cn[h])
                      })
                      ans@names <- an
                  ans
              }
          })

setMethod("diag<-",
          c(x = "flint"),
          function (x, value) {
              if (length(d <- x@dim) != 2L)
                  stop(gettextf("'%s' is not a matrix", "x"),
                       domain = NA)
              i <- ulong(1L) + (ulong(d[1L]) + ulong(1L)) * .Call(R_flint_ulong_seq, ulong(0L), ulong(min(d)), FALSE)
              x[i] <- value
              x
          })

setMethod("dim",
          c(x = "flint"),
          function (x)
              x@dim)

setMethod("dim<-",
          c(x = "flint", value = "NULL"),
          function (x, value) {
              if (!is.null(x@dim)) {
                  x@dim <- NULL
                  if (!is.null(x@dimnames))
                      x@dimnames <- NULL
              }
              x
          })

setMethod("dim<-",
          c(x = "flint", value = "numeric"),
          function (x, value) {
              if (length(value) == 0L)
                  stop(gettextf("length of '%s' is %d",
                                "value", 0L),
                       domain = NA)
              if (is.double(length(value)))
                  stop(gettextf("length of '%s' exceeds maximum %d",
                                "value", .Machine[["integer.max"]]),
                       domain = NA)
              if (anyNA(value))
                  stop(gettextf("%s[[%d]] is NA",
                                "value", which.max(is.na(value))),
                       domain = NA)
              if (min(value) < 0L)
                  stop(gettextf("%s[[%d]] is negative",
                                "value", which.max(value < 0)),
                       domain = NA)
              if (is.double(value) &&
                  max(value) - 1 > .Machine[["integer.max"]])
                  stop(gettextf("%s[[%d]] exceeds maximum %d",
                                "value", which.max(value - 1 > .Machine[["integer.max"]]), .Machine[["integer.max"]]),
                       domain = NA)
              if ((nv <- prod(ulong(value))) != (nx <- flintLength(x)))
                  stop(gettextf("product of '%s' [%s] is not equal to length of '%s' [%s]",
                                "value", format(nv), "x", format(nx)),
                       domain = NA)
              x@dim <- as.integer(value)
              if (!is.null(x@dimnames))
                  x@dimnames <- NULL
              x
          })

setMethod("dimnames",
          c(x = "flint"),
          function (x)
              x@dimnames)

setMethod("dimnames<-",
          c(x = "flint", value = "NULL"),
          function (x, value) {
              if (!is.null(x@dimnames))
                  x@dimnames <- NULL
              x
          })

setMethod("dimnames<-",
          c(x = "flint", value = "list"),
          function (x, value) {
              dim <- x@dim
              if (length(dim) == 0L)
                  stop(gettextf("attempt to assign '%s' to non-array",
                                "dimnames"),
                       domain = NA)
              if (length(value) != length(dim))
                  stop(gettextf("length of '%s' [%.0f] is not equal to length of '%s' [%.0f]",
                                "value", length(value), "dim", length(dim)),
                       domain = NA)
              for (i in seq_along(dim))
              switch(typeof(s <- value[[i]]),
                     "NULL" = NULL,
                     "raw" =, "logical" =, "integer" =, "double" =, "complex" =, "character" =, "list" =, "expression" =
                         value[i] <-
                         if (length(s) == 0L)
                             list(NULL)
                         else if (length(s) == dim[[i]])
                             list(if (is.factor(s)) as.character.factor(s) else as.character.default(s))
                         else
                             stop(gettextf("length of %s[[%d]] [%.0f] is not equal to %s[[%d]] [%.0f]",
                                           "value", i, length(s), "dim", i, dim[[i]]),
                                  domain = NA),
                     stop(gettextf("invalid type \"%s\" for %s[[%d]]",
                                   typeof(s), "value", i),
                          domain = NA))
              a <- attributes(value)
              if (!is.null(a) && (length(a) > 1L || names(a) != "names")) {
                  tmp <- names(value)
                  attributes(value) <- NULL
                  names(value) <- tmp
              }
              x@dimnames <- value
              x
          })

setMethod("drop",
          c(x = "flint"),
          function (x) {
              d <- x@dim
              if (!all(k <- d != 1L)) {
                  dn <- x@dimnames
                  if (length(w <- which(k)) > 1L) {
                      x@dim <- d[w]
                      x@dimnames <- dn[w]
                  } else {
                      x@dim <- x@dimnames <- NULL
                      x@names <-
                      if (length(w) == 1L ||
                          length(w <- which(!vapply(dn, is.null, FALSE))) == 1L)
                          dn[[w]]
                  }
              }
              x
          })

setMethod("duplicated",
          c(x = "flint"),
          function (x, incomparables = FALSE, MARGIN = 1L, ...) {
              ans <-
              if (is.null(d <- x@dim) ||
                  !(is.character(MARGIN) || any(MARGIN <- as.integer(MARGIN)))) {
                  if (!isFALSE(incomparables))
                      incomparables <- mtfrm(as(incomparables, flintClass(x)))
                  duplicated.default(y <- mtfrm(x), incomparables = incomparables, ...)
              } else {
                  if (!isFALSE(incomparables))
                      .NotYetUsed("incomparables")
                  duplicated.default(y <- asplit3(mtfrm(x), MARGIN = MARGIN, drop = TRUE), ...)
              }
              dim(ans) <- dim(y)
              dimnames(ans) <- dimnames(y)
              ans
          })

setMethod("eigen",
          c(x = "flint"),
          function (x, symmetric, only.values = FALSE, EISPACK = FALSE)
              .NotYetImplemented())

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

setMethod("identical",
          c(x = "flint", y = "flint"),
          function (x, y,
                    num.eq = TRUE, single.NA = TRUE,
                    attrib.as.set = TRUE, ignore.bytecode = TRUE,
                    ignore.environment = FALSE, ignore.srcref = TRUE,
                    extptr.as.ref = FALSE) {
              x. <- x
              y. <- y
              if (!extptr.as.ref)
                  x@.xData <- y@.xData <- new("externalptr")
              base::identical(x, y,
                              num.eq, single.NA,
                              attrib.as.set, ignore.bytecode,
                              ignore.environment, ignore.srcref,
                              extptr.as.ref) &&
                  (extptr.as.ref || .Call(R_flint_identical, x., y.))
          })

setMethod("is.array",
          c(x = "flint"),
          function (x)
              length(x@dim) >= 1L)

setMethod("is.matrix",
          c(x = "flint"),
          function (x)
              length(x@dim) == 2L)

setMethod("is.na<-",
          c(x = "flint"),
          function (x, value) {
              x[value] <- switch(flintClass(x), "ulong" =, "slong" =, "fmpz" =, "fmpq" = NA_integer_, "mag" =, "arf" =, "arb" = NA_real_, "acf" =, "acb" = NA_complex_)
              x
          })

setMethod("isSymmetric",
          c(object = "flint"),
          function (object,
                    tol = 100 * .Machine$double.eps, tol1 = 8 * tol,
                    trans = "C", ...) {
              if (length(d <- object@dim) != 2L)
                  stop(gettextf("'%s' is not a matrix", "object"),
                       domain = NA)
              if ((n <- d[1L]) != d[2L])
                  return(FALSE)
              op <- if (trans == "C" && any(flintClass(object) == c("acf", "acb")))
                        Conj
                    else identity
              if (n > 1L && length(tol1))
              for (i in c(1L, 2L, if (n > 3L) n - 1L, if (n > 2L) n))
                  if (is.character(all.equal(object[, i], op(t(object[i, ])),
                                             tolerance = tol1, ...)))
                      return(FALSE)
              !is.character(all.equal(object, op(t(object)),
                                      tolerance = tol, ...))
          })

.kronecker <- updateBody(.kronecker, as.array = asArray)
environment(.kronecker) <- environment()

setMethod("kronecker",
          c(X =   "ANY", Y = "flint"),
          .kronecker)

setMethod("kronecker",
          c(X = "flint", Y =   "ANY"),
          .kronecker)

setMethod("kronecker",
          c(X = "flint", Y = "flint"),
          .kronecker)

rm(.kronecker)

setMethod("length",
          c(x = "flint"),
          function (x)
              flintLength(x, exact = FALSE))

setMethod("length<-",
          c(x = "flint"),
          function (x, value)
              .Call(R_flint_length_assign, x, as(value, "ulong")))

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
              format(x, base = 62L, digits = 0L, digits.mag = 0L))

setMethod("names",
          c(x = "flint"),
          function (x)
              x@names)

setMethod("names<-",
          c(x = "flint", value = "NULL"),
          function (x, value) {
              if (!is.null(x@names))
                  x@names <- NULL
              x
          })

setMethod("names<-",
          c(x = "flint", value = "character"),
          function (x, value) {
              nx <- length(x)
              if (nx > 0x1p+52) {
                  warning(gettextf("length of '%s' [%s] exceeds maximum length of character vector [%.0f]; '%s' assignment is no-op",
                                   "x", format(flintLength(x)), 0x1p+52, "names"),
                          domain = NA)
                  return(x)
              }
              nv <- length(value)
              x@names <-
              if (nv == nx)
                  as.character(value)
              else {
                  if (nv > nx)
                  stop(gettextf("length of '%s' [%.0f] exceeds length of '%s' [%.0f]",
                                "value", nv, "x", nx),
                       domain = NA)
                  c(as.character(value), character(nx - nv))
              }
              x
          })

setMethod("norm",
          c(x = "flint", type = "ANY"),
          function (x, type, ...) {
              if (length(x@dim) != 2L)
                  stop(gettextf("'%s' is not a matrix", "x"),
                       domain = NA)
              type <- substr(type, 1L, 1L)
              norm0 <-
              function (x, value) {
                  if (length(x))
                      value
                  else flint(flintClass(abs(x)), length = 1L) # zero
              }
              switch(EXPR = type,
                     "1" =,
                     "O" =, "o" = norm0(x, max(colSums(abs(x)))),
                     "I" =, "i" = norm0(x, max(rowSums(abs(x)))),
                     "F" =, "f" =,
                     "E" =, "e" = sqrt(sum(x * x)),
                     "M" =, "m" = norm0(x, max(abs(x))),
                     "2" = norm0(x, svd(x, nu = 0L, nv = 0L)$d[1L]),
                     stop(gettextf("norm type \"%s\" is invalid", type),
                          domain = NA))
          })

## The method with signature c(x = "ANY", type = "missing") in package
## 'methods' seems to call base::norm, not methods:::.implicitTable$norm
setMethod("norm",
          c(x = "flint", type = "missing"),
          function (x, type, ...)
              norm(x, type = "O", ...))

.outer <- updateBody(outer, as.vector = asVector)
environment(.outer) <- environment()

setMethod("outer",
          c(X =   "ANY", Y = "flint"),
          .outer)

setMethod("outer",
          c(X = "flint", Y =   "ANY"),
          .outer)

setMethod("outer",
          c(X = "flint", Y = "flint"),
          .outer)

rm(.outer)

setMethod("print",
          c(x = "flint"),
          function (x, base = 10L, digits = NULL, digits.mag = NULL,
                    max = NULL, Rdiff = NULL, ...) {
              s <- flintTriple(x)
              if (is.null(Rdiff))
                  Rdiff <- getOption("flint.Rdiff", FALSE)
              Rdiff <- as.logical(Rdiff)
              if (Rdiff)
                  s[3L] <- paste0("<pointer: ", s[3L], ">")
              msg <-
              if (is.null(d <- x@dim))
              gettextf("class \"%s\", length %s, address %s",
                       s[1L], s[2L], s[3L])
              else {
              nd <- length(d)
              s[2L] <- paste(if (nd <= 4L) d else c(d[1L:4L], "..."),
                             collapse = ",")
              gettextf("class \"%s\", dim (%s), address %s",
                       s[1L], s[2L], s[3L])
              }
              cat(msg, "\n", sep = "")
              n <- flintLength(x)
              if (any(if (is.null(d)) n else d)) {
              if (is.null(max))
                  max <- getOption("max.print")
              max <- as.integer(max)
              if (max < 0L)
                  max <- 0L
              msg <-
              if (n <= max) {
                  y <- x
                  NULL
              }
              else if (is.null(d) || nd == 1L) {
                  ## Omit entries
                  y <- x[seq_len(max)]
                  gettextf(" [ reached '%s', omitted trailing %.0f entries ]",
                           "max",
                           as.double(n - ulong(max)))
              }
              else if (max < d[2L]) {
                  ## Omit pages then rows then columns
                  args <- rep(list(x, 1L, seq_len(max), 1L, drop = FALSE),
                              c(1L, 1L, 1L, nd - 2L, 1L))
                  y <- do.call(`[`, args)
                  if (nd == 2L)
                  gettextf(" [ reached '%s', omitted trailing %d rows, %d columns ]",
                           "max",
                           d[1L] - 1L, d[2L] - max)
                  else
                  gettextf(" [ reached '%s', omitted trailing (%s) indices ]",
                           "max",
                           paste(d - rep(c(1L, max, 1L), c(1L, 1L, nd - 2L)),
                                 collapse = ","))
              }
              else if (max %/% d[2L] < d[1L]) {
                  ## Omit pages then rows
                  k <- max %/% d[2L]
                  args <- rep(list(x, seq_len(k), substitute(), 1L, drop = FALSE),
                              c(1L, 1L, 1L, nd - 2L, 1L))
                  y <- do.call(`[`, args)
                  if (nd == 2L)
                  gettextf(" [ reached '%s', omitted trailing %d rows ]",
                           "max",
                           d[1L] - k)
                  else
                  gettextf(" [ reached '%s', omitted trailing (%s) indices ]",
                           "max",
                           paste(d - rep(c(k, d[2L], 1L), c(1L, 1L, nd - 2L)),
                                 collapse = ","))
              }
              else {
                  ## Omit pages
                  p <- cumprod(ulong(d))
                  w <- which(p > ulong(max)); w. <- w[1L]
                  k <- as.integer(max %/% p[[w. - 1L]])
                  args <- rep(list(x, substitute(), seq_len(k), 1L, drop = FALSE),
                              c(1L, w. - 1L, 1L, nd - w., 1L))
                  y <- do.call(`[`, args)
                  gettextf(" [ reached '%s', omitted trailing (%s) indices ]",
                           "max",
                           paste(d - rep(c(0L, k, 1L), c(w. - 1L, 1L, nd - w.)),
                                 collapse = ","))
              }
              print.default(format(y, base = base, digits = digits, digits.mag = digits.mag),
                            quote = FALSE, right = TRUE, max = max, ...)
              if (!is.null(msg))
                  cat(msg, "\n", sep = "")
              }
              invisible(x)
          })

setMethod("qr",
          c(x = "flint"),
          function (x, ...)
              .NotYetImplemented())

setMethod("quantile",
          c(x = "flint"),
          function (x, probs = fmpq(num = 0L:4L, den = 4L), type = 7L,
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
                          nppm <- nppm - fmpq(num = 1L, den = 2L)
                      j <- floor(nppm)
                      h <- switch(type,
                                  nppm > j,
                                  fmpq(num = (nppm > j) + 1L, den = 2L),
                                  nppm != j | j%%2L == 1L)
                  } else {
                      switch(type - 3L,
                             {
                                 a <- 0L
                                 b <- 1L
                             },
                             a <- b <- fmpq(num = 1L, den = 2L),
                             a <- b <- 0L,
                             a <- b <- 1L,
                             a <- b <- fmpq(num = 1L, den = 3L),
                             a <- b <- fmpq(num = 3L, den = 8L))
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

rbind.flint <-
function (..., deparse.level = 1) {
    if (nargs() == 0L)
        return(NULL)
    args <- list(...)
    classes <- vapply(args, .bind.class, "")
    common <- flintClassCommon(classes, strict = FALSE)
    if (any(common == c("NULL", "raw", "logical", "integer", "double", "complex")))
        return(cbind(NULL, ..., deparse.level = deparse.level))
    args <- lapply(args, asVector, common, strict = FALSE)
    if (any(common == c("character", "list", "expression")))
        do.call(rbind, c(args, list(deparse.level = deparse.level)))
    else {
        exps <- substitute(list(...))
        .Call(R_flint_bind, 0L, as.integer(deparse.level), args, exps)
    }
}

setMethod("rbind2",
          c(x =   "ANY", y = "flint"),
          function (x, y, ...)
              rbind.flint(x, y, deparse.level = 0L))

setMethod("rbind2",
          c(x = "flint", y =   "ANY"),
          function (x, y, ...)
              rbind.flint(x, y, deparse.level = 0L))

setMethod("rbind2",
          c(x = "flint", y = "flint"),
          function (x, y, ...)
              rbind.flint(x, y, deparse.level = 0L))

setMethod("rcond",
          c(x = "flint"),
          function (x, norm, ...)
              .NotYetImplemented())

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

setMethod("scale",
          c(x = "flint"),
          function (x, center = TRUE, scale = TRUE) {
              x <- asMatrix(x)
              d <- x@dim
              m <- d[1L]
              n <- d[2L]
              if (is.logical(center)) {
                  if (center) {
                      center <- colMeans(x, na.rm = TRUE)
                      x <- x - rep(center, each = m)
                  }
              } else {
                  if (length(center) == n)
                      x <- x - rep(center, each = m)
                  else stop(gettextf("length of '%s' does not equal number of columns of '%s'",
                                     "center", "x"),
                            domain = NA)
              }
              if (is.logical(scale)) {
                  if (scale) {
                      scale <-
                      do.call(c.flint,
                              lapply((0L:(n - 1L)) * m,
                                     function (h) {
                                         y <- if (m) x[(h + 1L):(h + m)] else x[0L]
                                         sqrt(sum(y * y, na.rm = TRUE)/
                                              max(1L, m - sum(is.na(y)) - 1L))
                                     }))
                      x <- x/rep(scale, each = m)
                  }
              } else {
                  if (length(scale) == n)
                      x <- x/rep(scale, each = m)
                  else stop(gettextf("length of '%s' does not equal number of columns of '%s'",
                                     "scale", "x"),
                            domain = NA)
              }
              if (!is.logical(center))
                  attr(x, "scaled:center") <- center
              if (!is.logical(scale))
                  attr(x, "scaled:scale") <- scale
              x
          })

setMethod("seq",
          c("..." = "flint"),
          function (from, to, by, length.out, along.with, ...) {
               isSlongLike <-
               function (x)
                   any(flintClassAny(x) == c("slong", "integer", "logical"))
               maybeSlong <- TRUE
               if (!missing(from)) {
               if (length(from) != 1L)
                   stop(gettextf("length of '%s' is not %d",
                                 "from", 1L),
                        domain = NA)
               else if (!is.finite(from))
                   stop(gettextf("'%s' is not a finite number",
                                 "from"),
                        domain = NA)
               maybeSlong <- maybeSlong && isSlongLike(from)
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
               maybeSlong <- maybeSlong && isSlongLike(to)
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
               maybeSlong <- maybeSlong && isSlongLike(by)
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
               else if (length.out >= ULONG_MAX + 1L)
                   stop(gettextf("value length would exceed maximum 2^%d-1",
                                 flintABI()),
                        domain = NA)
               length.out <- ulong(length.out)
               }
               if (!missing(along.with)) {
               if (!missing(length.out))
                   stop(gettextf("one of '%s' and '%s' must be missing",
                                 "length.out", "along.with"),
                        domain = NA)
               length.out <- flintLengthAny(along.with)
               }
               seqUlong <-
               function (from, length.out, reverse = FALSE)
                   .Call(R_flint_ulong_seq, ulong(from), ulong(length.out), reverse)
               ans <-
               switch(nargs() - ...length(),
               {
                   if (missing(length.out))
                       stop(gettextf("usage seq(%s=) is not yet implemented",
                                     if (missing(from)) if (missing(to)) "by" else "to" else "from"),
                            domain = NA)
                   seqUlong(1L, length.out)
               },
               {
                   if (missing(length.out) != missing(by))
                       stop(gettextf("usage seq(%s=, %s=) is not yet implemented",
                                     if (missing(from)) "to" else "from", if (missing(by)) "length.out" else "by"),
                            domain = NA)
                   if (missing(length.out)) {
                       d <- if (from <= to) { op <- `+`; to - from } else { op <- `-`; from - to }
                       length.out <- fmpz(d)
                       if (length.out == d)
                           length.out <- length.out + 1L
                       if (length.out >= ULONG_MAX + 1L)
                           stop(gettextf("value length would exceed maximum 2^%d-1",
                                         flintABI()),
                                domain = NA)
                       op(from, seqUlong(0L, length.out))
                   }
                   else 1L + by * seqUlong(0L, length.out)
               },
               {
                   if (missing(length.out)) {
                       d <- if (from == to) 0L else (to - from)/by
                       length.out <- fmpz(d)
                       if (length.out == d)
                           length.out <- length.out + 1L
                       if (length.out >= ULONG_MAX + 1L)
                           stop(gettextf("value length would exceed maximum 2^%d-1",
                                         flintABI()),
                                domain = NA)
                       from + by * seqUlong(0L, length.out)
                   }
                   else if (missing(by)) {
                       by <- if (length.out <= 1L) 0L else (to - from)/(length.out - 1L)
                       from + by * seqUlong(0L, length.out)
                   }
                   else if (missing(to))
                       from + by * seqUlong(0L, length.out)
                   else to - by * seqUlong(0L, length.out, reverse = TRUE)
               },
               stop(gettextf("usage seq(%s=, %s=, %s=, %s=) is not yet implemented",
                             "from", "to", "by", if (missing(along.with)) "length.out" else "along.with"),
                    domain = NA))
               if (maybeSlong && is(ans, "fmpz") &&
                   (length.out == 0L ||
                    {
                        r <- ans[c.flint(1L, length.out)]
                        all(r >= SLONG_MIN, r <= SLONG_MAX)
                    }))
                   slong(ans)
               else ans
          })

setMethod("sequence",
          c(nvec = "flint"),
          function (nvec, from = ulong(1L), by = ulong(1L), ...) {
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
              if (length(d <- object@dim) == 2L)
                  return(summary(as.data.frame(object), triple = triple, quantile.type = quantile.type, ...))
              if (triple || any(flintClass(object) == c("acf", "arb", "acb")))
                  return(`class<-`(`names<-`(flintTriple(object), c("class", "length", "address")), "noquote"))
              if (anyna <- anyNA(object))
                  object <- object[!(isna <- is.na(object))]
              qq <- quantile(object, type = quantile.type)
              qq <- c(qq[1L:3L], mean(object), qq[4L:5L])
              qq@names <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
              if (anyna) c(qq, "NaN" = sum(isna)) else qq
          })

setMethod("svd",
          c(x = "flint"),
          function (x, nu = min(n, p), nv = min(n, p), LINPACK = FALSE) {
              n <- p <- NULL
              .NotYetImplemented()
          })

setMethod("t",
          c(x = "flint"),
          function (x)
              .Call(R_flint_transpose, x, FALSE))

setMethod("unique",
          c(x = "flint"),
          function (x, incomparables = FALSE, MARGIN = 1L, ...) {
              if (is.null(d <- x@dim) ||
                  !(is.character(MARGIN) || any(MARGIN <- as.integer(MARGIN)))) {
                  if (!isFALSE(incomparables))
                      incomparables <- mtfrm(as(incomparables, flintClass(x)))
                  x[!duplicated.default(mtfrm(x), incomparables = incomparables, ...)]
              } else {
                  if (!isFALSE(incomparables))
                      .NotYetUsed("incomparables")
                  if (length(MARGIN) != 1L)
                      stop(gettextf("length of '%s' is not %d",
                                    "MARGIN", 1L),
                           domain = NA)
                  if (is.character(MARGIN))
                      MARGIN <- match(MARGIN, names(x@dimnames), 0L)
                  args <- rep(c(list(x), list(substitute()), list(drop = FALSE)),
                              c(1L, length(d), 1L))
                  args[[1L + MARGIN]] <- !duplicated.default(asplit3(mtfrm(x), MARGIN = MARGIN, drop = TRUE), ...)
                  do.call(`[`, args)
              }
          })
