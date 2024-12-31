.subscript.class <-
function (i)
    switch(type. <- typeof(i),
           "NULL" =, "logical" =, "integer" =, "double" =, "character" =
               if (anyNA(i))
                   stop("subscript NA not supported")
               else type.,
           "S4" =
               if (is.na(class. <- flintClass(i)) ||
                   any(class. == c("acf", "arb", "acb")))
                   stop(gettextf("invalid subscript class '%s'",
                                 class(i)),
                        domain = NA)
               else class.,
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
                         },
                     "character" =
                         {
                             if (is.null(nms <- names(x)) ||
                                 anyNA(m <- match(i, nms)))
                                 stop("subscript out of bounds")
                             m
                         })
              .Call(R_flint_subscript, x, i, TRUE)
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
                         },
                     "character" =
                         {
                             if (is.null(nms <- names(x)) ||
                                 anyNA(m <- match(i, nms)))
                                 stop("subscript out of bounds")
                             m
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
              common <- flintClassCommon(c(flintClass(x), cv))
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
                         },
                     "character" =
                         {
                             if (is.null(nms <- names(x)) ||
                                 anyNA(m <- match(i, nms)))
                                 stop("subscript out of bounds")
                             m
                         })
              ni <- length(i)
              }
              if (ni < 1L)
                  stop("attempt to select less than one element")
              else if (ni > 1L)
                  stop("attempt to select more than one element")
              .Call(R_flint_subscript, x, i, FALSE)
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
                         },
                     "character" =
                         {
                             if (is.null(nms <- names(x)) ||
                                 anyNA(m <- match(i, nms)))
                                 stop("subscript out of bounds")
                             m
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
              common <- flintClassCommon(c(flintClass(x), cv))
              x <- as(x, common)
              value <- as(value, common)
              .Call(R_flint_subassign, x, i, value)
          })

.all.equal <-
function (target, current,
          tolerance = sqrt(.Machine[["double.eps"]]),
          scale = NULL,
          countEQ = FALSE,
          formatFUN = function(err, what) format(err),
          ...,
          check.attributes = TRUE,
          check.class = TRUE,
          giveErr = FALSE) {
    msg <-
    if (check.attributes) {
        at <- attributes(target)
        ac <- attributes(current)
        if (is.object(target)) {
            at[["class"]] <- NULL
            if (typeof(target) == "S4")
                at[[".xData"]] <- NULL
        }
        if (is.object(current)) {
            ac[["class"]] <- NULL
            if (typeof(current) == "S4")
                ac[[".xData"]] <- NULL
        }
        attr.all.equal(`attributes<-`(0L, at), `attributes<-`(0L, ac),
                       tolerance = tolerance, scale = scale, ...)
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
        common <- flintClassCommon(c(class(target), class(current)))
        if (common == "mag")
            common <- "arf"
        target <- as(target, common)
        current <- as(target, common)
    }
    if (!identical(nt <- length(target), nc <- length(current))) {
        if (is.null(nt.off <- attr(nt, "off")) &&
            is.null(nc.off <- attr(nc, "off")))
        return(c(msg, gettextf("target length is %.0f, current length is %.0f",
                               nt, nc)))
        else
        return(c(msg, gettextf("target length is %.0f+%d, current length is %.0f+%d",
                               nt, if (is.null(nt.off)) 0L else nt.off,
                               nc, if (is.null(nc.off)) 0L else nc.off)))
    }
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
          function (x, ...) format(`names<-`(x, NULL), digits = 15L, rnd = "N"))

setMethod("as.list",
          c(x = "flint"),
          function (x, ...) as.vector(x, "list"))

setMethod("as.expression",
          c(x = "flint"),
          function (x, ...) as.vector(x, "expression"))

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

setMethod("as.data.frame",
          c(x = "flint"),
          as.data.frame.vector)

## MJ: we export this function and curse the author of DispatchAnyOrEval
c.flint <-
function (..., use.names = TRUE) {
    n <- nargs()
    if (n == 0L)
        return(NULL)
    args <- list(...)
    classes <- vapply(args, .c.class, "")
    common <- flintClassCommon(classes, strict = FALSE)
    if (any(common == c("NULL", "raw", "logical", "integer", "double", "complex")))
        return(c(..., use.names = use.names))
    args <- lapply(args, as, common)
    .Call(R_flint_bind, args, as.logical(use.names))
}

setMethod("c",
          c(x = "flint"),
          function (x, ..., use.names = TRUE)
              c.flint(x, ..., use.names = use.names))

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

setMethod("is.na<-",
          c(x = "flint"),
          function (x, value) {
              x[value] <- switch(flintClass(x), "slong" =, "ulong" =, "fmpz" =, "fmpq" = NA_integer_, "mag" =, "arf" =, "arb" = NA_real_, "acf" =, "acb" = NA_complex_)
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

setMethod("quantile",
          c(x = "flint"),
          function (x, probs = .fmpq(num = 0L:4L, den = 4L), type = 7L,
                    na.rm = FALSE, ...) {
              class. <-
              switch(flintClass(x),
                     "slong" =, "ulong" =, "fmpz" =, "fmpq" = "fmpq",
                     "mag" =, "arf" = "arf",
                     "acf" = "acf",
                     stop(gettextf("'%s' is not a total order on the range of '%s'",
                                   "<=", "arb"),
                          domain = NA))
              if (anyNA(x)) {
                  if (!na.rm)
                      stop(gettextf("'%s' contains NaN and '%s' is FALSE",
                                    "x", "na.rm"),
                           domain = NA)
                  x <- x[!is.na(x)]
              }
              x <- as(x, class.)
              n <- .fmpz(x = length(x))
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

setMethod("show",
          c(object = "flint"),
          function (object) {
              print(object, quote = FALSE)
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
