.rk.func <-
function (func) {
    if (!is.function(func) || is.primitive(func))
        stop(gettextf("'%s' is not a closure",
                      "func"),
             domain = NA)
    nf <- length(f <- formals(func))
    if (any((if (nf <= 4L) names(f) else names(f)[seq_len(4L)]) == "..."))
        stop(gettextf("'%s' has '%s' among first four formal arguments",
                      "func", "..."),
             domain = NA)
    if (nf < 4L)
        formals(func) <- c(f, alist(.__1__. =, .__2__. =, .__3__. =, .__4__. =)[seq.int(to = 4L, by = 1L, length.out = 4L - nf)])
    func
}

.rk.method <-
function (a, b, bb = NULL, c, p, prec = flintPrec()) {

    fmpq <- getClass("fmpq")
    arf <- getClass("arf")
    isNumber <-
    function (x)
        is.numeric(x) || is(x, fmpq) || is(x, arf)
    isSumOne <-
    function (x) {
        if (is.double(x) || is(x, arf))
            abs(sum(x) - 1L) < 2L^(1L-fmpz(prec))
        else sum(x) == 1L
    }
    stopifnot(is.integer(p) && length(p) == 1L && p >= 1L,
              isNumber(c) && (d <- length(c)) >= p && c[[1L]] == 0L,
              is.null(bb) ||
                  (isNumber(bb) && length(bb) == d && isSumOne(bb)),
              isNumber(b) && length(b) == d && isSumOne(b),
              isNumber(a) && length(a) == (d * (d - 1L)) %/% 2L)
    list(a = arf(a), b = arf(b), bb = if (!is.null(bb)) arf(bb),
         c = arf(c), d = d, p = p)
}

.rk.method.dormand.prince <-
function () {
    a  <- fmpq(num = c(1L,
                       3L, 9L,
                       44L, -56L, 32L,
                       19372L, -25360L, 64448L, -212L,
                       9017L, -355L, 46732L, 49L, -5103L,
                       35L, 0L, 500L, 125L, -2187L, 11L),
               den = c(5L,
                       40L, 40L,
                       45L, 15L, 9L,
                       6561L, 2187L, 6561L, 729L,
                       3168L, 33L, 5247L, 176L, 18656L,
                       384L, 1L, 1113L, 192L, 6784L, 84L))
    b  <- fmpq(num = c(35L, 0L, 500L, 125L, -2187L, 11L, 0L),
               den = c(384L, 1L, 1113L, 192L, 6784L, 84L, 1L))
    bb <- fmpq(num = c(5179L, 0L, 7571L, 393L, -92097L, 187L, 1L),
               den = c(57600L, 1L, 16695L, 640L, 339200L, 2100L, 40L))
    c  <- fmpq(num = c(0L, 1L, 3L, 4L, 8L, 1L, 1L),
               den = c(1L, 5L, 10L, 5L, 9L, 1L, 1L))
    p  <- 4L
    list(a = a, b = b, bb = bb, c = c, p = p)
}

arf_rk <-
function (func, t, y0, param = NULL, rtol = NULL, atol = NULL,
          hmin = 0, hmax = Inf, hini = NULL, smax = NULL,
          method = .rk.method.dormand.prince(), progress = 0L,
          prec = flintPrec()) {
    prec. <- flintPrec(prec)
    on.exit(flintPrec(prec.))
    res <- list(t = flintNew("arf"), y = flintNew("arf"))
    .Call(R_flint_arf_calc_rk, res,
          .rk.func(func), as(t, "arf"), as(y0, "arf"), param,
          if (!is.null(rtol)) as(rtol, "arf"),
          if (!is.null(atol)) as(atol, "arf"),
                              as(hmin, "arf"),
                              as(hmax, "arf"),
          if (!is.null(hini)) as(hini, "arf"),
          if (!is.null(smax)) as(smax, "ulong"),
          do.call(.rk.method, method), as.integer(progress), prec)
    res
}
