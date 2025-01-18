library(flint)

set.seed(0x021936L)
n <- 32L
i <- sample(n)
for (.cl in c("ulong", "slong", "fmpz", "fmpq", "mag", "arf", "acf"))
    stopifnot(identical(xtfrm(new(.cl, x = i)), i))
what <- c("notTotalOrderError", "error", "condition")
for (.cl in c("arb", "acb")) {
    e <- tryCatch(xtfrm(new(.cl, x = i)), condition = identity)
    stopifnot(all(inherits(e, what, which = TRUE)))
}

set.seed(0x022020L)
n <- 32L
x <- sample(n, n * 4L, replace = TRUE)
x[sample(n * 4L, n)] <- NA
o <- order(x)
xo <- x[o]
xou <- unique(xo)
y <- .arf(x = x)
yo <- .arf(x = xo)
you <- .arf(x = xou)
stopifnot(identical(order(xtfrm(y)), o),
          identical(order(y), o),
          flintIdentical(sort(y, na.last = TRUE), yo),
                is.unsorted(y                                 ) ,
          is.na(is.unsorted(yo                                )),
          is.na(is.unsorted(you                               )),
                is.unsorted(y  , na.rm = TRUE                 ) ,
              ! is.unsorted(yo , na.rm = TRUE                 ) ,
              ! is.unsorted(you, na.rm = TRUE                 ) ,
                is.unsorted(y  ,               strictly = TRUE) ,
                is.unsorted(yo ,               strictly = TRUE) ,
          is.na(is.unsorted(you,               strictly = TRUE)),
                is.unsorted(y  , na.rm = TRUE, strictly = TRUE) ,
                is.unsorted(yo , na.rm = TRUE, strictly = TRUE) ,
              ! is.unsorted(you, na.rm = TRUE, strictly = TRUE) )

s <- c(NaN, Inf, 1, 0)
x <- complex(     real = rep(s,  each = 4L),
             imaginary = rep(s, times = 4L))
y <- .acf(x = x)
stopifnot(identical(xtfrm(x), xtfrm(y)))
