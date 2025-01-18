library(flint)

n <- 4L
x. <- y. <- z. <- seq_len(n)
names(y.) <- letters[    seq_len(n)]
names(z.) <- letters[n + seq_len(n)]
x <- .slong(x = x.)
y <- .slong(x = y.)
z <- .slong(x = z.)

stopifnot(identical(length(x), n),
          flintIdentical(`length<-`(x, 1L),
                         .slong(x = x.[1L])),
          flintIdentical(`length<-`(y, 1L),
                         .slong(x = y.[1L])),
          flintIdentical(`length<-`(x, n + n),
                         .slong(x = c(x., integer(n)))),
          flintIdentical(`length<-`(y, n + n),
                         .slong(x = c(y., integer(n)))),
          identical(x@names, character(0L)),
          identical(names(x), NULL),
          identical(y@names, names(y.)),
          identical(names(y), names(y.)),
          flintIdentical(`names<-`(x, NULL), x),
          flintIdentical(`names<-`(y, NULL), x),
          flintIdentical(`names<-`(x, names(z.)), z),
          flintIdentical(`names<-`(y, names(z.)), z),
          flintIdentical(`names<-`(x, character(0L)),
                         `names<-`(x, character(n))),
          flintIdentical(`names<-`(x, "."),
                         `names<-`(x, c(".", character(n - 1L)))))
