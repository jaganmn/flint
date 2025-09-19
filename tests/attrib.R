library(flint)

n <- 4L
x. <- y. <- z. <- seq_len(n)
names(y.) <- letters[    seq_len(n)]
names(z.) <- letters[n + seq_len(n)]
x <- slong(x.)
y <- slong(y.)
z <- slong(z.)

stopifnot(identical(length(x), n),
          identical(`length<-`(x, 1L), slong(x.[1L])),
          identical(`length<-`(y, 1L), slong(y.[1L])),
          identical(`length<-`(x, n + n), slong(c(x., integer(n)))),
          identical(`length<-`(y, n + n), slong(c(y., integer(n)))),
          identical(x@names, NULL),
          identical(names(x), NULL),
          identical(y@names, names(y.)),
          identical(names(y), names(y.)),
          identical(`names<-`(x, NULL), x),
          identical(`names<-`(y, NULL), x),
          identical(`names<-`(x, names(z.)), z),
          identical(`names<-`(y, names(z.)), z),
          identical(`names<-`(x, character(0L)),
                    `names<-`(x, character(n))),
          identical(`names<-`(x, "."),
                    `names<-`(x, c(".", character(n - 1L)))))
