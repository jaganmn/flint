library(flint)


x0 <- .slong(0L)[0L] # signaled wrong OOB error
stopifnot(flintIdentical(x0, .slong(0L)),
          is(x0, "slong"), length(x0) == 0L)
