library(flint)


x0 <- new("slong")[0L] # signaled wrong OOB error
stopifnot(flintIdentical(x0, new("slong")),
          is(x0, "slong"), length(x0) == 0L)
