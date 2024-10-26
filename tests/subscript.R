library(methods) # is, new
loadNamespace("flint") # work with or without attaching


x0 <- new("slong")[0L] # signaled wrong OOB error
stopifnot(is(x0, "slong"), length(x0) == 0L)
