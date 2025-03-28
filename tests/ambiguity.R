library(methods)
p <- asNamespace("flint")
f <- getGenerics(p)
l <- sapply(f, testInheritedMethods, simplify = FALSE)
(nsig <- lengths(lapply(l, slot, "allSelections")))
(namb <- lengths(lapply(l, slot, "target")))
stopifnot(!any(namb))
