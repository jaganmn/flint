library(methods)
p <- asNamespace("flint")
f <- getGenerics(p)
l <- sapply(f, testInheritedMethods, simplify = FALSE)
(nsig <- lengths(lapply(l, slot, "allSelections")))
(namb <- lengths(lapply(l, slot, "target")))
## Tolerate methods for '[<-', '[[<-' with signature x="data.frame"
## defined in package 'methods'
`%notin%` <- function (x, table) match(x, table, nomatch = 0L) == 0L
namb.. <- namb[names(namb) %notin% c("[<-", "[[<-")]
stopifnot(all(namb.. == 0L),
          all(startsWith(l[[ "[<-"]]@target, "data.frame#")),
          all(startsWith(l[["[[<-"]]@target, "data.frame#")))
