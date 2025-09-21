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

## FIXME: Bug?
## 'testInheritedMethods' did not catch the following dispatch ambiguity
## in flint version 0.1.0:
##     norm,flint,ANY
##     norm,ANY,missing
## perhaps because 'norm' has an implicit generic function definition
