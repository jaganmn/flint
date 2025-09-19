library(flint)


X <- arb(pi)
stopifnot(flintIdentical(Mid(X), arf(pi)))
X2 <- X; Mid(X2) <- 2 * pi # gave error
## Error in q@dim :
##   no applicable method for `@` applied to an object of class "function"
stopifnot(X2 == 2 * pi)


Z <- acb(pi)
Imag(Z) <- 1 # gave error
## Error in q@dim :
##   no applicable method for `@` applied to an object of class "function"
stopifnot(Z == pi + 1i)
