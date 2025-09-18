P <- arb(pi)
stopifnot(flintIdentical(Mid(P), arf(pi)))
P2 <- P; Mid(P2) <- 2*pi # gave error
## Error in q@dim :
##   no applicable method for `@` applied to an object of class "function"
stopifnot(P2 == 2*pi)


Z <- acb(pi)
Imag(Z) <- 1 # gave error
## Error in q@dim :
##   no applicable method for `@` applied to an object of class "function"
stopifnot(Z == pi + 1i)


(mz <- Mid(Z)) # newly works
(rz <- Rad(Z))
stopifnot(inherits(mz, "acf"), mz == pi + 1i)
