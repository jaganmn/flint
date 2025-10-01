library(flint)


X <- arb(pi)
stopifnot(identical(Mid(X), arf(pi)))
X2 <- X; Mid(X2) <- 2 * pi # gave error in version 0.5.0
## Error in q@dim :
##   no applicable method for `@` applied to an object of class "function"
stopifnot(X2 == 2 * pi)


Z <- acb(pi)
Imag(Z) <- 1 # gave error in version 0.5.0
## Error in q@dim :
##   no applicable method for `@` applied to an object of class "function"
stopifnot(Z == pi + 1i)


(mZ <- Mid(Z)) # newly works
stopifnot(is(mZ, "acf"), mZ == pi + 1i)


## Dispatch ambiguity
nm <- norm(fmpz(matrix(0L, 0L, 0L))) # gave error in version 0.1.0
## Note: method with signature 'ANY#missing' chosen for function 'norm',
##  target signature 'fmpz#missing'.
##  "flint#ANY" would also be valid
## Error in norm(x, type = "O", ...) : invalid 'x': type "S4"
stopifnot(identical(nm, fmpz(0L)))


## Undefined behaviour (divide-by-zero in integer division) in
## colSums(<m-by-0>), rowSums(<0-by-n>), etc.; detected under x86_64
cs <- colSums(fmpq.array(0L, c(2L, 0L)))
rs <- rowSums(fmpq.array(0L, c(0L, 2L)))
stopifnot(identical(cs, fmpq()), identical(rs, fmpq()))


## Loop over 'cbind', 'rbind' *call* did not advance when filling
## dimension names
x <- y <- ulong(0L)
cn <- colnames(cbind(x,     y)) # gave c("x", "x") in version 0.1.0
rn <- rownames(rbind(x, z = y)) # ditto
stopifnot(identical(cn, c("x", "y")),
          identical(rn, c("x", "z")))


## Late assignment in array-handling branch of 'as.data.frame'
J <- diag(ulong(1L), 6L)
L <- as.data.frame(J) # gave error in version 0.1.0
## Error in h(simpleError(msg, call)) :
##   error in evaluating the argument 'i' in selecting a method for function '[': object 'r' not found
L. <- as.data.frame(diag(1L, 6L))
L.[] <- lapply(L., ulong)
stopifnot(all.equal(L, L.)) # 'all.equal' recurses with dispatch


## 'Summary' methods for 'ulong' and 'slong' kept attributes
validObject(u <- sum(ulong(1:2, names = c("a", "b"))))
validObject(s <- range(slong.array(1:2, dim = c(2L, 2L))))
stopifnot(identical(u, ulong(3L)), identical(s, slong(1:2)))
