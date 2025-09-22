library(flint)


X <- arb(pi)
stopifnot(identical(Mid(X), arf(pi)))
X2 <- X; Mid(X2) <- 2 * pi # gave error
## Error in q@dim :
##   no applicable method for `@` applied to an object of class "function"
stopifnot(X2 == 2 * pi)


Z <- acb(pi)
Imag(Z) <- 1 # gave error
## Error in q@dim :
##   no applicable method for `@` applied to an object of class "function"
stopifnot(Z == pi + 1i)


(mZ <- Mid(Z)) # newly works
stopifnot(is(mZ, "acf"), mZ == pi + 1i)


## Dispatch ambiguity in flint version 0.1.0
nm <- norm(fmpz(matrix(0L, 0L, 0L))) # gave error
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
