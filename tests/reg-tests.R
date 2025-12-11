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


## all.equal(check.class = FALSE) failed to detect numerical differences
stopifnot(is.character(all.equal(0, arf(1), check.class = FALSE)))


## tcrossprod(<vector>, <vector>) determined inner dimension incorrectly
stopifnot(identical(fmpz(tcrossprod(1:6, 1:8)),
                    ## below gave "non-conformable arguments":
                    tcrossprod(fmpz(1:6), fmpz(1:8))),
          identical(fmpq(tcrossprod(1:6, 1:6)),
                    ## below could trigger a segfault:
                    tcrossprod(fmpq(1:6), fmpq(1:6))))


## names(c(...)) was constructed incorrectly
x <- 1L
y <- 1L:2L
z <- `names<-`(1L:3L, c("z", "zz", "zzz"))
stopifnot(identical(c(slong(), a = x, b = y, c = z),
                    slong(c(a = x, b = y, c = z))))


## Arg(x) gave sign(x) * pi for 'x' of class 'mag', 'arf', 'arb'
stopifnot(all.equal(Arg(mag(c(    0, 1))), mag(c(    0, 0))),
          all.equal(Arg(arf(c(-1, 0, 1))), arf(c(pi, 0, 0))),
          all.equal(Arg(arb(c(-1, 0, 1))), arb(c(pi, 0, 0))))


## 0/0, Inf/Inf returned Inf for operands of class 'mag'
stopifnot(tryCatch(mag(  0)/mag(  0), error = function (e) TRUE),
          tryCatch(mag(Inf)/mag(Inf), error = function (e) TRUE))


## diag(x) <- value assigned to entries in the first row
x <- slong.array(0L, c(4L, 6L))
diag(x) <- slong(1L)
stopifnot(identical(print(x), slong(diag(1L, 4L, 6L))))
