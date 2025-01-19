library(flint)
options(flint.Rdiff = TRUE)

(L <- sapply(c("ulong", "slong", "fmpz", "fmpq", "mag", "arf", "acf",
              "arb", "acb"),
             new, x = seq_len(6L), simplify = FALSE))
(D <- as.data.frame(L))
stopifnot(identical(as.list(D), L),
          identical(do.call(data.frame, L), D))

(L. <- lapply(L, as.data.frame, nm = "?"))
(D. <- do.call(cbind, L.))
stopifnot(identical(D., `names<-`(D, rep("?", length(D)))))

## Complex.data.frame does not exist
try(Conj(D))

## Math.data.frame is not sufficiently generic
try(abs(D))

## Ops.data.frame seems to work
D  + D
D  & D
D == D

## Summary.data.frame is not sufficiently generic
try(sum(D))
