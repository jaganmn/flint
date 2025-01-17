library(flint)
options(flint.Rdiff = TRUE)

p <- cumprod(rep(.ulong(x = c(1L, 2L)), c(1L, 30L)))

vv <- withVisible(print(p))
stopifnot(identical(vv$value,    p), identical(vv$visible, FALSE))
vv <- withVisible( show(p))
stopifnot(identical(vv$value, NULL), identical(vv$visible, FALSE))

x <- c(-rev(p), 0L, p)
as(x, "slong")
as(x,  "fmpz")
as(x,  "fmpq")
as(x,   "mag")
as(x,   "arf")
as(x,   "acf")
as(x,   "arb")
as(x,   "acb")

prec <- cumprod(rep(c(1, 2), c(1L, 15L)))
e... <- arb_const_e(prec)
print(e..., digits = 32L)
