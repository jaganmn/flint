library(flint)
options(flint.Rdiff = TRUE)

p. <- c(0L, as.integer(cumprod(rep(c(1, 2), c(1L, 30L)))))
p <- .ulong(x = p.)

vv <- withVisible(print(p))
stopifnot(identical(vv$value,    p), identical(vv$visible, FALSE))
vv <- withVisible( show(p))
stopifnot(identical(vv$value, NULL), identical(vv$visible, FALSE))

x. <- c(-rev(p.), p.)
for (.cl in c("slong", "fmpz", "fmpq", "mag", "arf", "acf",
              "arb", "acb"))
    print(new(.cl, x = x.))

prec <- as.integer(cumprod(rep(c(1, 2), c(1L, 15L))))
e... <- arb_const_e(prec)
print(e..., digits = 32L)
