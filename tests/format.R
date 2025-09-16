library(flint)

p. <- c(0L, as.integer(cumprod(rep(c(1, 2), c(1L, 30L)))))
x. <- c(-rev(p.), p.)

stopifnot(identical(format(ulong(p.)),        format(p.)       ),
          identical(format(slong(x.)),        format(x.)       ),
          identical(format( fmpz(x.)),        format(x.)       ),
          identical(format( fmpq(x.)), paste0(format(x.), "/1")))

stopifnot(identical(format(fmpq(num = c(-1L, -11L), den = c(11L, 1L))),
                    c(" -1/11", "-11/ 1")),
          identical(format(fmpq(c(-0x1p-12, 0x1p-6))),
                    c("-1/4096", " 1/  64")))

f10 <- function (b) format(slong(b), base = abs(b))
s <- c(0:9, LETTERS, letters); ms <- c(" 0", paste0("-", s[-1L]))
stopifnot(all(vapply(  2:62 , f10, "") ==  "10"),
          all(vapply(-(2:62), f10, "") == "-10"),
          identical(format(slong(  0:61 ), base = 62L),  s),
          identical(format(slong(-(0:61)), base = 62L), ms))

## TODO: test floating-point types after deciding about trailing zeros
