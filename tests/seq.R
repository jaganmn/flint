library(flint)

args. <- list(from = 3L, to = -4L, by = -0.5, length.out = 8L)
args  <- mapply(as, args., c("slong", "slong", "fmpq", "ulong"),
                SIMPLIFY = FALSE)

usage <- list(c(                    "length.out"),
              c("from", "to"                    ),
              c(              "by", "length.out"),
              c("from", "to", "by"              ),
              c("from", "to",       "length.out"),
              c("from",       "by", "length.out"),
              c(        "to", "by", "length.out"))
value <- rep(c("ulong", "fmpz", "fmpq"), c(1L, 1L, 5L))

for (i in seq_along(usage)) {
    u <- usage[[i]]
    v <- value[[i]]
    s <- flint(v, do.call(seq.int, args.[u]))
    stopifnot(flintIdentical(do.call(seq.int, args[u]), s),
              flintIdentical(do.call(seq    , args[u]), s))
}

d <- 4L
nvec. <- d:1L
from. <- seq.int(from = 1L, by = d + 1L, length.out = d)
s <- ulong(sequence(nvec = nvec., from = from.))
stopifnot(flintIdentical(sequence(nvec = ulong(nvec.),
                                  from = ulong(from.)),
                         s),
          all(s == which(.row(c(d, d)) >= .col(c(d, d)))))
