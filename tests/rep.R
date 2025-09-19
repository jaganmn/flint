library(flint)

n <- 4L
x. <- seq_len(n)
names(x.) <- letters[seq_len(n)]

for (.cl in c("ulong", "slong", "fmpz", "fmpq", "mag", "arf", "acf",
              "arb", "acb")) {
    x <- flint(.cl, x.)
    stopifnot(identical(names(x), names(x.)),
              identical(flint(.cl, rep(x., each = 2L)),
                                   rep(x , each = 2L) ),
              identical(flint(.cl, rep(x., times = 2L)),
                                   rep(x , times = 2L) ),
              identical(flint(.cl, rep(x., times = x.)),
                                   rep(x , times = x.) ),
              identical(flint(.cl, rep(x., length.out = 9L)),
                                   rep(x , length.out = 9L) ),
              identical(flint(.cl, rep.int(x., 2L)),
                                   rep.int(x , 2L) ),
              identical(flint(.cl, rep.int(x., x.)),
                                   rep.int(x , x.) ),
              identical(flint(.cl, rep_len(x., 9L)),
                                   rep_len(x , 9L) ))
}
