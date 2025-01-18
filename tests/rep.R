library(flint)

n <- 4L
x. <- seq_len(n)
names(x.) <- letters[seq_len(n)]

for (.cl in c("ulong", "slong", "fmpz", "fmpq", "mag", "arf", "acf",
              "arb", "acb")) {
    x <- new(.cl, x = x.)
    stopifnot(identical(names(x), names(x.)),
              flintIdentical(new(.cl, x = rep(x., each = 2L)),
                                          rep(x , each = 2L) ),
              flintIdentical(new(.cl, x = rep(x., times = 2L)),
                                          rep(x , times = 2L) ),
              flintIdentical(new(.cl, x = rep(x., times = x.)),
                                          rep(x , times = x.) ),
              flintIdentical(new(.cl, x = rep(x., length.out = 9L)),
                                          rep(x , length.out = 9L) ),
              flintIdentical(new(.cl, x = rep.int(x., 2L)),
                                          rep.int(x , 2L) ),
              flintIdentical(new(.cl, x = rep.int(x., x.)),
                                          rep.int(x , x.) ),
              flintIdentical(new(.cl, x = rep_len(x., 9L)),
                                          rep_len(x , 9L) ))
}
