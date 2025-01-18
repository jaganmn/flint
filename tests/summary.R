library(flint)

x. <- as.integer(cumprod(1:11))
for (.cl in c("ulong", "slong", "fmpz", "fmpq", "mag", "arf", "acf",
              "arb", "acb")) {
    x <- new(.cl, x = x.)
    s <- structure(flintTriple(x),
                   names = c("class", "length", "address"),
                   class = "noquote")
    ## summary
    stopifnot(identical(summary(x, triple = TRUE), s))
    if (any(.cl == c("arb", "acb", "acf")))
    stopifnot(identical(summary(x), s))
    else
    stopifnot(all.equal(summary(x), summary(x.), check.class = FALSE))
    ## quantile
    if (any(.cl == c("arb", "acb")))
    stopifnot(tryCatch(as.null(quantile(x)),
                       notTotalOrderError = function (e) TRUE))
    else
    for (.qt in 1:9)
    stopifnot(all.equal(quantile(x , type = .qt),
                        quantile(x., type = .qt, names = FALSE),
                        check.class = FALSE))
    ## mean
    stopifnot(all.equal(mean(x), mean(x.), check.class = FALSE))
}
