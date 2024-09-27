library(methods) # as, new
loadNamespace("flint") # work with or without attaching


## Test that as(<class>, "<type>") and as.<type>(<class>) work correctly
## for all 'flint' subclasses <class> and all basic vector types <type>.
## Use 0 and 1 as test cases as these are in the range of every class.
## Allow for imprecision in "mag" conversions which are not exact even
## in exactly representable cases.

cl <- c("slong", "ulong", "fmpz", "fmpq", "arf", "mag", "arb", "acb")
basic <- c("raw", "logical", "integer", "double", "numeric", "complex",
           "list", "expression", "vector")
zu <- lapply(cl, function(s) new(s, x = c(0, 1)))
for (t in basic) {
    as. <- match.fun(paste0("as.", t))
    as.01 <- rep(list(as.(c(0, 1)), as.(c(0, 1)+0i)), c(7L, 1L))
    as.zu <- lapply(zu, as, t); as.zu. <- lapply(zu, as.)
    stopifnot(identical(as.zu, as.zu.),
              identical(as.zu[-6L], as.01[-6L]),
              all.equal(as.zu[ 6L], as.01[ 6L]))
}


## Test that typeof(as.vector(.)) is "complex" for 'acb', "double" otherwise.

stopifnot(identical(vapply(lapply(zu, as.vector), typeof, ""),
                    rep(c("double", "complex"), c(7L, 1L))))


## Test that as(<class>, <nclass>) works correctly.

ncl <- paste0("n", cl)
nzu <- .mapply(as, list(zu, ncl), NULL)
stopifnot(identical(nzu[-6L], list(new("nslong", c(0L, 1L)),
                                   new("nulong", c(0L, 1L)),
                                   new("nfmpz" , c(0L, 1L)),
                                   new("nfmpq" ,
                                       num = new("nfmpz", c(0L, 1L)),
                                       den = new("nfmpz", c(1L, 1L))),
                                   new("narf"  , c(0, 1)),
                                   new("narb"  ,
                                       mid = new("narf", c(0, 1)),
                                       rad = new("nmag", c(0, 0))),
                                   new("nacb",
                                       real = new("narb",
                                                  mid = new("narf", c(0, 1)),
                                                  rad = new("nmag", c(0, 0))),
                                       imag = new("narb",
                                                  mid = new("narf", c(0, 0)),
                                                  rad = new("nmag", c(0, 0)))))),
          all.equal(nzu[ 6L], list(new("nmag", c(0, 1)))))


## Test that as(<nclass>, <class>) works correctly.

nzu. <- .mapply(as, list(.mapply(as, list(nzu, cl), NULL), ncl), NULL)
stopifnot(identical(nzu[-6L], nzu.[-6L]),
          all.equal(nzu[ 6L], nzu.[ 6L]))


## Test that exactly one condition (a warning) is signaled when nonzero
## imaginary parts are discarded and that no condition is signaled otherwise.

zi <- new("acb", x = 0i)
ui <- new("acb", x = 1i)
tools::assertError(tools::assertCondition(zi.c <- as.complex(zi)))
tools::assertError(tools::assertCondition(zi.d <- as.double (zi)))
tools::assertError(tools::assertCondition(ui.c <- as.complex(ui)))
             aw <- tools::assertWarning  (ui.d <- as.double (ui))
stopifnot(length(aw) == 1L,
          identical(zi.c, 0i),
          identical(zi.d, 0),
          identical(ui.c, 1i),
          identical(ui.d, 0))
