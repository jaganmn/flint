library(flint)


## Test that as(<class>, "<type>") and as.<type>(<class>) work correctly
## for all 'flint' subclasses <class> and all basic vector types <type>.
## Use 0 and 1 as test cases as these are in the range of every class.
## Allow for imprecision in "mag" conversions which are not exact even
## in exactly representable cases.

cl <- c("slong", "ulong", "fmpz", "fmpq", "arf", "mag", "arb", "acb")
basic <- c("raw", "logical", "integer", "double", "numeric", "complex",
           "list", "expression", "vector")
zu <- lapply(cl, function (s) new(s, x = c(0, 1)))
for (t in basic) {
    as. <- match.fun(paste0("as.", t))
    as.01 <- rep(list(as.(c(0, 1)), as.(c(0, 1)+0i)), c(7L, 1L))
    as.zu <- lapply(zu, as, t); as.zu. <- lapply(zu, as.)
    stopifnot(identical(as.zu, as.zu.),
              identical(as.zu[-6L], as.01[-6L]),
              all.equal(as.zu[ 6L], as.01[ 6L]))
}


## Test that typeof(as.vector(.)) is "complex" for 'acb', "double"
## otherwise.

stopifnot(identical(vapply(lapply(zu, as.vector), typeof, ""),
                    rep(c("double", "complex"), c(7L, 1L))))


## Test that exactly one condition (a warning) is signaled when
## nonzero imaginary parts are discarded and that no condition is
## signaled otherwise.

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
