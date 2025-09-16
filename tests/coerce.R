library(flint)
flint:::.initBasic()


## Test that as(<class>, "<type>") and as.<type>(<class>) work correctly
## for all 'flint' subclasses <class> and all basic vector types <type>.
## Use 0 and 1 as test cases as these are in the range of every class.
## Allow for imprecision in "mag" conversions which are not exact even
## in exactly representable cases.

cl <- c("ulong", "slong", "fmpz", "fmpq", "mag", "arf", "acf",
        "arb", "acb")
cl.complexlike <- cl == "acf" | cl == "acb"
basic <- c("raw", "logical", "integer", "double", "numeric", "complex",
           "vector")
zu <- lapply(cl, flint, c(0, 1))
for (t in basic) {
    as. <- match.fun(paste0("as.", t))
    as.01 <- list(as.(c(0, 1)), as.(c(0, 1)+0i))[1L + cl.complexlike]
    as.zu <- lapply(zu, as, t); as.zu. <- lapply(zu, as.)
    stopifnot(identical(as.zu, as.zu.),
              identical(as.zu[-5L], as.01[-5L]),
              all.equal(as.zu[ 5L], as.01[ 5L]))
}


## Test that typeof(as.vector(.)) is "complex" for 'acf', 'acb'
## and "double" otherwise.

stopifnot(identical(vapply(lapply(zu, as.vector), typeof, ""),
                    c("double", "complex")[1L + cl.complexlike]))


## Test that exactly one condition (a warning) is signaled when
## nonzero imaginary parts are discarded and that no condition is
## signaled otherwise.

zi <- acb(0i)
ui <- acb(1i)
tools::assertError(tools::assertCondition(zi.c <- as.complex(zi)))
tools::assertError(tools::assertCondition(zi.d <- as.double (zi)))
tools::assertError(tools::assertCondition(ui.c <- as.complex(ui)))
             aw <- tools::assertWarning  (ui.d <- as.double (ui))
stopifnot(length(aw) == 1L,
          identical(zi.c, 0i),
          identical(zi.d, 0),
          identical(ui.c, 1i),
          identical(ui.d, 0))
