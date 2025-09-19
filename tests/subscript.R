library(flint)

zapnames <-
function (x) {
    is.empty <- function (.) !is.null(.) && length(.) == 0L
    if (is.empty(names(x)))
        names(x) <- NULL
    if (!is.null(a <- dimnames(x))) {
        if (any(e <- vapply(a, is.empty, FALSE)))
            a[e] <- list(NULL)
        dimnames(x) <-
            if (is.null(names(a)) && all(vapply(a, is.null, FALSE)))
                NULL
            else a
    }
    x
}

zapidentical <-
function (x, y, ...)
    identical(zapnames(x), zapnames(y), ...)

ok0 <-
function (...)
    tryCatch({ x[ ... ]; x.[ ... ]; FALSE }, error = function (e) TRUE) &&
    tryCatch({ x[[...]]; x.[[...]]; FALSE }, error = function (e) TRUE)
ok1 <-
function (...)
    zapidentical(x[ ... ], flint(.cl, x.[ ... ])) &&
    tryCatch({ x[[...]]; x.[[...]]; FALSE }, error = function (e) TRUE)
ok2 <-
function (...)
    tryCatch({ x[ ... ]; x.[ ... ]; FALSE }, error = function (e) TRUE) &&
    zapidentical(x[[...]], flint(.cl, x.[[...]]))
ok3 <-
function (...)
    zapidentical(x[ ... ], flint(.cl, x.[ ... ])) &&
    zapidentical(x[[...]], flint(.cl, x.[[...]]))

for (.cl in c("ulong", "slong", "fmpz", "fmpq", "mag", "arf", "acf",
              "arb", "acb")) {
    for (.n in 0L:2L) {
        x. <- seq_len(.n)
        names(x.) <- letters[seq_len(.n)]
        x <- flint(.cl, x.)
        stopifnot(ok0("?"),
                  ok0(NULL, NULL),
                  ok0(NA),
                  ok0(NA_integer_),
                  ok0(NA_real_),
                  ok0(complex(0L)),
                  ok1(),
                  ok1(NULL),
                  ok1(logical(0L)),
                  ok1(logical(.n)),
                  ok1(logical(.n + 2L)),
                  ok1(integer(0L)),
                  ok1(integer(.n + 2L)),
                  ok1(double(0L)),
                  ok1(double(.n + 2L)),
                  ok1(rep(-0.9, .n + 2L)),
                  ok1(rep( 0.9, .n + 2L)),
                  ok1(character(0L)))
        if (.n >= 1L)
        stopifnot(ok0(c(-1L, 1L)),
                  ok1(c( 1L, 1L)),
                  (if (.n == 1L) ok3 else ok1)(TRUE),
                  ok1(c(TRUE, logical(.n + 1L))),
                  ok3(1L),
                  ok1(c(1L, integer(.n + 1L))),
                  ok3(1),
                  ok1(c(1, double(.n + 1L))),
                  ok3(1.9),
                  ok1(c(1.9, rep(-0.9, .n + 1L))),
                  ok1(c(1.9, rep( 0.9, .n + 1L))),
                  ok3(names(x.)[1L]),
                  (if (.n == 2L) ok3 else ok1)(-.n))
        else
        stopifnot(ok0(TRUE),
                  ok0(1L),
                  ok0(1),
                  ok0(-1L))
        if (.n >= 2L)
        stopifnot(ok1(c(TRUE, TRUE, logical(.n))),
                  ok1(c(1L, 2L, integer(.n))),
                  ok1(c(1, 2, double(.n))),
                  ok1(c(1.9, 2.9, rep(-0.9, .n))),
                  ok1(c(1.9, 2.9, rep( 0.9, .n))),
                  ok1(names(x.)[c(1L, 2L)]),
                  ok1(-c(.n, .n - 1L)),
                  ok3(-seq_len(.n)[-1L]))
        else
        stopifnot(ok0(c(FALSE, TRUE)),
                  ok0(2L),
                  ok0(2),
                  ok0(-2L))
    }
}
