.asVector <-
function (x, mode = "any", strict = TRUE) {
    y <- as.vector(x, mode)
    if (strict)
        attributes(y) <- NULL
    else {
        if (is.null(dim(y)) && !is.null(a <- dim(x))) {
            dim(y) <- a
            if (!is.null(a <- dimnames(x)))
                dimnames(y) <- a
        }
        if (is.null(names(y)) && !is.null(a <- names(x)))
            names(y) <- a
    }
    y
}

asVector <-
function (x, mode = "any", strict = TRUE)
    switch(mode,
           "any" =
           {
               if (!is.na(class <- flintClass(x))) {
                   if (strict) {
                       y <- as(x, class)
                       y@dim <- y@dimnames <- y@names <- NULL
                       y
                   }
                   else x
               }
               else if (!is.object(x) &&
                        (is.atomic(x) || (is.list(x) && !is.pairlist(x)) || is.expression(x))) {
                   if (strict)
                       attributes(x) <- NULL
                   x
               }
               else .asVector(x, mode, strict)
           },
           "vector" =, "raw" =, "logical" =, "integer" =, "double" =, "numeric" =, "complex" =, "character" =, "list" =, "expression" =
           {
               if (!is.object(x) &&
                   switch(mode, "vector" = is.atomic(x) || (is.list(x) && !is.pairlist(x)) || is.expression(x), "numeric" = is.double(x), mode == typeof(x))) {
                   if (strict)
                       attributes(x) <- NULL
                   x
               }
               else .asVector(x, switch(mode, "vector" = "any", mode), strict)
           },
           "flint" =, "ulong" =, "slong" =, "fmpz" =, "fmpq" =, "mag" =, "arf" =, "acf" =, "arb" =, "acb" =
           {
               if (!is.na(class <- flintClass(x)) &&
                   (mode == "flint" || mode == class)) {
                   if (strict) {
                       y <- as(x, class)
                       y@dim <- y@dimnames <- y@names <- NULL
                       y
                   }
                   else x
               }
               else {
                   y <- as(x, mode)
                   if (strict)
                       y@dim <- y@dimnames <- y@names <- NULL
                   y
               }
           },
           stop(gettextf("invalid mode \"%s\"", mode),
                domain = NA))
