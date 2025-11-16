.asVector <-
function (x, mode = "any", strict = TRUE) {
    y <- as.vector(x, mode)
    if (strict) {
        if (!is.null(attributes(y)))
            attributes(y) <- NULL
    }
    else if (is.data.frame(x))
        attributes(y) <- list(names = names(x))
    else
        attributes(y) <- list(dim = dim(x), dimnames = dimnames(x),
                              names = names(x))
    y
}

.asMatrix <-
function (x, mode = "any", strict = TRUE) {
    y <- as.matrix(x)
    if (mode != "any" && switch(mode, "numeric" = "double", mode) != typeof(x))
        storage.mode(y) <- mode
    if (strict) {
        if (length(attributes(y)) != 1L)
            attributes(y) <- list(dim = dim(y))
    }
    y
}

.asArray <- updateBody(.asMatrix, as.matrix = as.array)

asVector <-
function (x, mode = "any", strict = TRUE)
    switch(mode,
           "any" =
           {
               if (!is.na(class <- flintClass(x))) {
                   if (strict) {
                       x <- as(x, class)
                       x@dim <- x@dimnames <- x@names <- NULL
                   }
                   x
               }
               else if (!is.object(x) &&
                        (is.atomic(x) || (is.list(x) && !is.pairlist(x)) || is.expression(x))) {
                   if (strict && !is.null(attributes(x)))
                       attributes(x) <- NULL
                   x
               }
               else .asVector(x, mode, strict)
           },
           "vector" =, "raw" =, "logical" =, "integer" =, "double" =, "numeric" =, "complex" =, "character" =, "list" =, "expression" =
           {
               if (!is.object(x) &&
                   switch(mode, "vector" = is.atomic(x) || (is.list(x) && !is.pairlist(x)) || is.expression(x), "numeric" = is.double(x), mode == typeof(x))) {
                   if (strict && !is.null(attributes(x)))
                       attributes(x) <- NULL
                   x
               }
               else .asVector(x, switch(mode, "vector" = "any", mode), strict)
           },
           "flint" =, "ulong" =, "slong" =, "fmpz" =, "fmpq" =, "mag" =, "arf" =, "acf" =, "arb" =, "acb" =
           {
               if (!is.na(class <- flintClass(x)) &&
                   (mode == "flint" || mode == class)) {
                   if (strict)
                       x <- as(x, class)
               }
               else
                   x <- as(x, mode)
               if (strict)
                   x@dim <- x@dimnames <- x@names <- NULL
               x
           },
           stop(gettextf("invalid mode \"%s\"", mode),
                domain = NA))

asMatrix <-
function (x, mode = "any", strict = TRUE) {
    if (ism <- is.matrix(x)) {
        dim <- dim(x)
        dimnames <- dimnames(x)
    } else {
        dim <- c(length(x), 1L)
        dimnames <- if (!is.null(nms <- names(x))) list(nms, NULL)
    }
    switch(mode,
           "any" =
           {
               if (!is.na(class <- flintClass(x))) {
                   if (strict)
                       x <- as(x, class)
                   x@dim <- dim
                   x@dimnames <- if (!strict) dimnames
                   x@names <- NULL
                   x
               }
               else if (!is.object(x) &&
                        (is.atomic(x) || (is.list(x) && !is.pairlist(x)) || is.expression(x))) {
                   if (strict) {
                       if (!ism || length(attributes(x)) != 1L)
                           attributes(x) <- list(dim = dim)
                   } else {
                       if (!ism) {
                           dim(x) <- dim
                           dimnames(x) <- dimnames
                       }
                   }
                   x
               }
               else .asMatrix(x, mode, strict)
           },
           "vector" =, "raw" =, "logical" =, "integer" =, "double" =, "numeric" =, "complex" =, "character" =, "list" =, "expression" =
           {
               if (!is.object(x) &&
                   switch(mode, "vector" = is.atomic(x) || (is.list(x) && !is.pairlist(x)) || is.expression(x), "numeric" = is.double(x), mode == typeof(x))) {
                   if (strict) {
                       if (!ism || length(attributes(x)) != 1L)
                           attributes(x) <- list(dim = dim)
                   } else {
                       if (!ism) {
                           dim(x) <- dim
                           dimnames(x) <- dimnames
                       }
                   }
                   x
               }
               else .asMatrix(x, switch(mode, "vector" = "any", mode), strict)
           },
           "flint" =, "ulong" =, "slong" =, "fmpz" =, "fmpq" =, "mag" =, "arf" =, "acf" =, "arb" =, "acb" =
           {
               if (!is.na(class <- flintClass(x)) &&
                   (mode == "flint" || mode == class)) {
                   if (strict)
                       x <- as(x, class)
               }
               else
                   x <- as(x, mode)
               x@dim <- dim
               x@dimnames <- if (!strict) dimnames
               x@names <- NULL
               x
           },
           stop(gettextf("invalid mode \"%s\"", mode),
                domain = NA))
}

asArray <- updateBody(asMatrix, is.matrix = is.array, .asMatrix = .asArray)
body(asArray)[[c(2L, 4L, 2L, 3L    )]] <- quote(length(x))
body(asArray)[[c(2L, 4L, 3L, 3L, 3L)]] <- quote(list(nms))
