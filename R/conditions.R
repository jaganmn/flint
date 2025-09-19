.error.notTotalOrder <-
function () {
    call <- sys.call(-1L)
    errorCondition(gettextf("'%s' is not a total order on the range of \"%s\"",
                            "<=", "arb"),
                   class = "notTotalOrderError", call = call)
}

.error.notSubsettable <-
function (object) {
    call <- sys.call(-1L)
    errorCondition(gettextf("object of type \"%s\" is not subsettable",
                            typeof(object)),
                   object = object,
                   class = "notSubsettableError", call = call)
}

.error.subscriptMissing <-
function () {
    call <- sys.call(-1L)
    errorCondition(gettext("missing subscript"),
                   class = "simpleError", call = call)
}

.error.subassignMissing <-
function () {
    call <- sys.call(-1L)
    errorCondition(gettext("missing subassignment value"),
                   class = "simpleError", call = call)
}

.error.argumentInvalidType <-
function (object) {
    call <- sys.call(-1L)
    errorCondition(gettextf("invalid argument type \"%s\"",
                            typeof(object)),
                   class = "simpleError", call = call)
}

.error.subscriptInvalidType <-
function (object) {
    call <- sys.call(-1L)
    errorCondition(gettextf("invalid subscript type \"%s\"",
                            typeof(object)),
                   class = "simpleError", call = call)
}

.error.subassignInvalidType <-
function (object) {
    call <- sys.call(-1L)
    errorCondition(gettextf("invalid subassignment value type \"%s\"",
                            typeof(object)),
                   class = "simpleError", call = call)
}

.error.argumentInvalidClass <-
function (object, implicit.ok = FALSE) {
    call <- sys.call(-1L)
    if (implicit.ok || is.object(object))
    errorCondition(gettextf("invalid argument class %s",
                            deparse(class(object))),
                   class = "simpleError", call = call)
    else
    errorCondition(gettextf("invalid argument type \"%s\"",
                            typeof(object)),
                   class = "simpleError", call = call)
}

.error.subscriptInvalidClass <-
function (object) {
    call <- sys.call(-1L)
    errorCondition(gettextf("invalid subscript class %s",
                            deparse(class(object))),
                   class = "simpleError", call = call)
}

.error.subassignInvalidClass <-
function (object) {
    call <- sys.call(-1L)
    errorCondition(gettextf("invalid subassignment value class %s",
                            deparse(class(object))),
                   class = "simpleError", call = call)
}

.error.subscriptInvalidArity <-
function () {
    call <- sys.call(-1L)
    errorCondition(gettext("incorrect number of dimensions"),
                   class = "simpleError", call = call)
}

.error.subscriptOutOfBounds <-
function (object, subscript, index) {
    call <- sys.call(-1L)
    errorCondition(gettext("subscript out of bounds"),
                   object = object,
                   subscript = subscript,
                   index = index,
                   class = "subscriptOutOfBoundsError", call = call)
}

.error.subscriptNegativePositive <-
function () {
    call <- sys.call(-1L)
    errorCondition(gettext("negative and positive subscripts cannot be mixed"),
                   class = "simpleError", call = call)
}

.error.subscriptTooFew <-
function () {
    call <- sys.call(-1L)
    errorCondition(gettext("subscript indexes zero elements"),
                   class = "simpleError", call = call)
}

.error.subscriptTooMany <-
function () {
    call <- sys.call(-1L)
    errorCondition(gettext("subscript indexes more than one element"),
                   class = "simpleError", call = call)
}

.error.subassignTooFew <-
function () {
    call <- sys.call(-1L)
    errorCondition(gettext("subassignment value has length zero"),
                   class = "simpleError", call = call)
}

.error.subassignTooMany <-
function () {
    call <- sys.call(-1L)
    errorCondition(gettext("subassignment value has length greater than one"),
                   class = "simpleError", call = call)
}
