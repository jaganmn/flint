.error.notTotalOrder <-
function () {
    call <- sys.call(-1L)
    errorCondition(gettextf("'%s' is not a total order on the range of '%s'",
                            "<=", "arb"),
                   class = "notTotalOrderError", call = call)
}

.error.notSubsettable <-
function (object) {
    call <- sys.call(-1L)
    errorCondition(gettextf("object of type '%s' is not subsettable",
                            typeof(object)),
                   object = object,
                   class = "notSubsettableError", call = call)
}

.error.missingSubscript <-
function () {
    call <- sys.call(-1L)
    errorCondition(gettext("missing subscript"),
                   class = "simpleError", call = call)
}

.error.missingSubassignValue <-
function () {
    call <- sys.call(-1L)
    errorCondition(gettext("missing subassignment value"),
                   class = "simpleError", call = call)
}

.error.invalidArgumentType <-
function (object) {
    call <- sys.call(-1L)
    errorCondition(gettextf("invalid argument type '%s'",
                            typeof(object)),
                   class = "simpleError", call = call)
}

.error.invalidSubscriptType <-
function (object) {
    call <- sys.call(-1L)
    errorCondition(gettextf("invalid subscript type '%s'",
                            typeof(object)),
                   class = "simpleError", call = call)
}

.error.invalidSubassignValueType <-
function (object) {
    call <- sys.call(-1L)
    errorCondition(gettextf("invalid subassignment value type '%s'",
                            typeof(object)),
                   class = "simpleError", call = call)
}

.error.invalidArgumentClass <-
function (object) {
    call <- sys.call(-1L)
    errorCondition(gettextf("invalid argument class %s",
                            deparse(class(object))),
                   class = "simpleError", call = call)
}

.error.invalidSubscriptClass <-
function (object) {
    call <- sys.call(-1L)
    errorCondition(gettextf("invalid subscript class %s",
                            deparse(class(object))),
                   class = "simpleError", call = call)
}

.error.invalidSubassignValueClass <-
function (object) {
    call <- sys.call(-1L)
    errorCondition(gettextf("invalid subassignment value class %s",
                            deparse(class(object))),
                   class = "simpleError", call = call)
}

.error.invalidArity <-
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
    errorCondition(gettext("attempt to select less than one element"),
                   class = "simpleError", call = call)
}

.error.subscriptTooMany <-
function () {
    call <- sys.call(-1L)
    errorCondition(gettext("attempt to select more than one element"),
                   class = "simpleError", call = call)
}

.error.emptyReplace <-
function () {
    call <- sys.call(-1L)
    errorCondition(gettext("replacement has length zero"),
                   class = "simpleError", call = call)
}

.warning.remainderInReplace <-
function () {
    call <- sys.call(-1L)
    warningCondition(gettext("number of items to replace is not a multiple of replacement length"),
                     class = "simpleWarning", call = call)
}
