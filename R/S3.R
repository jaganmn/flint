.S3.all.equal.flint <-
function (target, current, ...)
    all.equal(target, current, ...)

.S3.anyDuplicated.flint <-
function (x, incomparables = FALSE, ...)
    anyDuplicated(x, incomparables = incomparables, ...)

.S3.aperm.flint <-
function (a, perm, ...)
    aperm(a, perm = perm, ...)

.S3.as.matrix.flint <-
function (x, ...)
    as.matrix(x, ...)

.S3.as.array.flint <-
function (x, ...)
    as.array(x, ...)

.S3.as.data.frame.flint <-
function (x, row.names = NULL, optional = FALSE, ...)
    as.data.frame(x, row.names = row.names, optional = optional, ...)

.S3.as.Date.flint <-
function (x, ...)
    as.Date(x, ...)

.S3.as.POSIXct.flint <-
function (x, tz = "", ...)
    as.POSIXct(x, tz = tz, ...)

.S3.as.POSIXlt.flint <-
function (x, tz = "", ...)
    as.POSIXlt(x, tz = tz, ...)

.S3.chol.flint <-
function (x, ...)
    chol(x, ...)

.S3.determinant.flint <-
function (x, logarithm = TRUE, ...)
    determinant(x, logarithm = logarithm, ...)

.S3.diff.flint <-
function (x, ...)
    diff(x, ...)

.S3.diffinv.flint <-
function (x, ...)
    diffinv(x, ...)

.S3.duplicated.flint <-
function (x, incomparables = FALSE, ...)
    duplicated(x, incomparables = incomparables, ...)

.S3.format.flint <-
function (x, ...)
    format(x, ...)

`.S3.is.na<-.flint` <-
function (x, value)
    `is.na<-`(x, value)

.S3.is.unsorted.flint <-
function (x, na.rm = FALSE, strictly = FALSE)
    is.unsorted(x, na.rm = na.rm, strictly = strictly)

.S3.isSymmetric.flint <-
function (object, ...)
    isSymmetric(object, ...)

.S3.mean.flint <-
function (x, ...)
    mean(x, ...)

.S3.mtfrm.flint <-
function (x)
    mtfrm(x)

.S3.print.flint <-
function (x, ...)
    print(x, ...)

.S3.qr.flint <-
function (x, ...)
    qr(x, ...)

.S3.quantile.flint <-
function (x, ...)
    quantile(x, ...)

.S3.rep.int.flint <-
function (x, times)
    rep.int(x, times)

.S3.rep_len.flint <-
function (x, length.out)
    rep_len(x, length.out)

.S3.scale.flint <-
function (x, center = TRUE, scale = TRUE)
    scale(x, center = center, scale = scale)

.S3.seq.flint <-
function (...)
    seq(...)

.S3.sequence.flint <-
function (nvec, ...)
    sequence(nvec, ...)

.S3.solve.flint <-
function (a, b, ...)
    solve(a, b, ...)

.S3.summary.flint <-
function (object, ...)
    summary(object, ...)

.S3.t.flint <-
function (x)
    t(x)

.S3.unique.flint <-
function (x, incomparables = FALSE, ...)
    unique(x, incomparables = incomparables, ...)
