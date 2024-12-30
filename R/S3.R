.S3.as.Date.flint <-
function (x, ...)
    as.Date(x, ...)

.S3.as.POSIXct.flint <-
function (x, tz = "", ...)
    as.POSIXct(x, tz = tz, ...)

.S3.as.POSIXlt.flint <-
function (x, tz = "", ...)
    as.POSIXlt(x, tz = tz, ...)

.S3.as.data.frame.flint <-
function (x, row.names = NULL, optional = FALSE, ...)
    as.data.frame(x, row.names = row.names, optional = optional, ...)

.S3.format.flint <-
function (x, ...)
    format(x, ...)

`.S3.is.na<-.flint` <-
function (x, value)
    `is.na<-`(x, value)

.S3.is.unsorted.flint <-
function (x, na.rm = FALSE, strictly = FALSE)
    is.unsorted(x, na.rm = na.rm, strictly = strictly)

.S3.quantile.flint <-
function (x, ...)
    quantile(x, ...)

.S3.rep.int.flint <-
function (x, times)
    rep.int(x, times = times)

.S3.rep_len.flint <-
function (x, length.out)
    rep_len(x, length.out = length.out)

.S3.summary.flint <-
function (object, ...)
    summary(object, ...)
