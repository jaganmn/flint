.S3.as.data.frame.flint <-
function (x, row.names = NULL, optional = FALSE, ...)
    as.data.frame(x, row.names = row.names, optional = optional, ...)

.S3.format.flint <-
function (x, ...)
    format(x, ...)

.S3.is.unsorted.flint <-
function (x, na.rm = FALSE, strictly = FALSE)
    is.unsorted(x, na.rm = na.rm, strictly = strictly)

.S3.rep.int.flint <-
function (x, times)
    rep.int(x, times = times)

.S3.rep_len.flint <-
function (x, length.out)
    rep_len(x, length.out = length.out)
