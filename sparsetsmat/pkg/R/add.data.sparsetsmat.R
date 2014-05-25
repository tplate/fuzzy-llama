add.data <- function(x, ...) UseMethod('add.data')
add.data.sparsetsmat <- function(x, newdata, ..., sort.ids=non.null(x$sort.ids, FALSE), drop.unneeded.dates=TRUE) {
    x.df <- as.data.frame(x)
    if (inherits(newdata, 'data.frame')) {
        new.tsm <- sparsetsmat(newdata, ...)
        new.df <- as.data.frame(new.tsm)
    } else if (inherits(newdata, 'sparsetsmat')) {
        new.tsm <- newdata
        new.df <- as.data.frame(newdata)
    } else if (is.matrix(newdata)) {
        new.tsm <- sparsetsmat(newdata, ...)
        new.df <- as.data.frame(new.tsm)
    } else {
        stop('newdata must be data.frame, sparsetsmat, or matrix')
    }
    # check that x and newdata agree on types of date, ids, and value
    # data.frame version can have different colnames, go with position
    if (class(x.df[[1]]) != class(new.df[[1]]))
        stop('incompatible date classes: ', class(x.df[[1]]), ' and ', class(new.df[[1]]))
    if (class(x.df[[2]]) != class(new.df[[2]]))
        stop('incompatible id classes: ', class(x.df[[2]]), ' and ', class(new.df[[2]]))
    if (class(x.df[[3]]) != class(new.df[[3]]))
        stop('incompatible value classes: ', class(x.df[[3]]), ' and ', class(new.df[[3]]))

    all.ids <- unique(c(x$ids, new.tsm$ids))
    if (sort.ids)
        all.ids <- sort(all.ids)
    return(sparsetsmat(rbind(x.df, new.df), ids=all.ids, backfill=x$backfill, sort.ids=sort.ids, drop.unneeded.dates=drop.unneeded.dates))
}
