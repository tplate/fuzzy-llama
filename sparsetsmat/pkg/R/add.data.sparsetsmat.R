add.data <- function(x, ...) UseMethod('add.data')
add.data.sparsetsmat <- function(x, newdata, ..., sort.ids=non.null(x$sort.ids, FALSE), drop.unneeded.dates=TRUE, direct.df=TRUE) {
    x.df <- as.data.frame(x)
    new.ids <- NULL
    if (inherits(newdata, 'data.frame')) {
        if (direct.df) {
            if (ncol(newdata) != 3)
                stop('newdata has ', ncol(newdata), ' columns; need 3')
            new.df <- newdata
            new.ids <- unique(newdata[[2]])
        } else {
            new.tsm <- sparsetsmat(newdata, ...)
            new.df <- as.data.frame(new.tsm)
        }
        if (any(colnames(new.df) != colnames(x.df)))
            colnames(new.df) <- colnames(x.df)
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
    pc <- function(x) paste(x, collapse=', ')
    if (!identical(class(x.df[[1]]), class(new.df[[1]])))
        stop('incompatible date classes: ', pc(class(x.df[[1]])), ' and ', pc(class(new.df[[1]])))
    if (!identical(class(x.df[[2]]), class(new.df[[2]])))
        stop('incompatible id classes: ', pc(class(x.df[[2]])), ' and ', pc(class(new.df[[2]])))
    if (!identical(class(x.df[[3]]), class(new.df[[3]])))
        stop('incompatible value classes: ', pc(class(x.df[[3]])), ' and ', pc(class(new.df[[3]])))
    if (is.null(new.ids))
        new.ids <- new.tsm$ids
    all.ids <- unique(c(x$ids, new.ids))
    if (sort.ids)
        all.ids <- sort(all.ids)
    return(sparsetsmat(rbind(x.df, new.df), ids=all.ids, backfill=x$backfill, sort.ids=sort.ids, drop.unneeded.dates=drop.unneeded.dates))
}
