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
        if (is.null(rownames(newdata)))
            stop('must have rownames on newdata')
        # if all the dates of newdata are greater than the old, then
        # we can prune colums of newdata that are unchanged
        if (inherits(x$dates, 'Date'))
            new.d <- as.Date(rownames(newdata))
        else if (inherits(x$dates, 'POSIXct'))
            new.d <- as.POSIXct(rownames(newdata), tz='UTC')
        else
            new.d <- as.numeric(rownames(newdata))
        if (min(new.d) > max(x$dates)) {
            # Try to eliminate the new data that is unchanged.
            # This is a common operation, so try to do it efficiently.
            # We can change newdata here by eliminating columns that are unchanged.
            if (identical(colnames(x), x$ids)) {
                x.lv <- x[max(x$dates, na.rm=TRUE), , drop=FALSE]
                if (nrow(newdata)==1) {
                    newdata <- newdata[1, arediff(newdata[1,], x.lv), drop=FALSE]
                } else {
                    newdata <- newdata[, rowSums(arediff(t(newdata), t(x.lv))), drop=FALSE]
                }
            } else {
                i <- match(colnames(newdata), x$ids)
                if (any(!is.na(i))) {
                    n.ic <- newdata[, !is.na(i), drop=FALSE]
                    x.ic <- x[max(x$dates, na.rm=TRUE), i[!is.na(i)], drop=FALSE]
                    if (nrow(n.ic)==1)
                        i.diff <- arediff(x.ic, n.ic)
                    else
                        i.diff <- rowSums(arediff(t(x.ic), t(n.ic))) > 0
                    # j is TRUE where we need to keep newdata
                    j <- is.na(i)
                    j[!is.na(i)] <- i.diff
                    if (!all(j))
                        newdata <- newdata[, j, drop=FALSE]
                }
            }
        }
        if (nrow(newdata)==0 || ncol(newdata)==0)
            return(x)
        new.tsm <- sparsetsmat(newdata, ...)
        new.df <- as.data.frame(new.tsm)
    } else {
        stop('newdata must be data.frame, sparsetsmat, or matrix')
    }
    if (nrow(new.df)==0)
        return(x)
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

arediff <- function(x, y) {
    xna <- is.na(x)
    yna <- is.na(y)
    return(ifelse(xna & yna, FALSE, (xna!=yna) | (x!=y)))
}
