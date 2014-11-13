#' Add data to an object
#' @rdname add.tsdata
#' @param x An object to add time-series data to, either specified by name or by value.
#' If specified by name (as a character vector), the object is changed in place.

add.tsdata <- function(x, ...) UseMethod('add.tsdata')

#' @rdname add.tsdata
#' @method add.tsdata default
#' @param pos the search path position to find the variable

add.tsdata.default <- function(x, ..., pos=NULL) {
    if (is.character(x) && length(x)==1) {
        if (is.null(pos))
            pos <- find(x, numeric=TRUE)[1]
        if (is.na(pos))
            stop(x, ' not found')
        y <- get(x, pos=pos)
        z <- add.tsdata(y, ...)
        assign(x, value=z, pos=pos[1])
    } else {
        stop('cannot handle object with class ', class(x))
    }
}

#' @rdname add.tsdata
#' @method add.tsdata sparsetsmat
#' @param newdata The new data to add to the object.  Can be
#' a dataframe, a sparsetsmat object, or matrix data.  If
#' matrix data, explicit NA's 'stop' a prior value from
#' propagating forward, but missing columns do not.
#' @param ... additional arguments.
#' @param sort.ids Same as for \code{sparsetsmat}.
#' @param drop.unneeded.dates Same as for \code{sparsetsmat}.
#' @param shadow If TRUE, drop all data in x that is preceeded by data in newdata (i.e., newdata casts a shadow over x)
#' @param direct.df If TRUE, treat newdata as a standard
#' data.frame representation of a sparse tsmat object; i.e.,
#' dates are in column 1, ids in column 2, and values in
#' column 3.  If FALSE, \code{sparsetsmat(newdata, ...)} is
#' used to convert \code{newdata} to a sparsetsmat object.
#'
#' @details Data is added to a sparsetsmat object by
#' converting both to data.frames, rbinding, and then
#' converting back to sparsetsmat objects.
#'
add.tsdata.sparsetsmat <- function(x,
                                   newdata,
                                   ...,
                                   pos=1,
                                   sort.ids=non.null(x$sort.ids, FALSE),
                                   drop.unneeded.dates=TRUE,
                                   shadow=FALSE,
                                   direct.df=TRUE) {
    x.is.named <- FALSE
    if (is.character(x) && length(x)==1) {
        if (is.null(pos))
            pos <- find(x, numeric=TRUE)[1]
        if (is.na(pos))
            stop(x, ' not found')
        x.is.named <- TRUE
        x.name <- x
        x <- get(x, pos=pos)
    }
    x.df <- as.data.frame(x)
    new.ids <- NULL
    if (inherits(newdata, 'data.frame')) {
        if (direct.df) {
            if (ncol(newdata) != 3)
                stop('newdata has ', ncol(newdata), ' columns; need 3')
            new.df <- newdata
            new.ids <- unique(newdata[[2]])
            new.tsm <- NULL
        } else {
            new.tsm <- sparsetsmat(newdata, ..., sort.ids=sort.ids, drop.unneeded.dates=drop.unneeded.dates)
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
            if (identical(colnames(newdata), x$ids)) {
                x.lv <- x[max(x$dates, na.rm=TRUE), , drop=FALSE]
                if (!identical(colnames(newdata), colnames(x)))
                    stop('mismatch in column names of x and newdata')
                if (nrow(newdata)==1) {
                    newdata <- newdata[1, areDiff(newdata[1,], x.lv), drop=FALSE]
                } else {
                    newdata <- newdata[, rowSums(areDiff(t(newdata), drop(x.lv)))>0, drop=FALSE]
                }
            } else {
                i <- match(colnames(newdata), x$ids)
                if (any(!is.na(i))) {
                    n.ic <- newdata[, !is.na(i), drop=FALSE]
                    x.ic <- x[max(x$dates, na.rm=TRUE), i[!is.na(i)], drop=FALSE]
                    if (nrow(n.ic)==1)
                        i.diff <- areDiff(n.ic, x.ic)
                    else
                        i.diff <- rowSums(areDiff(t(n.ic), drop(x.ic))) > 0
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
    # Look to see which records should be dropped from x.df for the reason of
    # being overridden by new records in new.df.
    # If not all records in new.df start with the same date, then do the
    # dropping piecewise.
    if (shadow && is.null(new.tsm)) {
        if (!direct.df)
            stop('internal problem')
        if (min(new.df$dates) <= max(x$dates)) {
            # The tapply(..., FUN=min) on Date or POSIXct will return numeric
            new.id.stdate <- tapply(new.df$dates, new.df$ids, min)
            x.ids.new.idx <- match(x.df$ids, names(new.id.stdate))
            x.df.newst <- new.id.stdate[x.ids.new.idx]
            x.df <- x.df[is.na(x.ids.new.idx) | as.numeric(x.df$dates) < x.df.newst, , drop=FALSE]
        }
    } else if (shadow && min(new.tsm$dates) <= max(x$dates)) {
        # Trim ids down to those actually present.
        new.ids2.idx <- which(new.tsm$id.noc>0)
        new.ids2 <- new.tsm$ids[new.ids2.idx]
        if (length(new.ids2.idx)) {
            # what start dates?
            new.ids2.stdate <- new.tsm$dates[new.tsm$id.idx[new.ids2.idx]]
            if (all(new.ids2.stdate == min(new.ids2.stdate))) {
                old.alone.ids <- setdiff(x$ids, new.ids2)
                # all new ids have the same date
                if (length(old.alone.ids)==0) {
                    x.df <- x.df[x.df$dates < min(new.ids2.stdate), , drop=FALSE]
                } else {
                    x.ids.new.idx <- match(x.df$ids, new.tsm$ids)
                    x.df <- x.df[x.df$dates < min(new.ids2.stdate) | is.na(x.ids.new.idx), , drop=FALSE]
                }
            } else {
                # new ids have a variety of start dates
                x.ids.new.idx <- match(x.df$ids, new.tsm$ids)
                x.df.newst <- new.tsm$dates[new.tsm$id.idx[x.ids.new.idx]]
                x.df <- x.df[is.na(x.ids.new.idx) | x.df$dates < x.df.newst, , drop=FALSE]
            }
        }
    } else if (!shadow && min(new.df$dates) <= max(x$dates)) {
        # only remove direct conflicts (elements of new.df take priority over those of x.df)
        # can assume existence of x.df, x (tsm) and new.df
        new.ids.u <- unique(new.df$ids)
        new.dates.u <- as.numeric(unique(new.df$dates))
        new.ii <- match(as.numeric(new.df$dates), new.dates.u) * length(new.ids.u) + match(new.df$ids, new.ids.u)
        x.ii <- match(as.numeric(x.df$dates), new.dates.u) * length(new.ids.u) + match(x.df$ids, new.ids.u)
        x.keep <- !is.element(x.ii, new.ii)
        if (!all(x.keep))
            x.df <- x.df[x.keep, , drop=FALSE]
    }
    x <- sparsetsmat(rbind(x.df, new.df), ids=all.ids, backfill=x$backfill, sort.ids=sort.ids, drop.unneeded.dates=drop.unneeded.dates)
    if (x.is.named)
        assign(x.name, x, pos=pos)
    return(x)
}

areDiff <- function(x, y) {
    xna <- is.na(x)
    yna <- is.na(y)
    return(ifelse(xna & yna, FALSE, (xna!=yna) | (x!=y)))
}
