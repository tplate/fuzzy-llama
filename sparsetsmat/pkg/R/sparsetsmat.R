#' Sparse storage of persistent time-series data
#'
#' @param drop.unneeded.dates Logical.  If TRUE, dates where
#' no value changes are completely forgotten.  If FALSE,
#' dates where no values change are still recorded in the
#' \code{all.dates} component of the object.  In both cases
#' dates where no value changes do not appear in the
#' \code{date} component of the returned object.  In both
#' cases, \code{all.dates} is a sorted version of the dates.
#'
#' @return A \code{sparsetsmat} (S3) object with the following components:
#' \itemize{
#'
#'   \item dates The dates of the underlying data.  Can be
#' \code{Date}, \code{POSIXct}, \code{double} or
#' \code{integer}
#'
#'   \item values The values of the underlying data.  Can be
#' any atomic data type.  It is necessary to be able to do
#' operations like \code{matrix(x$values[1:6], ncol=2}) on
#' the values.  This vector must be the same length as
#' \code{date}.
#'
#'   \item ids The id values.  May be sorted.
#'
#'   \item id.idx The start value index into \code{date} and
#' \code{values} for each id.  Can contain NA values where an
#' id does not occur in the data.
#'
#'   \item id.noc The number of occurences of each id in
#' \code{dates} and \code{values}.  Can contain zeros.
#'
#'   \item all.dates class numeric, Date or POSIXct.  Will
#'   be sorted unless drop.unneeded.dates was FALSE and x was
#'   a matrix with out-of-order dates as rownames.
#'
#'    \item df.colnames colnames of the data.frame version
#'
#'   \item backfill logical
#' }
#'
#' The data are grouped by \code{ids} (possibly sorted) first
#' and sorted by \code{dates} second.  If there are no values
#' for a particular id the value in \code{id.idx} is
#' ignored.
#'
#' @details A sparsetsmat object is stored as a list of four
#' vectors: the date index, the identifier values and
#' indices, and the data values.  The identifiers correspond
#' to the columns in matrix data.  The date index and the
#' values are vectors of the same length.  The identifier
#' indices are the starting indices for each identifier in \code{id.idx}.
#'

non.null <- function(x, y) if (!is.null(x)) x else y

sparsetsmat <- function(x, ...) UseMethod('sparsetsmat')

sparsetsmat.data.frame <- function(x,
                                   date.col=1, id.col=2, value.col=3,
                                   ids=attr(x, 'ids'),
                                   sort.ids=non.null(attr(x, 'sort.ids'), FALSE),
                                   backfill=non.null(attr(x, 'backfill'), FALSE),
                                   keep.df.names=TRUE,
                                   drop.unneeded.dates=FALSE,
                                   POSIX=FALSE,
                                   tz='UTC',
                                   ...)
{
    if (any(is.na(x[,date.col])))
        stop('NA values in date col')
    if (any(is.na(x[,id.col])))
        stop('NA values in id col')
    if (is.null(ids))
        ids <- unique(x[,id.col])
    if (sort.ids)
        ids <- sort(ids)
    # sort the rows, and remove those with NA date or identifier
    df <- x[order(match(x[,id.col], ids), x[,date.col], na.last=NA), , drop=FALSE]
    if (!drop.unneeded.dates)
        all.dates <- sort(unique(x[,date.col]))
    # remove duplicate rows, careful with NA values, they need to be
    # regarded as good data and different to a prior non-na value
    val.na <- is.na(df[, value.col])
    dup.row <- (  (df[-1, id.col] == df[-nrow(df), id.col])
                & (val.na[-1] == val.na[-nrow(df)])
                & (df[-1, value.col] == df[-nrow(df), value.col]))
    if (any(dup.row))
        df <- df[c(TRUE, !dup.row), , drop=FALSE]
    dup.date <- (  (df[-1, id.col] == df[-nrow(df), id.col])
                 & (df[-1, date.col] == df[-nrow(df), date.col]))
    if (any(dup.date)) {
        i <- which(dup.date)[1]
        stop('have duplicate dates for an id: ', df[i,id.col])
    }
    if (is.factor(df[,id.col])) {
        if (is.factor(ids))
            id.idx <- match(as.integer(ids), as.integer(df[, id.col]))
        else
            id.idx <- match(ids, as.character(df[, id.col])) # TODO: match on codes
        id.rle <- rle(as.integer(df[,id.col]))
        id.noc <- id.rle$lengths[match(match(ids, levels(df[,id.col])), id.rle$values)]
        ids <- as.character(ids)
    } else {
        id.idx <- match(ids, df[, id.col])
        id.rle <- rle(df[,id.col])
        id.noc <- id.rle$lengths[match(ids, id.rle$values)]
    }
    id.na <- is.na(id.noc)
    id.noc[id.na] <- 0
    if (any(!id.na && is.na(id.idx)))
        stop('found an id in id.col, but not in rle!')
    # be careful coz id.idx could have NA values in it (if ids supplied)
    dates <- df[,date.col]
    if (inherits(dates, 'Date'))
        storage.mode(dates) <- 'integer'
    # record column names from original data
    if (keep.df.names)
        nm <- colnames(df)[c(date.col, id.col, value.col)]
    else
        nm <- c('dates', 'ids', 'values')
    if (inherits(dates, 'POSIXlt'))
        dates <- as.POSIXct(dates, tz='UTC')
    if (!inherits(dates, 'POSIXct') && POSIX)
        dates <- as.POSIXct(dates, tz='UTC')
    if (drop.unneeded.dates)
        all.dates <- sort(unique(dates))
    return(structure(list(dates=dates, values=df[,value.col],
                          ids=ids, id.idx=id.idx, id.noc=id.noc,
                          all.dates=all.dates, df.colnames=nm,
                          backfill=backfill, sort.ids=sort.ids),
                     class='sparsetsmat'))
}

sparsetsmat.default <- function(x,
                                date.col=1, id.col=2, value.col=3,
                                ids=attr(x, 'ids'),
                                sort.ids=non.null(attr(x, 'sort.ids'), FALSE),
                                backfill=non.null(attr(x, 'backfill'), FALSE),
                                keep.df.names=TRUE,
                                drop.unneeded.dates=FALSE,
                                POSIX=FALSE,
                                tz='UTC',
                                ...)
{
    if (length(dim(x))!=2)
        stop('expecting data.frame or matrix')
    # Construct a sparsetsmat from a matrix
    # First work out whether we have date or date-times
    d <- rownames(x)
    if (any(is.na(d)))
        stop('NAs in rownames on x')
    if (is.null(d)) {
        dates <- seq(nrow(x))
    } else if (all(regexpr('^[0-9]+$', d)>0)) {
        dates <- as.numeric(d)
    } else if (max(nchar(d))<=10 && !POSIX) {
        dates <- as.Date(d)
        storage.mode(dates) <- 'integer'
        if (any(is.na(dates)))
            stop('NAs in Date-like(?) rownames on x')
    } else {
        dates <- as.POSIXct(d, tz='UTC')
        if (any(is.na(dates)))
            stop('NAs in date-time(?) rownames on x')
    }
    all.dates <- dates
    if (is.null(ids))
        ids <- colnames(x)
    if (is.null(ids) && is.null(colnames(x))) {
        id.ord <- ids <- seq(ncol(x))
    } else {
        if (sort.ids)
            ids <- sort(ids)
        id.ord <- match(ids, colnames(x))
        if (all(is.na(id.ord)) && length(ids) && ncol(x))
            warning('no ids found in colnames of x')
    }
    res <- lapply(id.ord, function(i) {
        col <- x[,i,drop=TRUE]
        prev <- col[c(1, seq(len=length(col)-1))]
        j <- which((!is.na(col) & replace(is.na(prev) | col != prev, 1, TRUE))
                   | (is.na(col) & !is.na(prev)))
        return(list(j, length(j), col[j]))
    })
    dd <- dates[unlist(lapply(res, '[[', 1), use.names=FALSE)]
    vv <- unlist(lapply(res, '[[', 3), use.names=FALSE)
    id.noc <- as.integer(unlist(lapply(res, '[[', 2), use.names=FALSE))
    id.idx <- as.integer(round(cumsum(c(1,id.noc[-length(id.noc)]))))
    id.idx[id.noc==0] <- NA
    if (drop.unneeded.dates)
        all.dates <- sort(unique(dd))
    return(structure(list(dates=dd, values=vv, ids=ids,
                          id.idx=id.idx, id.noc=id.noc,
                          all.dates=all.dates, df.colnames=c('dates','ids','values'),
                          backfill=backfill, sort.ids=sort.ids),
                     class='sparsetsmat'))
}

sparsetsmat.matrix <- sparsetsmat.default
