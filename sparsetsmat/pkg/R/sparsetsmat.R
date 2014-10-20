#'
#' Sparse storage for persistent time-series data
#'
#' The \code{sparsetsmat} class provides an object to store
#' slow changing time-series data in a compact way.
#'
#' The data is stored compactly by assuming that if no data is
#' stored for a particular id on a particular date, the most
#' recent data available for that id should be used.  The
#' format is designed for fast extraction, but not for fast
#' modification or addition of new data.
#'
#' A \code{sparsetsmat} object behaves like an ordinary
#' matrix with respect to many methods, including extraction
#' (subsetting using the \code{x[i,j]} syntax), \code{dim},
#' \code{dimnames}, etc.  Rownames are dates (stored as
#' either \code{Date} or \code{POSIXct} objects) and
#' colnames are arbitrary character labels.  The data type
#' stored can be any atomic R object (e.g., double, integer,
#' character, etc.)
#'
#' For example, a dense matrix with the following structure
#' \tabular{rrrr}{
#'              \tab X \tab Y \tab Z \cr
#' '2001-01-01' \tab 'a' \tab NA \tab NA \cr
#' '2001-01-03' \tab 'a' \tab 'b' \tab NA \cr
#' '2001-01-05' \tab 'a' \tab 'B' \tab 'C'
#' }
#' is stored with effectively the following data:
#' \tabular{rrr}{
#' date \tab id \tab value \cr
#' '2001-01-01' \tab 'X' \tab 'a' \cr
#' '2001-01-03' \tab 'Y' \tab 'b' \cr
#' '2001-01-05' \tab 'Y' \tab 'B' \cr
#' '2001-01-05' \tab 'Z' \tab 'C'
#' }
#'
#' The result of subsetting a \code{sparsetsmat} object is
#' always an ordnary dense matrix.
#'
#' The indices supplied to the subset method can be
#' character, \code{Date}, or \code{POSIXct}.  Unlike with
#' ordinary matrices, the subsetting method for
#' \code{sparsetsmat} will automatically interpolate data
#' for dates in the date range of the object.
#'
#' Subsetting is intended to be as efficient as possible and
#' is coded in C++ using Rcpp.  Relative to subsetting a
#' large ordinary matrix, subsetting a \code{sparsetsmat}
#' object takes between 1 and 6 times as long.  See the
#' source package file \code{tests/zhuge.Rt} for timing examples.
#'
#' \code{sparsetsmat} objects differ from the sparse matrix
#' objects of the \code{Matrix} package in that the uncoded
#' value for \code{sparsetsmat} objects is the most recent
#' value in the column, or \code{NA} if there is no prior
#' value, whereas in the \code{Matrix} package, the uncoded
#' value is zero.  Furthermore, the \code{sparsetsmat}
#' package does not provide any matrix-algebra routines that
#' work directly on the compactly stored data.  The
#' \code{sparsetsmat} package does only one thing: store
#' slow-changing persistent time series data in a compact
#' manner, and convert that data to dense matrices in a
#' reasonably efficient manner.
#'
#' @docType package
#' @name sparsetsmat-package
#' @seealso \code{\link{sparsetsmat}}
NULL

#'
#' Sparse storage for persistent time-series data
#'
#' Create a sparsetsmat object to store slow-changing
#' time-series data in a space-efficient manner.
#'
#' @param x an object with data for a sparsetsmat: either a
#' data frame with three columns specifying the data
#' sparsely (dates, column lables and values), or a dense matrix.
#'
#' @param date.col The column of the data frame to get dates from (default = 1)
#' @param id.col The column of the data frame to get id from (default = 2)
#' @param value.col The column of the data frame to get values from (default = 3)
#' @param ids The complete set of ids to use.
#' @param sort.ids Should ids be sorted?  Affects how new data added to the object is treated. Default FALSE.
#' @param backfill Should a query with a data earlier than any for a particular id in the data get the value associated with the first date present in the data? Default FALSE.
#' @param keep.df.names Record the data frame column names so that when as.data.frame() is used on the sparsetsmat object, original names can be used?  Default TRUE.
#' @param POSIX Should dates be POSIXct or Date? Default FALSE.
#' @param tz Time zone to use for dates, default is UTC.
#'
#' @param drop.unneeded.dates Logical.  If TRUE, dates where
#' no value changes are completely forgotten.  If FALSE,
#' dates where no values change are still recorded in the
#' \code{all.dates} component of the object.  In both cases
#' dates where no value changes do not appear in the
#' \code{date} component of the returned object.  In both
#' cases, \code{all.dates} is a sorted version of the dates.
#'
#' @param ... additional arguments for constructors
#'
#' @return A \code{sparsetsmat} (S3) object with the following components:
#' \itemize{
#'
#'   \item dates The dates of the underlying data.  Can be
#' \code{Date}, \code{POSIXct}, \code{double} or
#' \code{integer}.  This can have repititions of dates.
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
#'   \item all.dates class numeric, Date or POSIXct.
#' Records all the dates that have been stored in the data,
#' which can include dates on which no data changed, and
#' which will not be recorded in the \code{dates} component.
#' Will be sorted unless drop.unneeded.dates was FALSE and x
#' was a matrix with out-of-order dates as rownames.
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
#' @details A sparsetsmat object is an S3 object that stores
#' slow-changing data in a compact format.  See the Value
#' section for a description of the structure.
#'
#' @examples
#' m2.df <- data.frame(d=seq(as.Date('2001-01-01'), len=5, by='days')[
#'                         c(    1,  4,  5,  2,  1,  2,  3,  5)],
#'                       p=c('a','a','a','b','e','e','d','d'),
#'                       v=c(  1,  2,  3,  4,  5,  6,  7,  8))
#' m2.tsm <- sparsetsmat(m2.df, sort.ids=TRUE)
#' m2.tsm[, ]
#' m2.tsm
#' m2.mat <- as.matrix(m2.tsm)
#' attributes(m2.mat)
#' as.data.frame(m2.tsm)
#' as.matrix(m2.tsm)
#' # Ordinary square i,j indexing
#' m2.tsm[2,3]
#' m2.tsm[2,2:3]
#' m2.tsm[2,1:3]
#' m2.tsm[1:2,1:4]
#' # Matrix indexing
#' m2.tsm[cbind(c(2,3,4),c(1))]
#' m2.tsm[cbind(c(2,3,4),c(1,2,3))]
#' m2.tsm[cbind(c(2,3,4),c(3))]
#' # Matrix indexing using character indices
#' m2.tsm[data.frame(c('2001-01-01','2001-01-03','2001-01-05'),c('a','b','e'))]

sparsetsmat <- function(x, ...) UseMethod('sparsetsmat')

#' sparsetsmat
#' @rdname sparsetsmat
#' @method sparsetsmat data.frame

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
                & (df[-1, value.col] == df[-nrow(df), value.col] | val.na[-1]))
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
        dates <- as.POSIXct(dates, tz=tz)
    if (!inherits(dates, 'POSIXct') && POSIX)
        dates <- as.POSIXct(dates, tz=tz)
    if (drop.unneeded.dates)
        all.dates <- sort(unique(dates))
    # TODO: assign preformatted dates & all.dates
    return(structure(list(dates=dates, values=df[,value.col],
                          ids=ids, id.idx=id.idx, id.noc=id.noc,
                          all.dates=all.dates, df.colnames=nm,
                          backfill=backfill, sort.ids=sort.ids),
                     class='sparsetsmat'))
}

#' sparsetsmat
#' @rdname sparsetsmat
#' @method sparsetsmat default

sparsetsmat.default <- function(x,
                                ids=attr(x, 'ids'),
                                sort.ids=non.null(attr(x, 'sort.ids'), FALSE),
                                backfill=non.null(attr(x, 'backfill'), FALSE),
                                drop.unneeded.dates=FALSE,
                                POSIX=FALSE,
                                tz='UTC',
                                ...)
{
    if (length(dim(x))!=2)
        stop('expecting data.frame or matrix')
    # Construct a sparsetsmat from a matrix (or a data.frame treated as a matrix)
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
        dates <- as.POSIXct(d, tz=tz)
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
    # TODO: assign preformatted dates & all.dates
    return(structure(list(dates=dd, values=vv, ids=ids,
                          id.idx=id.idx, id.noc=id.noc,
                          all.dates=all.dates, df.colnames=c('dates','ids','values'),
                          backfill=backfill, sort.ids=sort.ids),
                     class='sparsetsmat'))
}

sparsetsmat.matrix <- sparsetsmat.default

non.null <- function(x, y) if (!is.null(x)) x else y

