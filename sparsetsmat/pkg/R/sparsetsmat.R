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
#'   \item date The dates of the underlying data.  Can be
#' \code{Date}, \code{POSIXct}, \code{double} or
#' \code{integer}
#'
#'   \item value The values of the underlying data.  Can be
#' any atomic data type.  It is necessary to be able to do
#' operations like \code{matrix(x$value[1:6], ncol=2}) on
#' the values.  This vector must be the same length as
#' \code{date}.
#'
#'   \item id The id values.  Always sorted.
#'
#'   \item id.idx The start value index into \code{date} and
#'   \code{value} for each id, with the length of \code{date}
#'   as the last element, so that
#'   \code{length(id.idx)==(length(id)+1)}.
#'
#'   \item all.ids All the id values, including ones which
#' have no data.  May or may not be sorted.
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
#' The data is sorted by \code{id} first and \code{date}
#' second.  If there are no values for a particular id it
#' still has a start vlaue in \code{id.idx}, and the start
#' value of the next id will be the same start value.
#'
#' @details A sparsetsmat object is stored as a list of four
#' vectors: the date index, the identifier values and
#' indices, and the data values.  The identifiers correspond
#' to the columns in matrix data.  The date index and the
#' values are vectors of the same length.  The identifier
#' indices are the starting indices for each identifier,
#' with an extra value at the end which is the length of the
#' date index.
#'
sparsetsmat <- function(x, date.col=1, id.col=2, value.col=3, sort.ids=TRUE, ids=NULL, backfill=FALSE, drop.unneeded.dates=FALSE) {
    if (is.data.frame(x)) {
        # sort the rows, and remove those with NA date or identifier
        df <- x[order(x[,id.col], x[,date.col], na.last=NA), , drop=FALSE]
        if (!drop.unneeded.dates)
            all.dates <- sort(unique(x[,date.col]))
        # remove duplicate rows, careful with NA values, they need to be
        # regarded as good data and different to a prior non-na value
        val.na <- is.na(df[, value.col])
        dup.row <- (  (df[-1, id.col] == df[-nrow(df), id.col])
                    & (val.na[-1] == val.na[-nrow(df)])
                    & (df[-1, value.col] == df[-nrow(df), value.col]))
        dup.date <- (  (df[-1, id.col] == df[-nrow(df), id.col])
                     & (df[-1, date.col] == df[-nrow(df), date.col]))
        if (any(dup.date)) {
            i <- which(dup.date)[1]
            stop('have duplicate dates for an id: ', df[i,id.col])
        }
        if (any(dup.row))
            df <- df[c(TRUE, !dup.row), , drop=FALSE]
        if (is.null(ids)) {
            if (sort.ids) {
                all.ids <- ids <- unique(sort(df[,id.col]))
            } else {
                all.ids <- unique(df[,id.col])
                ids <- sort(all.ids)
            }
        }
        id.idx <- c(match(ids, df[, id.col]), nrow(df)+1L)
        # be careful coz id.idx could have NA values in it (if ids supplied)
        date <- df[,date.col]
        if (inherits(date, 'Date'))
            storage.mode(date) <- 'integer'
        # record column names from original data
        nm <- c(names(df[1, date.col, drop=FALSE]),
                names(df[1, id.col, drop=FALSE]),
                names(df[1, value.col, drop=FALSE]))
        if (inherits(date, 'POSIXlt'))
            date <- as.POSIXct(date)
        if (drop.unneeded.dates)
            all.dates <- sort(unique(date))
        if (any(diff(id.idx) < 0))
            stop('inconsistent id.idx was constructed')
        return(structure(list(date=date, value=df[,value.col],
                              id=ids, id.idx=id.idx,
                              all.ids=all.ids,
                              all.dates=all.dates, df.colnames=nm,
                              backfill=backfill, sort.ids=sort.ids),
                         class='sparsetsmat'))
    } else if (length(dim(x))==2) {
        # Construct a sparsetsmat from a matrix
        # First work out whether we have date or date-times
        d <- rownames(x)
        if (any(is.na(d)))
            stop('NAs in rownames on x')
        if (is.null(d)) {
            date <- seq(nrow(x))
        } else if (all(regexpr('^[0-9]+$', d)>0)) {
            date <- as.numeric(d)
        } else if (max(nchar(d))<=10) {
            date <- as.Date(d)
            if (any(is.na(date)))
                stop('NAs in Date-like(?) rownames on x')
        } else {
            date <- strptime(d)
            if (any(is.na(date)))
                stop('NAs in date-time(?) rownames on x')
        }
        all.dates <- date
        res <- lapply(seq(ncol(x)), function(i) {
            col <- x[,i,drop=TRUE]
            prev <- col[c(1, seq(len=length(col)-1))]
            j <- which((!is.na(col) & replace(is.na(prev) | col != prev, 1, TRUE))
                       | (is.na(col) & !is.na(prev)))
            return(list(j, length(j), col[j]))
        })
        dd <- date[unlist(lapply(res, '[[', 1), use.names=FALSE)]
        vv <- unlist(lapply(res, '[[', 3), use.names=FALSE)
        ids <- colnames(x)
        if (sort.ids) all.ids <- sort(ids) else all.ids <- ids
        id.idx <- as.integer(round(cumsum(c(1,unlist(lapply(res, '[[', 2), use.names=FALSE)))))
        if (drop.unneeded.dates)
            all.dates <- sort(unique(dd))
        if (any(diff(id.idx) < 0))
            stop('inconsistent id.idx was constructed')
        return(structure(list(date=dd, value=vv, id=ids, id.idx=id.idx, all.ids=all.ids,
                              all.dates=all.dates, df.colnames=c('date','id','value'),
                              backfill=backfill, sort.ids=sort.ids),
                         class='sparsetsmat'))
    } else {
        stop('expecting data.frame or matrix')
    }
}


