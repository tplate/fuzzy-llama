#' Replace.sparsetsmat
#'
#' Replace an ordinary sub-matrix or values in a sparsetsmat object.
#'
#' @rdname repl.sparsetsmat
#'
#' @method [<- sparsetsmat
#' @param x a sparsetsmat object
#'
#' @param i the row indices, can be numeric, character, Date
#' or POSIXct, or a 2-column matrix or dataframe (or list)
#' for matrix indexing.
#'
#' @param j the j indices, can be numeric, character or NULL
#' in the case of matrix indexing
#'
#' @param ... not used
#' @param value the replacement values
#' @param verbose if TRUE, print informative messages
#'
'[<-.sparsetsmat' <- function(x, i, j, ..., verbose=getOption('sparsetsmat.verbose', FALSE), value) {
    # verbose <- FALSE
    # Convert square i,j indexing to matrix indexing.
    # Retrieve the existing values and look for differences.
    # Then only assign the differences.
    # Get the existing and new as data.frames.
    # Then stages of assigning the differences (in the df representation):
    #   (1) for those that match on i,j indices, change the values
    #   (2) for the remaining (if any), call add.tsdata()
    # Finally, convert back to sparsetsmat.
    if (length(list(...)))
        stop('unexpected ... args')
    nIdxs <- nargs() - 1 - (!missing(value)) - (!missing(verbose))
    if (nIdxs==1)
        y <- x[i, details=TRUE, backfill=FALSE]
    else if (nIdxs==2)
        y <- x[i, j, details=TRUE, backfill=FALSE]
    else
        stop('unexpected number of indices')
    if (inherits(x$dates, 'Date'))
        y$i <- structure(y$i.idx, class='Date')
    else if (inherits(x$dates, 'POSIXct'))
        y$i <- structure(y$i.idx, class='POSIXct', tzone='UTC')
    else
        y$i <- y$i.idx
    # get the values in the same order as the indices in y
    kv <- if (is.null(y$kk)) value else value[y$kk]
    dd <- data.frame(i=y$i, j=x$ids[y$j.idx], val.idx=y$val.idx,
                     old=y$val, new=kv, row.names=NULL, stringsAsFactors=FALSE)
    # Have to be careful looking to delete rows where new!=old
    # because one new value can make another new value relevant.
    # E.g., if exising data is 4,4,4, and the change is to 2,4,4
    # the second 4 becomes relevant.
    # But, if no values are new, can safely return without doing anything
    if (all(is.na(dd$old) == is.na(dd$new) & (is.na(dd$old) | dd$old == dd$new))) {
        if (verbose) cat('Nothing to change\n')
        return(x) # nothing to change -- no new values
    }
    y$i.used <- x$dates[y$val.idx]
    # split into two parts: where indices match existing rows, and where they are new
    i <- y$i == y$i.used
    dde <- dd[i,,drop=FALSE]
    ddn <- dd[!i,,drop=FALSE]
    if (any(i)) {
        if (verbose) cat('Changing', sum(i), 'existing entries\n')
        x$values[dde$val.idx] <- kv[i]
    }
    if (nrow(ddn)==0) {
        # no new rows
        if (verbose) cat('Adding no new rows in data frame version\n')
        return(x)
    } else {
        # have new rows
        if (verbose) cat('Adding', nrow(ddn), 'new rows in data frame version\n')
        return(add.tsdata(x, ddn[,c('i','j','new')]))
    }
}
