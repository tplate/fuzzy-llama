'[<-.sparsetsmat' <- function(x, i, j, ..., value) {
    # Convert square i,j indexing to matrix indexing.
    # Retrieve the existing values and look for differences.
    # Then only assign the differences.
    # Get the existing and new as data.frames.
    # Then stages of assigning the differences (in the df representation):
    #   (1) for those that match on i,j indices, change the values
    #   (2) for the remaining (if any), call add.data()
    # Finally, convert back to sparsetsmat.
    if (length(list(...)))
        stop('unexpected ... args')
    nIdxs <- nargs() - 1 - (!missing(value))
    if (nIdxs==1)
        y <- x[i, details=TRUE]
    else if (nIdxs==2)
        y <- x[i, j, details=TRUE]
    else
        stop('unexpected number of indices')
    if (inherits(x$dates, 'Date'))
        y$i <- as.Date(y$i.idx)
    else if (inherits(x$dates, 'Date'))
        y$i <- as.POSIXct(y$i.idx, tz='UTC')
    else
        y$i <- y$i.idx
    # get the values in the same order as the indices in y
    kv <- if (is.null(y$kk)) value else value[y$kk]
    dd <- data.frame(i=y$i, j=x$ids[y$j.idx], old=y$val, new=kv, row.names=NULL, stringsAsFactors=FALSE)
    # Have to be careful looking to delete rows where new!=old
    # because one new value can make another new value relevant.
    # E.g., if exising data is 4,4,4, and the change is to 2,4,4
    # the second 4 becomes relevant.
    # But, if no values are new, can safely return without doing anything
    if (all(is.na(dd$old) == is.na(dd$new) & (is.na(dd$old) | dd$old == dd$new)))
        return(x) # nothing to change -- no new values
    y$i.used <- x$dates[y$val.idx]
    # split into two parts: where indices match existing rows, and where they are new
    i <- y$i == y$i.used
    dde <- dd[i,,drop=FALSE]
    ddn <- dd[!i,,drop=FALSE]
    if (any(i))
        x$values[dde$val.idx] <- kv[i]
    if (nrow(ddn)==0) {
        # no new rows
        return(x)
    } else {
        # have new rows
        return(add.data(x, ddn[,c('i','j','new')]))
    }
}
