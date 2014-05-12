#' @details Rules for interpreting i:
#' If the time index is numeric, then i is a value.
#' If the time index is a date, then if i is numeric, it is an index, else if i is character or date, it is a value.
#' The first column in a matrix index (can be a dataframe) is interpreted in the same way.
'[.sparsetsmat' <- function(x, i, j, ..., drop=TRUE) {
    mat.ind <- FALSE
    if (missing(i))
        i <- x$all.dates
    if (!is.null(dim(i)) && length(dim(i))>1) {
        # matrix indexing
        if (!missing(j))
            stop('cannot supply j when using matrix indexing')
        if (length(dim(i))!=2)
            stop('need 2-d i for matrix indexing')
        if (ncol(i)!=2)
            stop('need just 2 columns in i for matrix indexing')
        j <- i[,2,drop=TRUE]
        i <- i[,1,drop=TRUE]
        mat.ind <- TRUE
    }
    if (missing(j))
        j <- seq(along=x$id)
    if (!is.numeric(i) && !is.character(i) && !inherits(i, 'Date') && !inherits(i, 'POSIXt'))
        stop('first index must be numeric, character, Date or POSIXt')
    if (!is.null(dim(j)))
        stop('second index must be a vector')
    # Align date types of i and date-index in x
    if (is.character(i) && inherits(x$date, 'Date')) {
        i <- as.Date(i)
    } else if (is.character(i) && inherits(x$date, 'POSIXct')) {
        i <- as.POSIXct(i)
    } else if (inherits(i, 'POISXlt')) {
        if (inherits(x$date, 'Date'))
            i <- as.Date(i)
        else
            i <- as.POSIXct(i)
    } else if (inherits(i, 'POISXct') && inherits(x$date, 'Date')) {
        i <- as.Date(i)
    }
    if (   (is.numeric(i) | is.integer(i))
        && (inherits(x$date, 'Date') | inherits(x$date, 'POSIXct'))) {
        if (!is.integer(i) & any(abs(i - round(i)) > 1e-4))
            stop('non-wholenumber numeric i index with date indices on x')
        if (any(i < 0)) {
            # handle -ve indices
            if (any(i > 0))
                stop('cannot mix +ve and -ve numbers in numeric i index')
            i <- seq(nrow(length(x$all.dates)))[i]
        }
        if (any(!is.na(i) & (i < 1 | i > length(x$all.dates))))
            stop('numeric i index values out of range')
        i.idx1 <- if (is.integer(i)) i else as.integer(i)
        if (!mat.ind)
            i.dn <- format(x$all.dates[i.idx1])
        # translate the index to the numeric value of the date
        i.idx <- as.numeric(x$all.dates)[i.idx1]
    } else if (inherits(x$date, 'Date')) {
        if (!inherits(i, 'Date'))
            stop('cannot use ', class(i)[1], ' i index with Date indices on x')
        if (!mat.ind)
            i.dn <- format(i)
        i.idx <- as.integer(i)
    } else if (inherits(i, 'POISXct')) {
        if (!inherits(i, 'POSIXct'))
            stop('cannot use ', class(i)[1], ' i index with POSIXct indices on x')
        if (!mat.ind)
            i.dn <- format(i)
        i.idx <- as.numeric(i)
    } else if (is.numeric(x$date) && class(x$date)[1]=='numeric') {
        if (!(is.numeric(i) && (class(i)[1]=='numeric' || class(i)[1]=='integer')))
            stop('cannot use ', class(i)[1], ' i index with numeric indices on x')
        if (!mat.ind)
            i.dn <- as.character(i)
        i.idx <- i
    } else {
        stop('nyi: must try to match class of i to class of date in x')
    }
    if (is.numeric(j)) {
        if (any(j < 1) || any(j > length(x$id)))
            stop('j out of range')
        j.idx <- j
        j.dn <- x$id[j]
    } else if (is.character(j)) {
        j.idx <- match(j, x$id)
        j.dn <- j
    } else {
        stop('j must be numeric or character')
    }
    rule <- if (x$backfill) c(2,2) else c(1,2)
    if (mat.ind) {
        # group the j's together
        kk <- order(j.idx, i.idx, na.last=TRUE)
        i.idx <- i.idx[kk]
        j.idx <- j.idx[kk]
        j.idx.unq <- unique(j.idx)
        jj.idx <- c(match(j.idx.unq, j.idx), length(j.idx)+1)
        if (length(j.idx.unq)==0) {
            val <- x$value[0]
        } else if (length(j.idx.unq)==1) {
            if (is.na(j.idx.unq)) {
                val <- replace(rep(x$value[1], length(i.idx)), TRUE, NA)
            } else if (x$id.idx[j.idx.unq] == x$id.idx[j.idx.unq+1]) {
                val <- replace(rep(x$value[1], length(i.idx)), TRUE, NA)
            } else {
                k <- seq(x$id.idx[j.idx.unq], x$id.idx[j.idx.unq+1]-1)
                if (all(is.na(x$value[k]))) {
                    val <- x$value[k[1]]
                } else {
                    # need to cope with all NAs or some NAs
                    # do it by making the output of approx() the index into val
                    val.idx <- approx(x$date[k], k, xout=i.idx, method='constant', ties='ordered', rule=rule)$y
                    val <- x$value[val.idx]
                }
            }
        } else {
            val <- unlist(lapply(seq(along=j.idx.unq), function(jj) {
                # somewhere in here I'm indexing with a double that
                # is outside of the range of 32 bit int...
                if (is.na(j.idx.unq[jj]))
                    return(rep(NA, jj.idx[jj+1] - jj.idx[jj]))
                if (x$id.idx[j.idx.unq[jj]] == x$id.idx[j.idx.unq[jj]+1])
                    return(rep(NA, jj.idx[jj+1] - jj.idx[jj]))
                k <- seq(x$id.idx[j.idx.unq[jj]], x$id.idx[j.idx.unq[jj]+1]-1)
                if (all(is.na(x$value[k])))
                    return(x$value[k[1]])
                ii <- seq(jj.idx[jj], jj.idx[jj+1]-1)
                # need to cope with all NAs or some NAs
                val.idx <- approx(x$date[k], k, xout=i.idx[ii], method='constant', ties='ordered', rule=rule)$y
                if (min(val.idx, na.rm=T) < 1) stop('internal error')
                if (max(val.idx, na.rm=T) > length(x$value)) stop('internal error')
                return(x$value[val.idx])
            }), use.names=FALSE)
        }
        # need to put val back in the right order
        val[kk] <- val
    } else {
        # regular indexing, not matrix indexing
        if (length(i.idx)==0 || length(j.idx)==0) {
            val <- x$value[0]
        } else if (length(j.idx)==1) {
            # single column (could be single element)
            if (x$id.idx[j.idx] == x$id.idx[j.idx+1]) {
                val <- rep(NA, length(i.idx))
            } else {
                k <- seq(x$id.idx[j.idx], x$id.idx[j.idx+1]-1)
                if (all(is.na(x$value[k]))) {
                    val <- x$value[k[1]]
                } else {
                    # need to cope with all NAs or some NAs
                    val.idx <- approx(x$date[k], k, xout=i.idx, method='constant', ties='ordered', rule=rule)$y
                    val <- x$value[val.idx]
                }
            }
        } else {
            # multiple cols (could be single or multiple rows)
            val <- unlist(lapply(seq(along=j.idx), function(jj) {
                if (is.na(j.idx[jj]))
                    return(rep(NA, length(i.idx)))
                if (x$id.idx[j.idx[jj]] == x$id.idx[j.idx[jj]+1])
                    return(rep(NA, length(i.idx)))
                k <- seq(x$id.idx[j.idx[jj]], x$id.idx[j.idx[jj]+1]-1)
                if (all(is.na(x$value[k])))
                    return(x$value[k[1]])
                # need to cope with all NAs or some NAs
                val.idx <- approx(x$date[k], k, xout=i.idx, method='constant', ties='ordered', rule=rule)$y
                return(x$value[val.idx])
            }), use.name=FALSE)
        }
        # attach dimensions and names
        if (drop && length(i)==1) {
            if (length(j)>1)
                names(val) <- j.dn
        } else if (drop && length(j)==1) {
            if (length(i)>1)
                names(val) <- i.dn
        } else {
            attr(val, 'dim') <- c(length(i.idx), length(j.idx))
            attr(val, 'dimnames') <- list(i.dn, j.dn)
        }
    }
    return(val)
}
