# obselete
xtfast.obselete <- function(x, i, j, ..., drop=TRUE, vidx=FALSE) {
    if (length(list(...)))
        stop('unexpected ... args')
    nIdxs <- nargs() - 1 - (!missing(drop)) - (!missing(vidx))
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
        if (nIdxs != 1)
            stop('require just one index with matrix indexing')
    } else {
        if (nIdxs != 2 & nIdxs != 0)
            stop('require zero or both indices (i & j)')
    }
    if (missing(j))
        j <- seq(along=x$id)
    if (!is.null(dim(j)))
        stop('second index must be a vector')
    if (!is.numeric(i) && !is.character(i) && !is.factor(i) && !inherits(i, 'Date') && !inherits(i, 'POSIXt'))
        stop('first index must be numeric, character, factor, Date or POSIXt (is ', class(i), ')')
    if (is.factor(i)) {
        if (inherits(x$dates, 'Date')) {
            i <- as.Date(levels(i))[as.integer(i)]
        } else if (inherits(x$dates, 'POSIXct')) {
            i <- as.POSIXct(levels(i), tz='UTC')[as.integer(i)]
        }
    } else {
        # Align date types of i and date-index in x
        if (is.character(i) && inherits(x$dates, 'Date')) {
            # only parse unique values
            i <- with(list(iu=unique(i)), as.Date(iu)[match(i, iu)])
        } else if (is.character(i) && inherits(x$dates, 'POSIXct')) {
            # only parse unique values
            i <- with(list(iu=unique(i)), as.POSIXct(iu, tz='UTC')[match(i, iu)])
        } else if (inherits(i, 'POISXlt')) {
            if (inherits(x$dates, 'Date'))
                i <- as.Date(i)
            else
                i <- as.POSIXct(i, tz='UTC')
        } else if (inherits(i, 'POSIXct') && inherits(x$dates, 'Date')) {
            i <- as.Date(i)
        }
    }
    if (   (is.numeric(i) | is.integer(i))
        && !(inherits(i, 'Date') | inherits(i, 'POSIXct'))
        && (inherits(x$dates, 'Date') | inherits(x$dates, 'POSIXct'))) {
        if (!is.integer(i) & any(abs(i - round(i)) > 1e-4))
            stop('non-wholenumber numeric i index with date indices on x')
        if (isTRUE(any(i < 0))) {
            # handle -ve indices
            if (isTRUE(any(i > 0)))
                stop('cannot mix +ve and -ve numbers in numeric i index')
            i <- seq(length(x$all.dates))[i]
        }
        if (any(!is.na(i) & (i < 1 | i > length(x$all.dates))))
            stop('numeric i index values out of range')
        i.idx1 <- if (is.integer(i)) i else as.integer(i)
        if (!mat.ind)
            i.dn <- format(x$all.dates[i.idx1])
        # translate the index to the numeric value of the date
        if (inherits(x$dates, 'Date'))
            i.idx <- as.integer(x$all.dates)[i.idx1]
        else
            i.idx <- as.numeric(x$all.dates)[i.idx1]
    } else if (inherits(x$dates, 'Date')) {
        if (!inherits(i, 'Date'))
            stop('cannot use ', class(i)[1], ' i index with Date indices on x')
        if (!mat.ind)
            i.dn <- format(i)
        i.idx <- as.integer(i)
    } else if (inherits(x$dates, 'POSIXct')) {
        if (!inherits(i, 'POSIXct'))
            stop('cannot use ', class(i)[1], ' i index with POSIXct indices on x')
        if (!mat.ind)
            i.dn <- format(i)
        i.idx <- as.numeric(i)
    } else if (is.numeric(x$dates) && class(x$dates)[1]=='numeric') {
        if (!(is.numeric(i) && (class(i)[1]=='numeric' || class(i)[1]=='integer')))
            stop('cannot use ', class(i)[1], ' i index with numeric indices on x')
        if (!mat.ind)
            i.dn <- as.character(i)
        i.idx <- i
    } else {
        stop('nyi: must try to match class of i (', class(i),
             ') to class of date in x (', class(x$dates), ')')
    }
    if (is.numeric(j)) {
        if (isTRUE(any(j < 0))) {
            # handle -ve indices
            if (isTRUE(any(j > 0)))
                stop('cannot mix +ve and -ve numbers in numeric j index')
            j <- seq(length(x$id))[j]
        }
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
            val.idx <- 0
        } else if (length(j.idx.unq)==1) {
            if (is.na(j.idx.unq) || x$id.noc[j.idx.unq] == 0) {
                val.idx <- replace(integer(length(i.idx)), TRUE, NA)
            } else {
                k <- seq(x$id.idx[j.idx.unq], len=x$id.noc[j.idx.unq])
                if (all(is.na(x$value[k]))) {
                    val.idx <- k[1]
                } else {
                    # need to cope with all NAs or some NAs
                    # do it by making the output of approx() the index into val
                    val.idx <- approx(x$dates[k], k, xout=i.idx, method='constant', ties='ordered', rule=rule)$y
                }
            }
        } else {
            val.idx <- unlist(lapply(seq(along=j.idx.unq), function(jj) {
                # somewhere in here I'm indexing with a double that
                # is outside of the range of 32 bit int...
                if (x$id.noc[j.idx.unq[jj]] == 0)
                    return(rep(NA, jj.idx[jj+1] - jj.idx[jj]))
                k <- seq(x$id.idx[j.idx.unq[jj]], len=x$id.noc[j.idx.unq[jj]])
                if (all(is.na(x$value[k])))
                    return(x$value[k[1]])
                ii <- seq(jj.idx[jj], jj.idx[jj+1]-1)
                # need to cope with all NAs or some NAs
                val.idx <- approx(x$dates[k], k, xout=i.idx[ii], method='constant', ties='ordered', rule=rule)$y
                if (min(val.idx, na.rm=T) < 1) stop('internal error')
                if (max(val.idx, na.rm=T) > length(x$value)) stop('internal error')
                return(val.idx)
            }), use.names=FALSE)
        }
        if (vidx)
            val <- match(x$value, sort(unique(x$value)))[val.idx]
        else
            val <- x$value[val.idx]
        # need to put val back in the right order
        val[kk] <- val
    } else {
        # regular indexing, not matrix indexing
        # j.idx indexes into id.idx and id.noc, which have the start and # of rows
        # for the date and id.
        if (is.double(x$dates))
            val.idx <- stsm_xt_sqd(x$dates, as.double(i.idx), j.idx, x$id.idx, x$id.noc)
        else if (is.integer(x$dates))
            val.idx <- stsm_xt_sqi(as.integer(x$dates), as.integer(i.idx), j.idx, x$id.idx, x$id.noc)
        else
            stop('x$dates is neither integer nor double?')
        if (isTRUE(any(val.idx==0)))
            stop('internal error: have some val.idx==0')
        if (vidx)
            val <- match(x$value, sort(unique(x$value)))[val.idx]
        else
            val <- x$value[val.idx]
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
