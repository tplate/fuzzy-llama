#' Extract.sparsetsmat
#'
#' Extract an ordinary sub-matrix or values from a sparsetsmat object.
#'
#' @rdname sub.sparsetsmat
#'
#' @method [ sparsetsmat
#'
#' @param x a sparsetsmat object
#'
#' @param i the row indices, can be numeric, character, Date
#' or POSIXct, or a 2-column matrix or dataframe (or list)
#' for matrix indexing.
#'
#' @param j the j indices, can be numeric, character or NULL
#' in the case of matrix indexing
#'
#' @param drop logical, default TRUE
#'
#' @param vidx logical If TRUE, return integer index of the
#' unique values, rather than the values themselves.  The
#' returned integers index into \code{values(x)} (the sorted
#' unique values in \code{x}).
#'
#' @param details logical If TRUE, return a list with the
#' normalized indices and values.
#'
#' @param backfill logical If TRUE, use the oldest data for
#' earlier dates.
#'
#' @param Cpp logical If TRUE, use fast C++ code
#'
#' @param naked logical If TRUE do not return dimnames on
#' the result.  This can result is a very large speeed up
#' when there are many rownames compared to returned
#' elements, because constructing rownames involves format
#' date objects which is slow.
#'
#' @param ... required by the generic, but extra arguments
#' not described here are not alowed.
#'
#' @details
#'
#' Rules for interpreting i: \itemize{
#'
#'    \item If the time index is numeric (not a Date or
#' POSIXct), then i is always interpreted as a value, i.e.,
#' i cannot be interpreted as positional in this case.
#' E.g., with a numeric time index, an i value of 17 does
#' not refer to the 17th row, rather it refers to the value
#' 17.  Similarly, with a numeric time index, an i value of
#' -3 does not result in dropping the 3rd row of the matrix,
#' rather it refers to a time value of -3 (and negative
#' numeric time indices can be stored).
#'
#'    \item If the time index is a date, then if i is
#' numeric, it is an index (positional), else if i is
#' character or date, it is a value.  When the time index is
#' a date, a negative i value has the conventional R
#' interpretation as dropping that row from the result.
#'
#'    \item The first column in a matrix index (can be a
#' dataframe) is interpreted in the same way.
#'
#' }
#'
#' @note Matrix-indexing for \code{sparsetsmat} objects is
#' slightly more verstatile than for standard matrix objects
#' in R in that \code{sparsetsmat} objects can take a
#' dataframe for the matrix index, allowing the use of
#' different datatypes for specifying the indices on the
#' rows and columns.
#'
'[.sparsetsmat' <- function(x, i, j, ..., drop=TRUE, vidx=FALSE, details=FALSE, backfill=x$backfill, Cpp=TRUE, naked=FALSE) {
    if (length(list(...)))
        stop('unexpected ... args')
    nIdxs <- nargs() - 1 - (!missing(drop)) - (!missing(vidx)) - (!missing(details)) - (!missing(backfill)) - (!missing(Cpp)) - (!missing(naked))
    mat.ind <- FALSE
    i.idx <- NULL
    i.idx.srtd <- FALSE
    if (missing(i)) {
        # avoid unnecessary conversion between dates and numbers
        # because it is quite slow
        i <- x$all.dates
        if (inherits(x$all.dates, 'Date')) {
            i.idx <- as.integer(x$all.dates)
        } else if (inherits(x$all.dates, 'POSIXct')) {
            i.idx <- as.double(x$all.dates)
        } else {
            i.idx <- x$all.dates
        }
        i.idx.srtd <- TRUE
        if (!naked)
            i.dn <- format(x$all.dates)
    }
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
    } else if (is.list(i) && length(i)==2) {
        # list-style matrix indexing
        if (!missing(j))
            stop('cannot supply j when using matrix indexing')
        if (length(i)!=2)
            stop('need just 2 columns in i for matrix indexing')
        j <- i[[2]]
        i <- i[[1]]
        if (length(i) != length(j))
            stop('both vectors in the list must be the same length for list-delivered matrix indexing (found ', length(i), ' & ', length(j), ')')
        mat.ind <- TRUE
        if (nIdxs != 1)
            stop('require just one index with matrix indexing')
    } else {
        if (nIdxs != 2 & nIdxs != 0)
            stop('require neither or both indices (i & j)')
    }
    if (missing(j))
        j <- seq(along=x$ids)
    if (!is.null(dim(j)))
        stop('second index must be a vector')
    if (!is.numeric(i) && !is.character(i) && !is.factor(i) && !is.logical(i)
        && !inherits(i, 'Date') && !inherits(i, 'POSIXt'))
        stop('first index must be numeric, character, factor, Date or POSIXt (is ', class(i), ')')
    if (is.logical(i)) {
        if (mat.ind)
            stop('cannot use logical index in matrix indexing')
        if (length(i)==1)
            i <- rep(NA, length(x$all.dates))
        if (length(i) != length(x$all.dates))
            stop('logical i must be length 1 or ', length(x$all.dates))
        i <- seq(len=length(x$all.dates))[i]
    } else if (is.factor(i)) {
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
        } else if (inherits(i, 'POSIXlt')) {
            if (inherits(x$dates, 'Date'))
                i <- as.Date(i)
            else
                i <- as.POSIXct(i, tz='UTC')
        } else if (inherits(i, 'POSIXct') && inherits(x$dates, 'Date')) {
            i <- as.Date(i)
        }
    }
    if (!is.null(i.idx)) {
        # already calculated i.idx above
    } else if (   (is.numeric(i) | is.integer(i))
        && !(inherits(i, 'Date') | inherits(i, 'POSIXct'))
        && (inherits(x$dates, 'Date') | inherits(x$dates, 'POSIXct'))) {
        if (!is.integer(i) && any(!is.na(i) & abs(i - round(i)) > 1e-4))
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
        if (!mat.ind && !naked)
            i.dn <- format(x$all.dates[i.idx1])
        # translate the index to the numeric value of the date
        i.idx <- as.numeric(x$all.dates)[i.idx1]
    } else if (inherits(x$dates, 'Date')) {
        if (!inherits(i, 'Date'))
            stop('cannot use ', class(i)[1], ' i index with Date indices on x')
        if (!mat.ind && !naked)
            i.dn <- format(i)
        i.idx <- as.integer(i)
    } else if (inherits(x$dates, 'POSIXct')) {
        if (!inherits(i, 'POSIXct'))
            stop('cannot use ', class(i)[1], ' i index with POSIXct indices on x')
        if (!mat.ind && !naked)
            i.dn <- format(i)
        i.idx <- as.numeric(i)
    } else if (is.numeric(x$dates) && class(x$dates)[1]=='numeric') {
        if (!(is.numeric(i) && (class(i)[1]=='numeric' || class(i)[1]=='integer')))
            stop('cannot use ', class(i)[1], ' i index with numeric indices on x')
        if (!mat.ind && !naked)
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
            j <- seq(length(x$ids))[j]
        }
        j.na <- is.na(j)
        if (any(j[!j.na] < 1) || any(j[!j.na] > length(x$ids)))
            stop('j out of range')
        j.idx <- j
        j.dn <- x$ids[j]
    } else if (is.character(j)) {
        j.idx <- match(j, x$ids)
        j.dn <- j
    } else if (is.logical(j)) {
        if (mat.ind)
            stop('cannot use logical index in matrix indexing')
        if (length(j)==1)
            j <- rep(NA, length(x$ids))
        if (length(j) != length(x$ids))
            stop('logical j must be length 1 or ', length(x$ids))
        j.idx <- seq(len=length(x$ids))[j]
        j.dn <- x$ids[j.idx]
    } else if (is.factor(j)) {
        j.idx <- match(levels(j), x$ids)[as.integer(j)]
        j.dn <- as.character(j)
    } else {
        stop('j must be numeric, character, logical, or factor (is ', class(j), ')')
    }
    rule <- if (is.logical(backfill) && backfill) c(2,2) else c(1,2)
    if (mat.ind) {
        # group the j's together
        kk <- order(j.idx, i.idx, na.last=TRUE)
        i.idx <- i.idx[kk]
        j.idx <- j.idx[kk]
        j.idx.unq <- unique(j.idx)
        jj.idx <- c(match(j.idx.unq, j.idx), length(j.idx)+1)
        if (length(j.idx.unq)==0) {
            val.idx <- 0
        } else if (Cpp) {
            if (Cpp==-1)
                val.idx <- stsm_xt_mir(as.double(i.idx), j.idx, x$dates, x$id.idx, x$id.noc, backfill)
            else if (is.double(x$dates))
                val.idx <- stsm_xt_mid(as.double(i.idx), j.idx, x$dates, x$id.idx, x$id.noc, backfill)
            else if (is.integer(x$dates))
                val.idx <- stsm_xt_mii(as.integer(i.idx), j.idx, as.integer(x$dates), x$id.idx, x$id.noc, backfill)
            else
                stop('x$dates is neither integer nor double?')
            if (isTRUE(any(val.idx==0)))
                stop('internal error: have some val.idx==0')
        } else if (length(j.idx.unq)==1) {
            if (is.na(j.idx.unq) || x$id.noc[j.idx.unq] == 0) {
                val.idx <- replace(integer(length(i.idx)), TRUE, NA)
            } else {
                k <- seq(x$id.idx[j.idx.unq], len=x$id.noc[j.idx.unq])
                if (all(is.na(x$values[k]))) {
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
                if (all(is.na(x$values[k])))
                    return(x$values[k[1]])
                ii <- seq(jj.idx[jj], jj.idx[jj+1]-1)
                # need to cope with all NAs or some NAs
                val.idx <- approx(x$dates[k], k, xout=i.idx[ii], method='constant', ties='ordered', rule=rule)$y
                if (min(val.idx, na.rm=T) < 1) stop('internal error')
                if (max(val.idx, na.rm=T) > length(x$values)) stop('internal error')
                return(val.idx)
            }), use.names=FALSE)
        }
        if (details) {
            return(list(mat.idx=TRUE, i.idx=i.idx, j.idx=j.idx,
                        val.idx=val.idx, val=x$values[val.idx], kk=kk))
        }
        if (vidx)
            val <- match(x$values, sort(unique(x$values), na.last=TRUE))[val.idx]
        else
            val <- x$values[val.idx]
        # need to put val back in the right order
        val[kk] <- val
    } else {
        # regular indexing, not matrix indexing
        if (length(i.idx)==0 || length(j.idx)==0) {
            val.idx <- integer(0)
        } else {
            if (!isTRUE(i.idx.srtd)) {
                if (any(i.idx.isna <- is.na(i.idx))) {
                    i.idx.srtd <- isTRUE(all(diff(i.idx[!i.idx.isna]) >= 0))
                } else {
                    i.idx.srtd <- isTRUE(all(diff(i.idx) >= 0))
                }
                if (!i.idx.srtd) {
                    kk <- order(i.idx, na.last=TRUE)
                    i.idx <- i.idx[kk]
                }
            } else {
                kk <- seq(along=i.idx)
            }
            if (Cpp) {
                # call to C++ for fast indexing
                # j.idx indexes into id.idx and id.noc, which have the start and # of rows
                # for the date and id.
                if (is.double(x$dates))
                    val.idx <- stsm_xt_sqd(x$dates, as.double(i.idx), j.idx, x$id.idx, x$id.noc, backfill)
                else if (is.integer(x$dates))
                    val.idx <- stsm_xt_sqi(x$dates, as.integer(i.idx), j.idx, x$id.idx, x$id.noc, backfill)
                else
                    stop('x$dates is neither integer nor double?')
                if (isTRUE(any(val.idx==0)))
                    stop('internal error: have some val.idx==0')
            } else if (length(j.idx)==1) {
                # single column (could be single element)
                if (x$id.noc[j.idx] == 0) {
                    val.idx <- replace(integer(length(i.idx)), TRUE, NA)
                } else {
                    k <- seq(x$id.idx[j.idx], len=x$id.noc[j.idx])
                    # need to cope with all NAs or some NAs, in the interpolated values
                    # so can't use approx directly
                    val.idx <- approx(x$dates[k], k, xout=i.idx, method='constant', ties='ordered', rule=rule)$y
                }
            } else {
                # multiple cols (could be single or multiple rows)
                val.idx <- unlist(lapply(seq(along=j.idx), function(jj) {
                    if (is.na(j.idx[jj]))
                        return(replace(integer(length(i.idx)), TRUE, NA))
                    if (x$id.noc[j.idx[jj]] == 0)
                        return(replace(integer(length(i.idx)), TRUE, NA))
                    k <- seq(x$id.idx[j.idx[jj]], len=x$id.noc[j.idx[jj]])
                    # need to cope with all NAs or some NAs, in the interpolated values
                    # so can't use approx directly
                    val.idx <- approx(x$dates[k], k, xout=i.idx, method='constant', ties='ordered', rule=rule)$y
                    return(val.idx)
                }), use.names=FALSE)
            }
            if (!i.idx.srtd) {
                # return i.idx to its original order and val.idx correspondingly
                # when kk = order(x) the reverse of x[kk] is replace(x, kk, x[kk])
                i.idx <- replace(1L, kk, i.idx)
                val.idx <- replace(1L, rep(seq(0,len=length(j.idx), by=length(i.idx)), each=length(i.idx)) + kk, val.idx)
            }
        }
        if (details)
            return(list(mat.idx=FALSE, i.idx=i.idx, j.idx=j.idx,
                        val.idx=val.idx, val=x$values[val.idx], k=seq(along=val.idx)))
        if (vidx) {
            val <- match(x$values, sort(unique(x$values), na.last=TRUE))[val.idx]
        } else {
            val <- x$values[val.idx]
            # if (!is.null(x$default) && !is.na(x$default)) val[which(is.na(val.idx))] <- x$default
        }
        # attach dimensions and names
        if (drop && length(i)==1) {
            if (length(j)>1)
                names(val) <- j.dn
        } else if (drop && length(j)==1) {
            if (length(i)>1 && !naked)
                names(val) <- i.dn
        } else {
            attr(val, 'dim') <- c(length(i.idx), length(j.idx))
            if (!naked)
                attr(val, 'dimnames') <- list(i.dn, j.dn)
        }
    }
    return(val)
}

stsm_xt_mir <- function(i.idx, j.idx, dates, id.idx, id.noc, backfill) {
    # Calculate indices into compact data for matrix-style indices,
    # i.e., length(i.idx)==length(j.idx) and returned value is a
    # vector of the same length of indices into compact data so that
    # retval[k] is the index of the value for x[i.idx[k], j.idx[i]].
    #
    # This is a non-vectorized version for translation to Rcpp.
    # Assumptions about input variables:
    # The values in j.idx are grouped and are indices into id.idx and id.noc.
    # The values in id.idx are indices into dates.
    # The values in id.noc are the number of dates for that id.
    # The values in i.idx are ascending sorted within groups of j.idx and are
    # directly comparable with the values in dates
    # Both i.idx and dates are sorted in groups of ids.
    # The return values are indices into dates.
    # Initialize the result with NA values.
    val.idx <- rep(as.integer(0), length(i.idx))
    i <- 1 # index into i.idx and j.idx
    while (i <= length(i.idx)) {
        j <- j.idx[i]
        # Process all of this id group together
        if (id.noc[j] == 0) {
            # No data for this id at all
            while (i <= length(i.idx) && j.idx[i] == j) {
                val.idx[i] <- NA
                i <- i+1
            }
        } else {
            k <- s <- id.idx[j]
            e <- s + id.noc[j] - 1
            d <- dates[k]
            while (i <= length(i.idx) && j.idx[i] == j) {
                # increase k until dates[k] is the largest still
                # smaller than i.idx[i]
                while (k < e && dates[k+1] <= i.idx[i])
                    k <- k + 1
                if (backfill || i.idx[i] >= dates[k])
                    val.idx[i] <- k
                else
                    val.idx[i] <- NA
                i <- i+1
            }
        }
    }
    val.idx
}
#' sparsetsmat method for generic lookup.arr
#' @method lookup.arr sparsetsmat
#'
#' @note The lookup.arr.sparsetsmat method is an alias for the standard extract method
#'
#' @param x a sparsetsmat object
#' @param i a matrix index
#' @param ... unused
#' @param drop should dimensions be dropped from a vector result

lookup.arr.sparsetsmat <- function(x, i, ..., drop=TRUE) `[.sparsetsmat`(x=x, i=i, ..., drop=drop)

#' Lookup values in an array
#'
#' @param x a matrix-like object
#' @param i a matrix index
#' @param ... additional arguments to pass to methods

lookup.arr <- function(x, i, ...) UseMethod('lookup.arr')

