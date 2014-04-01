plus.ocf <- function(x, y) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(x + y)
    } else {
        r <- conform(list(x=x, y=y), fill=0, do.all='union')
        return(r$x + r$y)
    }
}
`%+ocf%` <- plus.ocf

plus.icf <- function(x, y) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(x + y)
    } else {
        r <- conform(list(x=x, y=y), fill=0, do.all='intersect')
        return(r$x + r$y)
    }
}
`%+icf%` <- plus.icf

plus.lcf <- function(x, y) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(x + y)
    } else {
        if (is.virtual.array(x))
            x <- M(x)
        return(x + conform(y, x, fill=0))
    }
}
`%+lcf%` <- plus.lcf

plus.rcf <- function(x, y) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(x + y)
    } else {
        if (is.virtual.array(y))
            y <- M(y)
        return(conform(x, y, fill=0) + y)
    }
}
`%+rcf%` <- plus.rcf

times.ocf <- function(x, y) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(x * y)
    } else {
        r <- conform(list(x=x, y=y), fill=0, do.all='union')
        return(r$x * r$y)
    }
}
`%*ocf%` <- times.ocf

times.icf <- function(x, y) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(x * y)
    } else {
        r <- conform(list(x=x, y=y), fill=0, do.all='intersect')
        return(r$x * r$y)
    }
}
`%*icf%` <- times.icf

times.lcf <- function(x, y) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(x * y)
    } else {
        if (is.virtual.array(x))
            x <- M(x)
        return(x * conform(y, x, fill=0))
    }
}
`%*lcf%` <- times.lcf

times.rcf <- function(x, y) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(x * y)
    } else {
        if (is.virtual.array(y))
            y <- M(y)
        return(conform(x, y, fill=0) * y)
    }
}
`%*rcf%` <- times.rcf

minus.ocf <- function(x, y) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(x - y)
    } else {
        r <- conform(list(x=x, y=y), fill=0, do.all='union')
        return(r$x - r$y)
    }
}
`%-ocf%` <- minus.ocf

minus.icf <- function(x, y) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(x - y)
    } else {
        r <- conform(list(x=x, y=y), fill=0, do.all='intersect')
        return(r$x - r$y)
    }
}
`%-icf%` <- minus.icf

minus.lcf <- function(x, y) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(x - y)
    } else {
        if (is.virtual.array(x))
            x <- M(x)
        return(x - conform(y, x, fill=0))
    }
}
`%+lcf%` <- minus.lcf

minus.rcf <- function(x, y) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(x - y)
    } else {
        if (is.virtual.array(y))
            y <- M(y)
        return(conform(x, y, fill=0) - y)
    }
}
`%+rcf%` <- minus.rcf

pmax.ocf <- function(x, y, na.rm=TRUE) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(pmax(x, y))
    } else {
        r <- conform(list(x=x, y=y), fill=NA, do.all='union')
        return(pmax(r$x, r$y, na.rm=na.rm))
    }
}

pmax.icf <- function(x, y, na.rm=TRUE) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(pmax(x, y))
    } else {
        r <- conform(list(x=x, y=y), fill=NA, do.all='intersect')
        return(pmax(r$x, r$y, na.rm=na.rm))
    }
}

pmax.lcf <- function(x, y) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(pmax(x, y))
    } else {
        if (is.virtual.array(x))
            x <- M(x)
        return(pmax(x, conform(y, x, fill=0)))
    }
}

pmax.rcf <- function(x, y) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(pmax(x, y))
    } else {
        if (is.virtual.array(y))
            y <- M(y)
        return(pmax(conform(x, y, fill=0), y))
    }
}

pmin.ocf <- function(x, y, na.rm=TRUE) {
    if (same.dimnames(x, y))
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(pmin(x, y))
    r <- conform(list(x=x, y=y), fill=NA, do.all='union')
    return(pmin(r$x, r$y, na.rm=na.rm))
}

pmin.icf <- function(x, y, na.rm=TRUE) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(pmin(x, y))
    } else {
        r <- conform(list(x=x, y=y), fill=NA, do.all='intersect')
        return(pmin(r$x, r$y, na.rm=na.rm))
    }
}

pmin.lcf <- function(x, y) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(pmin(x, y))
    } else {
        if (is.virtual.array(x))
            x <- M(x)
        return(pmin(x, conform(y, x, fill=0)))
    }
}

pmin.rcf <- function(x, y) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(pmin(x, y))
    } else {
        if (is.virtual.array(y))
            y <- M(y)
        return(pmin(conform(x, y, fill=0), y))
    }
}

pmex.ocf <- function(x, y, na.rm=TRUE) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(pmex(x, y))
    } else {
        r <- conform(list(x=x, y=y), fill=NA, do.all='union')
        return(pmex(r$x, r$y, na.rm=na.rm))
    }
}

pmex.icf <- function(x, y, na.rm=TRUE) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(pmex(x, y))
    } else {
        r <- conform(list(x=x, y=y), fill=NA, do.all='intersect')
        return(pmex(r$x, r$y, na.rm=na.rm))
    }
}

pmex.lcf <- function(x, y) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(pmex(x, y))
    } else {
        if (is.virtual.array(x))
            x <- M(x)
        return(pmex(x, conform(y, x, fill=0)))
    }
}

pmex.rcf <- function(x, y) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(pmex(x, y))
    } else {
        if (is.virtual.array(y))
            y <- M(y)
        return(pmex(conform(x, y, fill=0), y))
    }
}

plex.ocf <- function(x, y, na.rm=TRUE) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(plex(x, y))
    } else {
        r <- conform(list(x=x, y=y), fill=NA, do.all='union')
        return(plex(r$x, r$y, na.rm=na.rm))
    }
}

plex.icf <- function(x, y, na.rm=TRUE) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(plex(x, y))
    } else {
        r <- conform(list(x=x, y=y), fill=NA, do.all='intersect')
        return(plex(r$x, r$y, na.rm=na.rm))
    }
}

plex.lcf <- function(x, y) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(plex(x, y))
    } else {
        if (is.virtual.array(x))
            x <- M(x)
        return(plex(x, conform(y, x, fill=0)))
    }
}

plex.rcf <- function(x, y) {
    if (same.dimnames(x, y)) {
        if (is.virtual.array(x))
            x <- M(x)
        if (is.virtual.array(y))
            y <- M(y)
        return(plex(x, y))
    } else {
        if (is.virtual.array(y))
            y <- M(y)
        return(plex(conform(x, y, fill=0), y))
    }
}

pmex <- function(x, y, na.rm=TRUE) {
    # return the most extreme, according to sign of x
    ifelse(is.true(x==0 & y!=0), y,
           ifelse(is.true(x >= 0), pmax(x, y, na.rm=na.rm), pmin(x, y, na.rm=na.rm)))
}

plex <- function(x, y, na.rm=TRUE) {
    # return the least extreme, or zero if x and y have different sign
    if (na.rm && any(is.na(x)))
        x <- replace.na(x)
    if (na.rm && any(is.na(y)))
        y <- replace.na(y)
    ifelse(is.true(x==0 | y==0) | is.true(sign(x) != sign(y)), 0,
           ifelse(is.true(x >= 0), pmin(x, y, na.rm=na.rm), pmax(x, y, na.rm=na.rm)))
}

pmin.safe <- function(..., na.rm=T) {
    if (length(dim(..1))) {
        if (length(dim(..2)))
            if (length(dim(..1)) != length(dim(..2)))
                stop('1st and 2nd args are matrices of different rank')
            else if (!all(dim(..1) == dim(..2)))
                stop('1st and 2nd args have different dimensions:', paste(dim(..1), collapse=' x '), ' and ', paste(dim(..2), collapse=' x '))
        else if (length(..2)!=1 && length(..2) != lastelt(dim(..1)))
            stop('2nd arg is not an array and has length different to 1 or length of last dim of 1st arg')
    }
    pmin(..., na.rm=na.rm)
}

pmax.safe <- function(..., na.rm=T) {
    if (length(dim(..1))) {
        if (length(dim(..2)))
            if (length(dim(..1)) != length(dim(..2)))
                stop('1st and 2nd args are matrices of different rank')
            else if (!all(dim(..1) == dim(..2)))
                stop('1st and 2nd args have different dimensions:', paste(dim(..1), collapse=' x '), ' and ', paste(dim(..2), collapse=' x '))
        else if (length(..2)!=1 && length(..2) != lastelt(dim(..1)))
            stop('2nd arg is not an array and has length different to 1 or length of last dim of 1st arg')
    }
    pmax(..., na.rm=na.rm)
}

sub.xts <- function(x, i, j=NULL, ..., dim.time=1, drop=FALSE, extend=NULL) {
    # Subscript an array using dates/times as indices on regular dimnames, but using xts capabilities
    # for date specifications.
    # Also allow extending the date index range by a count of how many extra values to include
    # E.g., extend=c(3,2) adds 3 extra elements at the beginning, and 2 extra at the end.
    #   (extend=2 will add 2 at each end)
    # Note that dates that do not match exactly are silently ignored
    if (length(dim(x))) {
        if (!in.range(dim.time, 1, length(dim(x))))
            stop('dim.time ', dim.time, ' is out of range')
        d <- dim(x)[dim.time]
        dn <- dimnames(x)[[dim.time]]
    } else if (dim.time==1) {
        d <- length(x)
        dn <- names(x)
    } else {
        stop('must have dim.time=1 for non-array x')
    }
    # to use dim.time, need to have it apply to j or ...
    if (dim.time != 1)
        stop('not yet implemented for dim.time != 1')
    if (length(list(...)))
        stop('not yet implemented for more indices than i,j')
    if (length(dim(x)) < 2 && !is.null(j))
        stop('cannot use j when length(dim(x)) < 2')
    y <- xts(seq(len=d), dateParse(dn))
    # ii <- drop(coredata(y[i,]))
    ii <- y[i,,which.i=TRUE]
    if (is.null(ii))
        ii <- integer(0)
    if (length(extend) && length(ii) && any(!is.na(ii))) {
        extend <- rep(extend, length.out=2)
        if (extend[1]>0)
            ii <- c(seq(to=min(ii, na.rm=TRUE)-1, length=min(extend[1], min(ii, na.rm=TRUE)-1)), ii)
        if (extend[2]>0)
            ii <- c(ii, seq(max(ii, na.rm=TRUE)+1, length=min(extend[2], d-max(ii, na.rm=TRUE))))
    }
    if (is.null(dim(x))) {
        return(x[ii])
    } else if (length(dim(x))==1) {
        return(x[ii])
    } else if (length(dim(x))==2) {
        if (is.null(j))
            x <- x[ii,,drop=drop]
        else
            x <- x[ii,j,drop=drop]
        return(x)
    } else if (length(dim(x))==3) {
        if (is.null(j))
            x <- x[ii,,,drop=drop]
        else
            x <- x[ii,j,,drop=drop]
        return(x)
    } else {
        if (is.null(j))
            x <- asub(x, list(ii), dims=1, drop=drop)
        else
            x <- asub(x, list(ii, j), dims=1:2, drop=drop)
        return(x)
    }
    stop("cannot handle dim.time=", dim.time, " and length(dim(x))=", length(dim(x)))
}

to.xts <- function(x, index.col=NULL) {
    # works for data.frame and matrix
    # default is to try to parse the rownames as date/time
    if (length(dim(x)) > 2)
        stop('must have length(dim(x)) < 2')
    if (is.null(index.col)) {
        if (length(dim(x))>0)
            d <- rownames(x)
        else
            d <- names(x)
        if (is.null(d))
            stop('no suitable dimnames on x')
    } else {
        if (length(dim(x))!=2)
            stop('must have length(dim(x))==2 when supplying index.col')
        if (is.character(index.col)) {
            index.col <- match(index.col, colnames(x))
            if (is.na(index.col))
                stop('cannot find index.col in x')
        }
        if (!is.numeric(index.col))
            stop('index.col must be numeric or column name')
        if (index.col < 1 || index.col > ncol(x))
            stop('index.col is out of range')
        d <- x[, index.col, drop=TRUE]
        if (is.matrix(x))
            x <- x[, -index.col, drop=FALSE]
        else if (is.data.frame(x))
            x <- x[-index.col]
        else
            stop('x must be a data.frame or matrix')
    }
    # dateParse does a better job of parsing the data than xts
    if (is.character(d))
        d <- dateParse(d)
    xts(x, d)
}
ifelse.safe <- function(test, yes, no) {
    if (is.null(dim(test))) {
        if (length(yes) > 1 && length(yes)!=length(test))
            stop("length of 'yes' does not match length of 'test'")
        if (length(no) > 1 && length(no)!=length(test))
            stop("length of 'no' does not match length of 'test'")
    } else {
        if (!is.null(dim(yes)) && !equiv(dim(yes), dim(test)))
            stop("dimensions of 'yes' does not match dimensions of 'test'")
        if (!is.null(dim(no)) && !equiv(dim(no), dim(test)))
            stop("dimensions of 'no' does not match dimensions of 'test'")
    }
    ifelse(test, yes, no)
}
pmin.safe <- function(..., na.rm=T) {
    if (length(dim(..1))) {
        if (length(dim(..2)))
            if (length(dim(..1)) != length(dim(..2)))
                stop('1st and 2nd args are matrices of different rank')
            else if (!all(dim(..1) == dim(..2)))
                stop('1st and 2nd args have different dimensions:', paste(dim(..1), collapse=' x '), ' and ', paste(dim(..2), collapse=' x '))
        else if (length(..2)!=1 && length(..2) != lastelt(dim(..1)))
            stop('2nd arg is not an array and has length different to 1 or length of last dim of 1st arg')
    }
    pmin(..., na.rm=na.rm)
}

pmax.safe <- function(..., na.rm=T) {
    if (length(dim(..1))) {
        if (length(dim(..2)))
            if (length(dim(..1)) != length(dim(..2)))
                stop('1st and 2nd args are matrices of different rank')
            else if (!all(dim(..1) == dim(..2)))
                stop('1st and 2nd args have different dimensions:', paste(dim(..1), collapse=' x '), ' and ', paste(dim(..2), collapse=' x '))
        else if (length(..2)!=1 && length(..2) != lastelt(dim(..1)))
            stop('2nd arg is not an array and has length different to 1 or length of last dim of 1st arg')
    }
    pmax(..., na.rm=na.rm)
}
replace.safe <- function(x, list, values) {
    # safe version of replace() checks that logical indices in 'list' conform with x,
    # and that numerical indices are within range
    if (is.logical(list)) {
        if (!equiv(dim(x), dim(list)))
            stop("x does not conform with indices")
        if (length(x) != length(list))
            stop("x is not the same length as indices")
    } else if (is.numeric(list)) {
        if (is.null(dim(list)) || length(dim(list)==1)) {
            if (isTRUE(any(abs(list) > length(x))))
                stop("numeric indices are out of bounds")
        } else {
            if (is.null(dim(x)))
                stop("cannot use matrix-style indexing on a vector")
            if (length(dim(list))!=2)
                stop("can only index with 1 or 2-d index object")
            if (ncol(list) != length(dim(x)))
                stop("ncol(list) != length(dim(x))")
            for (i in seq(len=ncol(list))) {
                if (isTRUE(any(abs(list[,i]) > dim(x)[i])))
                    stop("numeric matrix indices are out of bounds in column ", i)
            }
        }
    }
    return(replace(x, list, values))
}
scale.safe <- function(x, center = T, scale = T) {
    # scale and/or center the columns of x without introducing NA's or infinities
    if (center)
        x <- x %-row% colMeans(x, na.rm=T)
    if (scale)
        x <- x %*row% (1/replace.value(colStdevs(x, na.rm=T), c(0,NA), 1))
    x
}
# From https://stat.ethz.ch/pipermail/r-help/2002-August/024329.html
colVars <- function(x, na.rm=FALSE, dims=1, unbiased=TRUE, SumSquares=FALSE,
                    twopass=FALSE) {
  if (SumSquares) return(colSums(x^2, na.rm, dims))
  N <- colSums(!is.na(x), FALSE, dims)
  Nm1 <- if (unbiased) N-1 else N
  if (twopass) {x <- if (dims==length(dim(x))) x - mean(x, na.rm=na.rm) else
                     sweep(x, (dims+1):length(dim(x)), colMeans(x,na.rm,dims))}
  (colSums(x^2, na.rm, dims) - colSums(x, na.rm, dims)^2/N) / Nm1
}

rowVars <- function(x, na.rm=FALSE, dims=1, unbiased=TRUE, SumSquares=FALSE,
                    twopass=FALSE) {
  if (SumSquares) return(rowSums(x^2, na.rm, dims))
  N <- rowSums(!is.na(x), FALSE, dims)
  Nm1 <- if (unbiased) N-1 else N
  if (twopass) {x <- if (dims==0) x - mean(x, na.rm=na.rm) else
                     sweep(x, 1:dims, rowMeans(x,na.rm,dims))}
  (rowSums(x^2, na.rm, dims) - rowSums(x, na.rm, dims)^2/N) / Nm1
}

colStdevs <- function(x, ...) sqrt(colVars(x, ...))

rowStdevs <- function(x, ...) sqrt(rowVars(x, ...))

rowMins <- function(x, na.rm=TRUE) {
    if (!na.rm) {
        y <- apply(x, 1, min)
    } else {
        xok <- (rowSums(is.na(x)) < ncol(x))
        y <- x[,1]+NA
        if (any(xok))
            y[xok] <- apply(x[xok,,drop=FALSE], 1, min, na.rm=TRUE)
    }
    y
}
rowMaxs <- function(x, na.rm=TRUE) {
    if (!na.rm) {
        y <- apply(x, 1, max)
    } else {
        xok <- (rowSums(is.na(x)) < ncol(x))
        y <- x[,1]+NA
        if (any(xok))
            y[xok] <- apply(x[xok,,drop=FALSE], 1, max, na.rm=TRUE)
    }
    y
}
colMins <- function(x, na.rm=TRUE) {
    if (!na.rm) {
        y <- apply(x, 1, min)
    } else {
        xok <- (colSums(is.na(x)) < nrow(x))
        y <- x[1,]+NA
        if (any(xok))
            y[xok] <- apply(x[,xok,drop=FALSE], 2, min, na.rm=TRUE)
    }
    y
}
colMaxs <- function(x, na.rm=TRUE) {
    if (!na.rm) {
        y <- apply(x, 1, max)
    } else {
        xok <- (colSums(is.na(x)) < nrow(x))
        y <- x[1,]+NA
        if (any(xok))
            y[xok] <- apply(x[,xok,drop=FALSE], 2, max, na.rm=TRUE)
    }
    y
}
plus.row <- function(x, y)
{
    # The tricky stuff with the arg.names argument is so it won't
    # be evaluated unless we need it in an error message
    # The if condition implements the following logic:
    # swap <- F
    # if (is.matrix(y))
    #     if (!is.matrix(x))
    #         swap <- T
    #     else if (all(dim(y)!=1) && any(dim(x)==1))
    #         swap <- T
    # if (swap) ...
    arg.names <- c(deparse(substitute(x)), deparse(substitute(y)))
    if (is.matrix(y) && (!is.matrix(x) || (all(dim(y)!=1) && any(dim(x)==1)))) {
        tt <- y
        y <- x
        x <- tt
        arg.order <- 2:1
    } else {
        arg.order <- 1:2
    }
    arg.names <- arg.names[arg.order]
    if (!is.matrix(x))
        stop("must have one vector argument and one matrix argument")
    if (is.array(y) && !(is.matrix(y) && nrow(y)==1))
        stop(arg.names[2], " must be just a vector (or a matrix with one row)")
    if (ncol(x) != length(y))
        stop("the arguments do not conform: ", arg.names[1], " has ", ncol(x),
             " cols, and ", arg.names[2], " has ", length(y), " elts")
    return(x + matrix(y, byrow = T, nrow = nrow(x), ncol = length(y)))
}

# Operator version
"%+row%" <- plus.row

minus.row <- function(x, y)
{
    # The tricky stuff with the arg.names argument is so it won't
    # be evaluated unless we need it in an error message
    # The if condition implements the following logic:
    # swap <- F
    # if (is.matrix(y))
    #     if (!is.matrix(x))
    #         swap <- T
    #     else if (all(dim(y)!=1) && any(dim(x)==1))
    #         swap <- T
    # if (swap) ...
    arg.names <- c(deparse(substitute(x)), deparse(substitute(y)))
    if (is.matrix(y) && (!is.matrix(x) || (all(dim(y)!=1) && any(dim(x)==1)))) {
        tt <- y
        y <- x
        x <- tt
        arg.order <- 2:1
    } else {
        arg.order <- 1:2
    }
    arg.names <- arg.names[arg.order]
    if (!is.matrix(x))
        stop("must have one vector argument and one matrix argument")
    if (is.array(y) && !(is.matrix(y) && nrow(y)==1))
        stop(arg.names[2], " must be just a vector (or a matrix with one row)")
    if (ncol(x) != length(y))
        stop("the arguments do not conform: ", arg.names[1], " has ", ncol(x),
             " cols, and ", arg.names[2], " has ", length(y), " elts")
    if (arg.order[1]==1)
        return(x - matrix(y, byrow = T, nrow = nrow(x), ncol = length(y)))
    else
        return(matrix(y, byrow = T, nrow = nrow(x), ncol = length(y)) - x)
}

# Operator version
"%-row%" <- minus.row

times.row <- function(x, y)
{
    # The tricky stuff with the arg.names argument is so it won't
    # be evaluated unless we need it in an error message
    # The if condition implements the following logic:
    # swap <- F
    # if (is.matrix(y))
    #     if (!is.matrix(x))
    #         swap <- T
    #     else if (all(dim(y)!=1) && any(dim(x)==1))
    #         swap <- T
    # if (swap) ...
    arg.names <- c(deparse(substitute(x)), deparse(substitute(y)))
    if (is.matrix(y) && (!is.matrix(x) || (all(dim(y)!=1) && any(dim(x)==1)))) {
        tt <- y
        y <- x
        x <- tt
        arg.order <- 2:1
    } else {
        arg.order <- 1:2
    }
    arg.names <- arg.names[arg.order]
    if (!is.matrix(x))
        stop("must have one vector argument and one matrix argument")
    if (is.array(y) && !(is.matrix(y) && nrow(y)==1))
        stop(arg.names[2], " must be just a vector (or a matrix with one row)")
    if (ncol(x) != length(y))
        stop("the arguments do not conform: ", arg.names[1], " has ", ncol(x),
             " cols, and ", arg.names[2], " has ", length(y), " elts")
    return(x * matrix(y, byrow = T, nrow = nrow(x), ncol = length(y)))
}

# Operator version
"%*row%" <- times.row

pclip <- function(x, lower=range[1], upper=range[2], range=c(NA, NA), na.rm=FALSE, zap=FALSE) {
    # if zap=TRUE, values outside of the range are set to NA rather than the limits
    zap <- rep(zap, length.out=2)
    x.attr <- attributes(x)
    if (!is.null(range) && length(range)!=2)
        stop("length of range must be 2")
    if (length(lower)!=1 && length(lower)!=length(x))
        stop("lower must be length 1 or same length as x")
    if (length(upper)!=1 && length(upper)!=length(x))
        stop("upper must be length 1 or same length as x")
    # be careful because NA's in the bounds and x are treated assymetrically:
    # NA in x is never replaced by a bound, but NA in a bound is ignored.
    # pmax() and pmin() behave symmetrically, e.g., pmin(x, bound, na.rm=FALSE)
    # will return NA if bound is NA, regardless of x.
    if (length(lower)==1) {
        if (!is.na(lower))
            if (zap[1])
                x[] <- ifelse(x < lower, NA, x)
            else
                x[] <- pmax(x, lower, na.rm=FALSE)
    } else {
        i <- !is.na(lower)
        if (all(i))
            if (zap[1])
                x[] <- ifelse(x < lower, NA, x)
            else
                x[] <- pmax(x, lower, na.rm=FALSE)
        else if (any(i))
            if (zap[1])
                x[i] <- ifelse(x[i] < lower[i], NA, x[i])
            else
                x[i] <- pmax(x[i], lower[i], na.rm=FALSE)
    }
    if (length(upper)==1) {
        if (!is.na(upper))
            if (zap[2])
                x[] <- ifelse(x > upper, NA, x)
            else
                x[] <- pmin(x, upper, na.rm=FALSE)
    } else {
        i <- !is.na(upper)
        if (all(i))
            if (zap[2])
                x[] <- ifelse(x > upper, NA, x)
            else
                x[] <- pmin(x, upper, na.rm=FALSE)
        else if (any(i))
            if (zap[2])
                x[i] <- ifelse(x[i] > upper[i], NA, x[i])
            else
                x[i] <- pmin(x[i], upper[i], na.rm=FALSE)
    }
    if (any(i <- is.true(lower > upper))) {
        eps <- if (storage.mode(lower)=="double" && storage.mode(upper)=="double") .Machine$double.eps else .Machine$single.eps
        if (any(j <- (lower-upper)[i] > sqrt(eps) * pmax(abs(lower), abs(upper))[i]))
            warning("have ", sum(j), " lower > upper")
    }
    # Attributes should still be there because we assigned using x[] <- ...
    # attributes(x) <- x.attr
    x
}
# like factor, but works with matrices and arrays too

make.factor <- function(x, levels=sort(unique(x), na.last=TRUE), labels=as.character(levels), exclude=NA) {
    d <- dim(x)
    dn <- dimnames(x)
    x <- factor(as.integer(x), levels=levels, labels=labels, exclude=exclude)
    if (!is.null(d))
        dim(x) <- d
    if (!is.null(dn))
        dimnames(x) <- dn
    x
}
zmul <- function(x, y) {
    # Differs from ordinary multiplication in that zmul(x, y) is zero
    # if either x or y are zero, even if the other is NA or Inf.
    ifelse(x==0 | y==0, 0, x * y)
}

zdiv <- function(x, y) {
    # Differs from ordinary division in that zdiv(x, y) is zero
    # if x is zero, even if y is zero, NA, or Inf.
    ifelse(x==0, 0, x / y)
}

'%Z*%' <- zmul
'%Z/%' <- zdiv

zapf <- function(x, y, value=NA, how=NULL, error.how=NULL) {
    # x should be a vector or array, y should be logical same shape
    # where y is T, set x to value (=NA)
    if (!is.logical(y))
        stop("y must be logical")
    if (length(dim(y))==2 && length(dim(x))!=2)
        stop("cannot use vector x and matrix y")
    if (!is.null(how) && length(dim(y))==2)
        y <- sub.time(y, rownames(x), how=how, error.how=error.how)
    if (length(dim(y))==2)
        res <- replace(x, conform.dimnames(y, x, excess.ok=T, missing.ok=F), value)
    else if (length(dim(x))==2)
        if (length(names(y)) && length(colnames(x)))
            res <- replace(x, rep(conform.dimnames(y, list(colnames(x)), excess.ok=T, missing.ok=F), each=nrow(x)), value)
        else if (length(y) == ncol(x))
            res <- replace(x, rep(y, each=nrow(x)), value)
        else
            stop("length of vector y doesn't match ncol of matrix x, and don't have names")
    else
        if (length(names(y)) && length(names(x)))
            res <- replace(x, conform.dimnames(y, list(names(x)), excess.ok=T, missing.ok=F), value)
        else if (length(y) == length(x))
            res <- replace(x, y, value)
        else
            stop("length of vector y doesn't match length of vector x, and don't have names")
    return(res)
}
zapu <- function(x, y, value=NA, how=NULL, error.how=NULL, missing.ok=FALSE) {
    # 'zap unless'
    # where y is F or NA (or missing from y), set x to value (=NA)
    # x should be a vector or array, y should be logical same shape or vector
    # vector x or y is treated as cross-sectional (i.e., for one date)
    if (!is.logical(y)) {
        if (is.numeric(y) && any(y!=0 & y!=1, na.rm=TRUE))
            stop("y must be logical or numeric with all values 0 or 1")
        # we do a negation on y before we use it, which will coerce y to logical
    }
    if (length(dim(y))==2 && length(dim(x))!=2)
        stop("cannot use vector x and matrix y")
    if (!is.null(how) && length(dim(y))==2)
        y <- sub.time(y, rownames(x), how=how, error.how=error.how)
    if (length(dim(y))==2) {
        y <- !replace.na(conform(y, x, missing.ok=missing.ok))
        res <- replace(x, y, value)
    } else if (length(dim(x))==2) {
        if (length(names(y)) && length(colnames(x))) {
            y <- !replace.na(conform(y, list(colnames(x)), missing.ok=missing.ok))
            res <- replace(x, rep(y, each=nrow(x)), value)
        } else if (length(y) == ncol(x)) {
            y <- !replace.na(y)
            res <- replace(x, rep(y, each=nrow(x)), value)
        } else {
            stop("length of vector y doesn't match ncol of matrix x, and don't have names")
        }
    } else {
        if (length(names(y)) && length(names(x))) {
            y <- !replace.na(conform(y, list(names(x)), missing.ok=missing.ok))
            res <- replace(x, y, value)
        } else if (length(y) == length(x)) {
            y <- !replace.na(y)
            res <- replace(x, y, value)
        } else {
            stop("length of vector y doesn't match length of vector x, and don't have names")
        }
    }
    return(res)
}

zapu.icf <- function(x, y, value=NA, how=NULL, error.how="NA") {
    # 'zap unless', with inner conform
    #
    # Inner conform part is a little tricky; for ids it's simple
    # For dates, if how=NULL, it's a simple intersection.
    # If how is not NULL, then if the dateMatch() returns NA, drop that row from the result.
    #
    # Where y is F or NA set x to value (=NA).
    # x should be a vector or array, y should be logical same shape or vector
    # vector x or y is treated as cross-sectional (i.e., for one date)
    if (!is.logical(y)) {
        if (is.numeric(y) && any(y!=0 & y!=1, na.rm=TRUE))
            stop("y must be logical or numeric with all values 0 or 1")
        # we do a negation on y before we use it, which will coerce y to logical
    }
    y.id <- if (length(dim(y))==2) colnames(y) else names(y)
    x.id <- if (length(dim(x))==2) colnames(x) else names(x)
    res.id <- intersect(x.id, y.id)

    if (length(dim(y))==2 && length(dim(x))!=2)
        stop("cannot use vector x and matrix y")

    if (length(dim(y))==2) {
        # Both x and y are matrices
        y.rn <- if (length(dim(y))==2) rownames(y) else NULL
        x.rn <- if (length(dim(x))==2) rownames(x) else NULL
        if (!is.null(how)) {
            if (is.null(x.rn) || is.null(y.rn))
                stop("cannot use 'how' when missing some rownames")
            # have to do date matching before the intersect
            i <- dateMatch(x.rn, y.rn, how=how, error.how=error.how)
            if (any(is.na(i))) {
                res.rn <- x.rn[!is.na(i)]
                i <- i[!is.na(i)]
            } else {
                res.rn <- x.rn
            }
            y <- y[i,,drop=FALSE]
        } else {
            # simple intersect
            if (is.null(x.rn) || is.null(y.rn)) {
                if (nrow(x) != nrow(y))
                    stop('number of rows must be identical when rownames are missing')
                res.rn <- seq(len=nrow(x))
            } else {
                res.rn <- intersect(x.rn, y.rn)
            }
        }
    }

    if (length(dim(y))==2) {
        # Both are two dimensional
        if (is.null(y.id) || is.null(x.id)) {
            if (ncol(x) != ncol(y))
                stop('number of columns must be identical when colnames are missing')
            y <- !replace.na(y[res.rn, , drop=FALSE])
            res <- replace(x[res.rn, , drop=FALSE], y, value)
        } else {
            y <- !replace.na(y[res.rn, res.id, drop=FALSE])
            res <- replace(x[res.rn, res.id, drop=FALSE], y, value)
        }
    } else if (length(dim(x))==2) {
        # x is 2 dimensional, y is a vector (cross-sectional)
        if (length(y.id) && length(x.id)) {
            # both have names
            y <- !replace.na(y[res.id])
            res <- replace(x[,res.id], rep(y, each=nrow(x)), value)
        } else if (length(y) == ncol(x)) {
            # no id names on at least one -- must be same extent
            y <- !replace.na(y)
            res <- replace(x, rep(y, each=nrow(x)), value)
        } else {
            stop("length of vector y doesn't match ncol of matrix x, and don't have names")
        }
    } else {
        # Both x and y are one dimensional
        if (length(y.id) && length(x.id)) {
            y <- !replace.na(y[res.id])
            res <- replace(x[res.id], y, value)
        } else if (length(y) == length(x)) {
            y <- !replace.na(y)
            res <- replace(x, y, value)
        } else {
            stop("length of vector y doesn't match length of vector x, and don't have names")
        }
    }
    return(res)
}

zaplt <- function(x, fill=0, threshold=1) {
    ifelse(abs(x) >= threshold, x, fill)
}
set.dimnames <- function(x, i, names) {
    if (is.null(i) && missing(names)) {
        dimnames(x) <- NULL
        return(x)
    }

    if (is.list(i)) {
        if (!missing(names))
            stop("cannot supply names and a list value for i")
        if (length(i) != length(dim(x)))
            stop("dimnames are wrong length for x")
        dimnames(x) <- i
        return(x)
    }

    if (length(i)==1) {
        if (!is.character(names) && !is.null(names))
            stop("names must be character or NULL when length(i)==1")
        dimnames(x)[[i]] <- names
    } else if (length(i>1)) {
        if (!is.list(names) || length(i)!=length(names))
            stop("names must be a list when setting dimnames on more than one dimension")
        dimnames(x)[i] <- names
    }
    x
}

set.colnames <- function(x, names) {
    colnames(x) <- names
    x
}

set.rownames <- function(x, names=NULL, from.col=NULL, del.col=TRUE) {
    if (!is.null(names) && !is.null(from.col))
        stop("must supply just one of 'names' and 'from.col'")
    if (!is.null(from.col)) {
        names <- x[ ,from.col, drop=TRUE]
        if (del.col)
            x <- x[, -match(from.col, colnames(x)), drop=FALSE]
    }
    rownames(x) <- names
    x
}

set.trilnames <- function(x, names) {
    trilnames(x) <- names
    x
}

set.names <- function(x, names) {
    names(x) <- names
    x
}

set.dim <- function(x, dim, dimnames=NULL) {
    if (is.data.frame(x))
        stop("x must be an array or matrix")
    if (sum(is.na(dim))==1) {
        i <- length(x) / prod(na.del(dim))
        if (!is.wholenumber(i))
            stop("inference for missing dim does not give a whole number: ", i)
        dim[is.na(dim)] <- round(i)
    }
    if (any(is.na(dim)))
        stop("dim cannot have more than one missing value")
    if (prod(dim) != length(x))
        stop("supplied dimensions are incorrect for x (length(x)=", length(x), ", prod(",
             paste(dim, collapse=","), ")=", prod(dim))
    # Have to first remove the dim attribute, otherwise we get an error
    # or warning on assigning a dim vector with a different length.
    attr(x, "dimnames") <- NULL
    attr(x, "dim") <- NULL
    attr(x, "dim") <- dim
    dimnames(x) <- dimnames
    x
}

is.virtual.array <- function(x) UseMethod("is.virtual.array")

is.virtual.array.farray <- function(x) TRUE
is.virtual.array.default <- function(x) FALSE

elt.sample <- function(x) UseMethod("elt.sample")
elt.sample.default <- function(x) if (!is.null(dim(x))) asub(x, rep(1, length(dim(x)))) else x[1]

M <- function(x, ...) if (is.atomic(x) && length(dim(x))==2 && length(args(...))==0) return(x) else return(as.matrix(x, ...))
