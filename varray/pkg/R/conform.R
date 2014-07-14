#' Make matrices or arrays conform with each other by adding or removing dimension elements.
#'
#' @param x An object or list of objects to conform
#' @param \dots The object to conform to, or other objects to conform
#' @param fill The fill value (default NA)
#' @param do.all If do.all=="union" or "intersect", all arguments are conformed to the
#' union (or intersection) of all dimnames, and are returned in a list.
#' With "union", this is the same behavior as when a list is supplied as the
#' only argument.
#' @param along Integer specification of dimensions on which to conform
#' @param excess.ok
#' If \code{excess.ok==TRUE}, dimension elements of x not in y are silently deleted.
#' If \code{excess.ok==FALSE}, an error is generated if x has dimension elements not in y
#' @param missing.ok
#' If \code{missing.ok==TRUE}, dimension elements of y not in x are filled with the fill value
#' If \code{missing.ok==FALSE}, an error is generated if x is missing some dimension elements in y
#'
#'


# x <- data.frame(row.names=letters[c(1,3,4,7,8)], i=1:5, d=(1:5)/10, f=as.factor(letters[22:26]), c=LETTERS[1:5], t=timeSeq(timeDate("2001/01/01"), len=5), stringsAsFactors=FALSE)

# x <- array(1:20,dim=c(5:4),dimnames=list(paste("R",1:5,sep=""),paste("C",1:4,sep="")))

conform <- function(x, ..., fill=NA, do.all=c("no", "union", "intersect"), along=NULL, excess.ok=TRUE, missing.ok=TRUE) UseMethod("conform")

# this method works with lists, or vectors, matrices and arrays
conform.default <- function(x, ..., fill=NA, do.all=c("no", "union", "intersect"), along=NULL, excess.ok=TRUE, missing.ok=TRUE) {
    do.all <- match.arg(do.all)
    nDotArgs <- length(match.call(expand.dots=FALSE)$...)
    if (is.list(x) && is.null(dim(x))) {
        # handle x being a list
        # excess.ok is irrelevant here -- no!
        if (nDotArgs==0) {
            if (do.all=="intersect")
                z.d <- intersect.dimnames(x, along=along)
            else
                z.d <- union.dimnames(x, along=along)
        } else if (nDotArgs==1) {
            if (is.dimnames(..1))
                z.d <- ..1
            else
                z.d <- dimnames(..1)
        } else {
            stop("can only have one ... arg when x is a list")
        }
        z <- list()
        for (i in seq(along=x))
            z[[i]] <- conform(x[[i]], z.d, fill=fill, do.all="no", along=along, excess.ok=excess.ok, missing.ok=missing.ok)
        if (!is.null(names(x)))
            names(z) <- names(x)
        return(z)
    } else {
        # handle x and ... being vectors, matrices and arrays
        if (do.all != "no") {
            return(conform.list(x=c(list(x), list(...)), fill=fill, do.all=do.all, along=along, excess.ok=excess.ok, missing.ok=missing.ok))
        } else if (is.list(x) && nDotArgs==0) {
            # can get incorrectly dispatched to conform.array when
            # x is a named list
            return(conform.list(x=x, fill=fill, do.all="union", along=along, excess.ok=excess.ok, missing.ok=missing.ok))
        }

        get.dimnames <- function(x) {
            if (is.dimnames(x))
                x
            else if (length(dim(x))) # instead of is.array(), which doesn't work for virtual arrays
                non.null(dimnames(x), rep(list(character(0)), length(dim(x))))
            else if (!is.null(names(x)))
                list(names(x))
            else
                list(character(0))
        }

        get.dim <- function(x) {
            if (length(dim(x)))
                dim(x)
            else
                length(x)
        }

        if (nDotArgs>1)
            if (do.all=='intersect')
                y.d <- intersect.dimnames(..., along=along)
            else
                y.d <- union.dimnames(..., along=along)
        else if (is.dimnames(..1))
            y.d <- ..1
        else
            y.d <- get.dimnames(..1)
        x.d <- get.dimnames(x)
        x.attr <- attributes(x)
        if (is.null(along)) {
            along <- seq(along=get.dim(x))
            if (length(x.d) != length(y.d))
                stop("x has ", length(x.d), " dimensions but y has ", length(y.d))
        } else {
            if (length(x.d) < max(along))
                stop("x has ", length(x.d), " dimensions max(along) is ", max(along))
            if (length(y.d) < max(along))
                stop("y has ", length(y.d), " dimensions max(along) is ", max(along))
        }

        if (!excess.ok)
            for (i in along)
                if (any(!is.element(x.d[[i]], y.d[[i]])))
                    stop("x has some dimname[", i, "] elements not in y, e.g.: ",
                         paste(some.examples(x.d[[i]][!is.element(x.d[[i]], y.d[[i]])]), collapse=", "))
        if (!missing.ok)
            for (i in along)
                if (any(!is.element(y.d[[i]], x.d[[i]])))
                    stop("x is missing some dimname[", i, "] elements that are in y, e.g.: ",
                         paste(some.examples(y.d[[i]][!is.element(y.d[[i]], x.d[[i]])]), collapse=", "))
        for (i in along) {
            if (!identical(x.d[[i]], y.d[[i]])) {
                # indexing with NA values generated by match for values of y.d not in x.d
                # will pull in correct NA rows
                args <- c(lapply(get.dim(x), function(n) seq(len=n)), list(drop=FALSE))
                args[[i]] <- match(y.d[[i]], x.d[[i]])
                x <- do.call("[", c(list(x), args))
                if (length(dim(x)))
                    dimnames(x)[[i]] <- y.d[[i]]
                else if (is.atomic(x) && i==1)
                    names(x) <- y.d[[i]]
                if (!is.na(fill)) {
                    rargs <- args[seq(len=length(x.d))]
                    rargs[[i]] <- which(is.na(rargs[[i]]))
                    if (length(rargs[[i]]))
                        x <- do.call("[<-", c(list(x), rargs, list(value=fill)))
                }
            }
        }
        # see if we need to put back any attributes of x that got lost
        # no, this doesn't work well -- better to just rely on behavior of [ and cbind
        if (FALSE) {
            for (a in intersect(names(attributes(x)), names(x.attr)))
                x.attr[[a]] <- NULL
            if (length(x.attr))
                attributes(x) <- c(attributes(x), x.attr)
        }
        return(x)
    }
}

# the *.list method doesn't get dispatched by S3 in S-PLUS, fold this into the default
conform.list <- function(x, ..., fill=NA, do.all=c("no", "union", "intersect"), along=NULL, excess.ok=TRUE, missing.ok=TRUE) {
    do.all <- match.arg(do.all)
    nDotArgs <- length(match.call(expand.dots=FALSE)$...)
    if (!is.list(x))
        stop("x must be a list")
    if (nDotArgs==0) {
        if (do.all=='intersect')
            z.d <- intersect.dimnames(x, along=along)
        else
            z.d <- union.dimnames(x, along=along)
    } else if (nDotArgs==1) {
        if (is.dimnames(..1))
            z.d <- ..1
        else
            z.d <- dimnames(..1)
    } else {
        stop("can only have one ... arg when x is a list")
    }
    z <- list()
    for (i in seq(along=x))
        z[[i]] <- conform(x[[i]], z.d, fill=fill, do.all="no", along=along, excess.ok=excess.ok, missing.ok=missing.ok)
    if (!is.null(names(x)))
        names(z) <- names(x)
    return(z)
}

conform.data.frame <- function(x, ..., fill=NA, do.all=c("no", "union", "intersect"), along=NULL, excess.ok=TRUE, missing.ok=TRUE) {
    do.all <- match.arg(do.all)
    nDotArgs <- length(match.call(expand.dots=FALSE)$...)
    if (do.all!="no")
        return(conform.list(x=c(list(x), list(...)), fill=fill, do.all=do.all, along=along, excess.ok=excess.ok, missing.ok=missing.ok))

    get.dimnames <- function(x) {
        if (is.dimnames(x))
            x
        else if (length(dim(x))) # instead of is.array(), which doesn't work for virtual arrays
            non.null(dimnames(x), rep(list(character(0)), length(dim(x))))
        else if (!is.null(names(x)))
            list(names(x))
        else
            list(character(0))
    }
    get.dim <- function(x) {
        if (length(dim(x)))
            dim(x)
        else
            length(x)
    }

    if (nDotArgs>1)
        if (do.all=='intersect')
            y.d <- intersect.dimnames(..., along=along)
        else
            y.d <- union.dimnames(..., along=along)
    else if (is.dimnames(..1))
        y.d <- ..1
    else
        y.d <- get.dimnames(..1)

    x.d <- get.dimnames(x)
    x.attr <- attributes(x)

    if (is.null(along)) {
        along <- seq(along=get.dim(x))
        if (length(x.d) != length(y.d))
            stop("x has ", length(x.d), " dimensions but y has ", length(y.d))
    } else {
        if (length(x.d) < max(along))
            stop("x has ", length(x.d), " dimensions max(along) is ", max(along))
        if (length(y.d) < max(along))
            stop("y has ", length(y.d), " dimensions max(along) is ", max(along))
    }
    if (!excess.ok)
        for (i in along)
            if (any(!is.element(x.d[[i]], y.d[[i]])))
                stop("x has some dimname[", i, "] elements not in y, e.g.: ",
                     paste(some.examples(x.d[[i]][!is.element(x.d[[i]], y.d[[i]])]), collapse=", "))
    if (!missing.ok)
        for (i in along)
            if (any(!is.element(y.d[[i]], x.d[[i]])))
                stop("x is missing some dimname[", i, "] elements that are in y, e.g.: ",
                     paste(some.examples(y.d[[i]][!is.element(y.d[[i]], x.d[[i]])]), collapse=", "))

    # fix rows
    if (is.element(1, along) && !identical(x.d[[1]], y.d[[1]])) {
        # indexing with NA values generated by match for values of y.d not in x.d
        # will pull in correct NA rows
        x <- x[i <- match(y.d[[1]], x.d[[1]]), , drop=FALSE]
        if (length(dim(x)))
            dimnames(x)[[1]] <- y.d[[1]]
        else
            names(x) <- y.d[[1]]
        if (!is.na(fill) && any(is.na(i)))
            x[which(is.na(i)), ] <- fill

    }
    # fix columns
    if (is.element(2, along) && !identical(x.d[[2]], y.d[[2]])) {
        if (length(dim(x))==0)
            stop("x is not a 2-d object")
        extra.i <- which(!is.element(y.d[[2]], x.d[[2]]))
        if (length(extra.i)) {
            if (length(dim(..1))) {
                # if we know the columns in y, use those types
                # need to use row indices of nrow(y)+1 to pull in correct number of rows
                # (indexing with 1 past end returns NA)
                extra.cols <- ..1[rep(length(y.d[[1]])+1, NROW(x)), extra.i, drop=FALSE]
                if (!is.na(fill) && nrow(x) > 0)
                    for (i in seq(len=ncol(extra.cols)))
                        extra.cols[seq(len=nrow(x)), i] <- fill
            } else {
                # Don't know column types from y (because all we have is dimnames)
                # If all columns in x have same class, use that class
                # Otherwise, fill with numeric NA columns
                if (!is.na(fill)) {
                    extra.cols <- matrix(fill, nrow=NROW(x), ncol=length(extra.i))
                } else if (is.data.frame(x) && length(unique(sapply(x, function(o) class(o)[1])))==1) {
                    extra.cols <- x[rep(length(x.d[[1]])+1, NROW(x)), rep(1, length(extra.i)), drop=FALSE]
                } else {
                    extra.cols <- matrix(as.double(NA), nrow=NROW(x), ncol=length(extra.i))
                }
            }
            dimnames(extra.cols) <- list(get.dimnames(x)[[1]], y.d[[2]][extra.i])
            # append the extra cols at the end of the data frame
            x <- cbind(x, extra.cols)
        }
        # get the columns in the right order
        x <- x[, match(y.d[[2]], get.dimnames(x)[[2]]), drop=FALSE]
    }

    # see if we need to put back any attributes of x that got lost
    for (a in intersect(names(attributes(x)), names(x.attr)))
        x.attr[[a]] <- NULL
    if (length(x.attr))
        attributes(x) <- c(attributes(x), x.attr)
    return(x)
}

