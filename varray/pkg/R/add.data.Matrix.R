#' Add data to an object
#'
#' Update the contents of a Matrix or matrix object, adding new dimension
#' indices if necessary.
#'
#' @param x An object to add data to, either specified by name or by value.
#' If specified by name (as a character vector), the object is changed in place (i.e., the function will have side effects)
#'
#' @param newdata New data to incorporate in the object.
#' Should have the same number of dimensions as the object
#' being updated (i.e.,
#' \code{length(dim(x))==length(dim(newdata))}).  Must have
#' dimnames.
#'
#' @param need.dimnames Dimension names that should be included in the updated object.
#'
#' @param keep.ordered Logical.  Specifies which dimensions should be
#' kept ordered.  Can be a single element or a vector with
#' length equal to the number of dimensions of
#' \code{object}.
#'
#' @param envir where to find the object, or where to create it if it does not already exist
#'
#' @param clear.dim numeric: dimension indices for which to
#' clear all values when present, e.g., with clear.dim=1,
#' all rows that match a row supplied in newdata are cleared
#'
#' @param \dots Not used, but needed because \code{add.data()} could be a generic.
#'
#' @details
#' Can be used in multiple ways, either called as a generic or calling the method directly (to create the object if it does not already exist):
#' \itemize{
#' \item
#' add.data(x, newdata): adds newdata to existing object x and returns the modified x (no side effects)
#' \item
#' add.data.Matrix(x, newdata): adds newdata to existing Matrix object x (no side effects)
#' \item
#' add.data('x', newdata): adds newdata to existing object named 'x' and saves the modified x (has side effects)
#' \item
#' add.data.Matrix('x', newdata): adds newdata to existing Matrix object named 'x' and saves the modified object in 'x' OR if 'x' doesn't exist, creates new Matrix object with contents newdata and saves it in 'x' (has side effects)
#' }
#'
#' @return The altered object \code{x}.  If \code{x} was the
#' name of an object, then the altered object is also
#' updated in place.
#'
#' @note   Not really closely related to \code{varray} objects, but supplied here
#'  as a useful analogue to \code{\link{add.tsdata.varray}}.
#'
#' @examples
#' x <- cbind(A=c(a=1))
#' add.data.matrix('x', cbind(B=c(b=2)))
#' x

add.data <- function(x, ...) UseMethod('add.data')

#' @describeIn add.data Default method that adds data to a object specified by a character name

add.data.default <- function(x, ...) {
    if (is.character(x) && length(x)==1 && is.null(dim(x))) {
        x.pos <- find(x, numeric=TRUE)
        if (length(x.pos)<1)
            stop('object ', x, ' does not exist')
        y <- get(x, pos=x.pos[1])
        # avoid infinite loops
        if (is.character(y) && length(x)==1 && is.null(dim(y)))
            stop('cannot add data to a single-element character string')
        z <- add.data(y, ...)
        assign(x, value=z, pos=x.pos[1])
        return(invisible(z))
    } else {
        stop('cannot handle object with class ', class(x))
    }
}

#' @describeIn add.data Add data to a Matrix object

add.data.Matrix <- function(x,
                            newdata,
                            need.dimnames=list(NULL, NULL),
                            keep.ordered=TRUE,
                            clear.dim=NULL,
                            ...,
                            envir=NULL) {
    # have ... args to satisfy the generic add.data()
    if (length(list(...)))
        warning('additional arguments ignored: ', paste(names(list(...)), collapse=', '))
    x.name <- NULL
    newObj <- FALSE
    if (is.character(x) && length(x)==1 && is.null(dim(x))) {
        x.name <- x
        if (is.null(envir)) {
            x.pos <- find(x.name, numeric=TRUE)
            if (length(x.pos)>0)
                envir <- as.environment(x.pos[1])
            else
                envir <- globalenv()
        }
        if (exists(x, envir=envir)) {
            x <- get(x.name, envir=envir, inherits=FALSE)
            if (!inherits(x, 'Matrix'))
                stop('object ', x.name, ' is not a Matrix')
        } else {
            x <- NULL
            newObj <- TRUE
        }
    } else {
        if (!inherits(x, 'Matrix'))
            stop('x is not a Matrix')
    }
    if (!inherits(newdata, 'Matrix') && !is.matrix(newdata))
        stop('newdata must be a Matrix or a matrix')
    if (newObj) {
        x <- Matrix(newdata, sparse=TRUE)
    } else if (length(clear.dim)) {
        if (is.element(1, clear.dim)) {
            ii <- intersect(rownames(x), rownames(newdata))
            if (length(ii))
                x[ii,] <- 0
        }
        if (is.element(2, clear.dim)) {
            ii <- intersect(colnames(x), colnames(newdata))
            if (length(ii))
                x[,ii] <- 0
        }
    }
    dn <- list(unique(c(rownames(x), rownames(newdata), need.dimnames[[1]])),
               unique(c(colnames(x), colnames(newdata), need.dimnames[[2]])))
    keep.ordered <- rep(keep.ordered, length.out=length(dim(x)))
    if (any(keep.ordered))
        dn[keep.ordered] <- lapply(dn[keep.ordered], sort, na.last=NA)
    sample <- replace(x[1,1], 1, 0L)
    if (!isTRUE(all.equal(rownames(x), dn[[1]]))) {
        new <- setdiff(dn[[1]], rownames(x))
        if (length(new))
            x <- rBind(x, array(sample, dimnames=list(new, colnames(x)), dim=c(length(new), ncol(x))))
        if (!isTRUE(all.equal(rownames(x), dn[[1]])))
            x <- x[dn[[1]],,drop=FALSE]
    }
    if (!isTRUE(all.equal(colnames(x), dn[[2]]))) {
        new <- setdiff(dn[[2]], colnames(x))
        if (length(new))
            x <- cBind(x, array(sample, dimnames=list(rownames(x), new), dim=c(nrow(x), length(new))))
        if (!isTRUE(all.equal(colnames(x), dn[[2]])))
            x <- x[,dn[[2]],drop=FALSE]
    }
    if (!newObj) {
        ii <- cbind(rep(match(rownames(newdata), rownames(x)), ncol(newdata)),
                    rep(match(colnames(newdata), colnames(x)), each=nrow(newdata)))
        x[ii] <- as.vector(newdata)
    }
    if (!is.null(x.name)) {
        assign(x.name, value=x, envir=envir)
        return(invisible(x))
    } else {
        return(x)
    }
}

#' @describeIn add.data Add data to an ordinary matrix object

add.data.matrix <- function(x,
                            newdata,
                            need.dimnames=list(NULL, NULL),
                            keep.ordered=TRUE,
                            clear.dim=NULL,
                            ...,
                            envir=NULL) {
    # have ... args to satisfy the generic add.data()
    if (length(list(...)))
        warning('additional arguments ignored: ', paste(names(list(...)), collapse=', '))
    x.name <- NULL
    newObj <- FALSE
    if (is.character(x) && length(x)==1 && is.null(dim(x))) {
        x.name <- x
        if (is.null(envir)) {
            x.pos <- find(x.name, numeric=TRUE)
            if (length(x.pos)>0)
                envir <- as.environment(x.pos[1])
            else
                envir <- globalenv()
        }
        if (exists(x, envir=envir)) {
            x <- get(x.name, envir=envir, inherits=FALSE)
            if (!is.matrix(x))
                stop('object ', x.name, ' is not a matrix')
        } else {
            x <- NULL
            newObj <- TRUE
        }
    } else {
        if (!is.matrix(x))
            stop('x is not a matrix')
    }
    if (newObj) {
        x <- as.matrix(newdata)
    } else if (length(clear.dim)) {
        if (is.element(1, clear.dim)) {
            ii <- intersect(rownames(x), rownames(newdata))
            if (length(ii))
                x[ii,] <- 0
        }
        if (is.element(2, clear.dim)) {
            ii <- intersect(colnames(x), colnames(newdata))
            if (length(ii))
                x[,ii] <- 0
        }
    }
    dn <- list(unique(c(rownames(x), rownames(newdata), need.dimnames[[1]])),
               unique(c(colnames(x), colnames(newdata), need.dimnames[[2]])))
    keep.ordered <- rep(keep.ordered, length.out=length(dim(x)))
    if (any(keep.ordered))
        dn[keep.ordered] <- lapply(dn[keep.ordered], sort, na.last=NA)
    sample <- replace(x[1,1], 1, 0L)
    if (!isTRUE(all.equal(rownames(x), dn[[1]]))) {
        new <- setdiff(dn[[1]], rownames(x))
        if (length(new))
            x <- rbind(x, array(sample, dimnames=list(new, colnames(x)), dim=c(length(new), ncol(x))))
        if (!isTRUE(all.equal(rownames(x), dn[[1]])))
            x <- x[dn[[1]],,drop=FALSE]
    }
    if (!isTRUE(all.equal(colnames(x), dn[[2]]))) {
        new <- setdiff(dn[[2]], colnames(x))
        if (length(new))
            x <- cbind(x, array(sample, dimnames=list(rownames(x), new), dim=c(nrow(x), length(new))))
        if (!isTRUE(all.equal(colnames(x), dn[[2]])))
            x <- x[,dn[[2]],drop=FALSE]
    }
    if (!newObj) {
        ii <- cbind(rep(match(rownames(newdata), rownames(x)), ncol(newdata)),
                    rep(match(colnames(newdata), colnames(x)), each=nrow(newdata)))
        x[ii] <- as.vector(newdata)
    }
    if (!is.null(x.name)) {
        assign(x.name, value=x, envir=envir)
        return(invisible(x))
    } else {
        return(x)
    }
}
