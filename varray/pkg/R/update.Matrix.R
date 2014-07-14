#' Add data to an object (obselete version of add.data)
#'
#' Update the contents of a Matrix or matrix object, adding new dimension
#' indices if necessary.
#'
#' @param object An object to add data to, specified by name (i.e., a character string).
#' The object is changed in place (i.e., the function will have side effects).
#'
#' @param data New data to incorporate in the object.
#' Should have the same number of dimensions as the object
#' being updated (i.e.,
#' \code{length(dim(x))==length(dim(data))}).  Must have
#' dimnames.
#'
#' @param need.dimnames Dimension names that should be included in the updated object.
#'
#' @param keep.ordered Logical.  Specifies which dimensions should be
#' kept ordered.  Can be a single element or a vector with
#' length equal to the number of dimensions of
#' \code{object}.
#'
#' @method update Matrix
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
#' update.matrix('x', cbind(B=c(b=2)))
#' x
#'
#' @rdname update.matrix
#'
update.Matrix <- function(object, data, need.dimnames=list(NULL, NULL), keep.ordered=TRUE, ...) {
    # have ... args to satisfy the generic update()
    if (length(list(...)))
        warning('additional arguments ignored: ', paste(names(list(...)), collapse=', '))
    va.name <- object
    if (!is.character(va.name))
        stop('object must be supplied as character data naming matrix to be updated')
    if (!exists(va.name)) {
        x <- Matrix(data, sparse=TRUE)
    } else {
        x <- get(va.name)
    }
    dn <- list(unique(c(rownames(x), rownames(data), need.dimnames[[1]])),
               unique(c(colnames(x), colnames(data), need.dimnames[[2]])))
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
        if (!isTRUE(all.equal(rownames(x), dn[[1]])))
            x <- x[dn[[1]],,drop=FALSE]
    }
    ii <- cbind(rep(match(rownames(data), rownames(x)), ncol(data)),
                rep(match(colnames(data), colnames(x)), each=nrow(data)))
    x[ii] <- as.vector(data)
    assign(va.name, value=x, pos=1)
}

#' @rdname update.matrix
#' @method update matrix
update.matrix <- function(object, data, need.dimnames=list(NULL, NULL), keep.ordered=TRUE, ...) {
    # have ... args to satisfy the generic update()
    if (length(list(...)))
        warning('additional arguments ignored: ', paste(names(list(...)), collapse=', '))
    va.name <- object
    if (!is.character(va.name))
        stop('object must be supplied as character data naming matrix to be updated')
    if (!exists(va.name)) {
        if (!is.matrix(data))
            stop('data must be a matrix')
        x <- data
    } else {
        x <- get(va.name)
        if (!is.matrix(x))
            stop(va.name, ' already exists but is not a matrix')
    }
    dn <- list(unique(c(rownames(x), rownames(data), need.dimnames[[1]])),
               unique(c(colnames(x), colnames(data), need.dimnames[[2]])))
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
        if (!isTRUE(all.equal(rownames(x), dn[[1]])))
            x <- x[dn[[1]],,drop=FALSE]
    }
    ii <- cbind(rep(match(rownames(data), rownames(x)), ncol(data)),
                rep(match(colnames(data), colnames(x)), each=nrow(data)))
    x[ii] <- as.vector(data)
    assign(va.name, value=x, pos=1)
}
