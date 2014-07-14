#' Standard matrix/array methods for sparsetsmat objects
#'
#' as.matrix: Convert a sparsetsmat object to an ordinary (non-sparse) matrix
#'
#' @rdname standard.array.methods
#' @method as.matrix sparsetsmat
#' @param x a sparsetsmat object
#' @param ... additional arguments
as.matrix.sparsetsmat <- function(x, ...) {
    x[ , , drop=FALSE]
}

#' Standard matrix/array methods for sparsetsmat objects
#'
#' as.array: Convert a sparsetsmat object to an ordinary (non-sparse) matrix
#'
#' @rdname standard.array.methods
#' @method as.array sparsetsmat
#'
#'
as.array.sparsetsmat <- function(x, ...) {
    x[ , , drop=FALSE]
}
