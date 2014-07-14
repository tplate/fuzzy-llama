#' @description dim: return \code{dim} of a sparsetsmat
#' @rdname standard.array.methods
#' @method dim sparsetsmat
dim.sparsetsmat <- function(x) c(length(x$all.dates), length(x$ids))
#' @description dimnames: return \code{dimnames} of a sparsetsmat
#' @rdname standard.array.methods
#' @method dimnames sparsetsmat
dimnames.sparsetsmat <- function(x) list(format(x$all.dates), x$ids)
#' @rdname standard.array.methods
#' @method nrow sparsetsmat
#' @description nrow: return number of rows (virtual) of a sparsetsmat
nrow.sparsetsmat <- function(x) length(x$all.dates)
#' @rdname standard.array.methods
#' @method ncol sparsetsmat
#' @description ncol: return number of cols of a sparsetsmat
ncol.sparsetsmat <- function(x) length(x$ids)
#' @rdname standard.array.methods
#' @method rownames sparsetsmat
#' @description rownames: return rownames of a sparsetsmat
rownames.sparsetsmat <- function(x) format(x$all.dates)
#' @rdname standard.array.methods
#' @method colnames sparsetsmat
#' @description colnames: return colnames of a sparsetsmat
colnames.sparsetsmat <- function(x) x$ids
values <- function(x) sort(unique(x$value))
#' is.virtual.array
#' @method is.virtual.array sparsetsmat
#' @param x a sparsetsmat object
#'
#' @details Should return \code{TRUE} for any object that is not a
#' standard R array but that can be treated as an array
#' using functions like \code{dim()}, \code{dimnames()},
#' \code{x[...]}, etc.  Only makes sense for objects which
#' have a \code{dim()} method.
#'
#' For objects that are virtual arrays, the contents can
#' only be safely accessed through ordinary indexing.  In
#' contrast, the contents of an ordinary matrix or array
#' based on an atomic vector can be directly accessed by
#' indexing into the atomic vector.
#'
is.virtual.array <- function(x) UseMethod('is.virtual.array')
#' @rdname is.virtual.array
is.virtual.array.default <- function(x) FALSE
#' @rdname is.virtual.array
is.virtual.array.sparsetsmat <- function(x) TRUE
