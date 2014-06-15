dim.sparsetsmat <- function(x) c(length(x$all.dates), length(x$ids))
dimnames.sparsetsmat <- function(x) list(format(x$all.dates), x$ids)
nrow.sparsetsmat <- function(x) length(x$all.dates)
ncol.sparsetsmat <- function(x) length(x$ids)
rownames.sparsetsmat <- function(x) format(x$all.dates)
colnames.sparsetsmat <- function(x) x$ids
#' is.virtual.array
#' @method is.virtual.array sparsetsmat
#' @param x a sparsetsmat object
#' @details Should return TRUE for any object that is not a standard R array but that can be treated as an array using functions like \code{dim()}, \code{dimnames()}, \code{x[...]}, etc.  Only makes sense for objects which have a \code{dim()} method.
is.virtual.array <- function(x) UseMethod('is.virtual.array')
#' @rdname is.virtual.array
is.virtual.array.default <- function(x) FALSE
#' @rdname is.virtual.array
is.virtual.array.sparsetsmat <- function(x) TRUE
values <- function(x) sort(unique(x$value))
