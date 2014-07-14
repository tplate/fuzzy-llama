#' Indicate whether an array-like object is native or virtual
#'
#' Indicate whether an array-like object is native or
#' virtual.  A native array object is a primitive vector
#' object with a \code{dim} attribute.  A virtual array
#' object is some object that behaves like an array in that
#' it has a dim() and subset extraction methods \code{'['}.
#' This is a generic function with methods defined for class
#' \code{varray}.
#' @param x An array-like object
#' @return A logical value, \code{TRUE} or \code{FALSE}
is.virtual.array <- function(x) UseMethod('is.virtual.array')
#' @rdname is.virtual.array
#' @method is.virtual.array varray
is.virtual.array.varray <- function(x) TRUE
#' @rdname is.virtual.array
#' @method is.virtual.array default
is.virtual.array.default <- function(x) FALSE

