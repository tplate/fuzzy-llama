is.virtual.array.varray <- function(x) TRUE
is.virtual.array.default <- function(x) FALSE
is.virtual.array <- function(x) UseMethod('is.virtual.array')

