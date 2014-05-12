dim.sparsetsmat <- function(x) c(length(x$all.dates), length(x$id))
dimnames.sparsetsmat <- function(x) list(format(x$all.dates), x$id)
nrow.sparsetsmat <- function(x) length(x$all.dates)
ncol.sparsetsmat <- function(x) length(x$id)
rownames.sparsetsmat <- function(x) format(x$all.dates)
colnames.sparsetsmat <- function(x) x$id
is.virtual.array.sparsemat <- function(x) TRUE
