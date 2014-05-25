dim.sparsetsmat <- function(x) c(length(x$all.dates), length(x$ids))
dimnames.sparsetsmat <- function(x) list(format(x$all.dates), x$ids)
nrow.sparsetsmat <- function(x) length(x$all.dates)
ncol.sparsetsmat <- function(x) length(x$ids)
rownames.sparsetsmat <- function(x) format(x$all.dates)
colnames.sparsetsmat <- function(x) x$ids
is.virtual.array.sparsetsmat <- function(x) TRUE
values <- function(x) sort(unique(x$value))
