summary.sparsetsmat <- function(x, ...) {
    cat(paste(dim(x), collapse=' x '),
        "sparse persistent time-series matrix of class '", class(x), "'\n")
    cat("Rownames are class '", class(x$all.dates), "' and range from ",
        format(min(x$all.dates)), " to ", format(max(x$all.dates)), "\n", sep="")
    cat("Colnames are class '", class(x$id), "' and range from ",
        min(x$id), " to ", max(x$id), "\n", sep="")
    cat("Values are class '", class(x$value), "' and range from ",
        min(x$value, na.rm=TRUE), " to ", max(x$value, na.rm=TRUE), "\n", sep="")
    nna <- sum(is.na(x$value))
    cat("There are ", length(unique(x$value)), " unique values with ",
        nna, " explicit NA ", if (nna==1) "value" else "values", "\n", sep="")
    invisible(x)
}
print.sparsetsmat <- summary.sparsetsmat
