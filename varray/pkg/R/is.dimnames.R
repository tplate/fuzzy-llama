is.dimnames <- function(x) {
    return(is.list(x) && all(is.element(set=c("NULL", "character"), sapply(x, class))) && all(sapply(x, function(x) is.null(names(x)))))
}

