non.null <- function(...) {
    # return the first non-null argument
    args <- list(...)
    for (i in seq(along=args))
        if (!is.null(args[[i]]))
            return(args[[i]])
    return(args[[length(args)]])
}


