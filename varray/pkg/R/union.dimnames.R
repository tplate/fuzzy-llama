# compute the union of dimnames
# union.dimnames(list(c("A","C"),c("b","c","d")), list(c("A","B"),c("a","c","d","e")))
# union.dimnames(list(c("A","C"),c("b","c","d")), list(c("A","B"),c("a","c","d","e")), list(c("A","D"),c("f")))


union.dimnames <- function(..., along=NULL) {
    get.dimnames <- function(x) {
        if (is.dimnames(x))
            x
        else if (length(dim(x))) # instead of is.array(), which doesn't work for virtual arrays
            non.null(dimnames(x), rep(list(character(0)), length(dim(x))))
        else if (!is.null(names(x)))
            list(names(x))
        else
            NULL
    }
    nDotArgs <- nargs() - !missing(along)
    # make x be the first object with dimnames, and args be a list of the remainder
    if (is.list(..1) && !is.data.frame(..1) && !is.dimnames(..1) && !is.virtual.array(..1)) {
        x <- get.dimnames(..1[[1]])
        args <- c(..1[-1], list(...)[-1])
        args.names <- paste("obj", seq(along=args), sep="")
    } else {
        x <- get.dimnames(..1)
        args <- list(...)[-1]
        args.names <- paste("obj", seq(along=args), sep="")
        # Don't bother with working out names from the call
        if (FALSE && length(args)) {
            args.names <- names(args)
            if (is.null(args.names) || any(args.names == ""))
                args.names <- mapply(non.null(args.names, character(length(args))),
                                     match.call(expand.dots=FALSE)$...[-1],
                                     FUN=function(x, y) if (x!="") return(x) else return(deparse(y)))
        }
    }
    if (!is.dimnames(x))
        stop("first argument is not a dimnames object and has no dimnames")
    for (i in seq(along=args)) {
        y <- get.dimnames(args[[i]])
        if (!is.dimnames(y))
            stop(args.names[i], "is not a dimnames object and has no dimnames")
        if (is.null(along)) {
            if (length(x) != length(y))
                stop(args.names[i], " has ", length(y), " dimensions but ", args.names[1], " has ", length(x))
            for (j in seq(along=x)) {
                dd <- union.ordered(x[[j]], y[[j]])
                if (!is.null(dd))
                    x[[j]] <- dd
            }
        } else {
            if (length(x) < max(along) || length(y) < max(along))
                stop(args.names[i], " has ", length(y), " dimensions and x has ", length(x), " but along=", paste(along, collaspe=", "))
            for (j in along) {
                dd <- union.ordered(x[[j]], y[[j]])
                if (!is.null(dd))
                    x[[j]] <- dd
            }
        }
    }
    x
}
