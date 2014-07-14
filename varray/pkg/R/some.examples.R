some.examples <- function(x,
                          max.examples=3,
                          collapse=NULL,
                          quote=FALSE,
                          ellipsis=FALSE,
                          total=FALSE,
                          nuniq=FALSE,
                          countna=FALSE,
                          final=TRUE)
{
    len <- length(x)
    if (len > max.examples + ellipsis) {
        z <- as.character(x[c(seq(len=max.examples -1 + ellipsis), len)])
    } else {
        z <- as.character(x)
    }
    if (length(z) > max.examples) {
        # work out whether to use "..." or z[max.examples]
        if (nchar(z[max.examples]) > 10 || len > max.examples+1)
            z[max.examples] <- '...'
    }
    if (is.logical(quote) && quote)
        quote <- "\""
    if (is.null(collapse))
        collapse <- total | nuniq | countna
    if (is.logical(collapse))
        if (collapse)
            collapse <- ", "
        else
            collapse <- NULL
    if (is.character(quote))
        z <- paste(quote, as.character(z), quote, sep="")
    if (!is.null(collapse))
        z <- paste(as.character(z), collapse=collapse)
    comment <- character(0)
    if (total) {
        comment <- c(comment, paste('length', len))
    }
    if (countna | nuniq) {
        nna <- sum(is.na(x))
        if (nuniq) {
            nuniq <- length(unique(x))
            comment <- c(comment, paste(nuniq - min(nna, 1), 'unique'))
        }
        if (countna)
            comment <- c(comment, paste(nna, if (nna==1) 'NA' else 'NAs'))
    }
    if (length(comment)) {
        if (!is.null(collapse))
            z <- paste(as.character(z), " (", paste(comment, collapse=collapse), ")", sep="")
        else
            z <- c(as.character(z), comment)
    }
    z
}
# pick some initial elements from a vector
some.examples.old <- function(x, max.examples=3, collapse=NULL,
                          quote=FALSE, ellipsis=FALSE, total=FALSE, final=FALSE)
{
    len <- length(x)
    # don't return something like 'A, B, ..., D' for 'A, B, C, D'
    if (len == max.examples+1 && ellipsis)
        max.examples <- len
    xc <- as.character(x)
    if (final)
        xc <- xc[unique(c(seq(len=min(len, max.examples-1)), len))]
    else
        xc <- xc[seq(len=min(len, max.examples))]
    if (is.logical(quote) && quote)
        quote <- "\""
    if (is.logical(collapse))
        if (collapse)
            collapse <- ", "
        else
            collapse <- NULL
    if (is.character(quote))
        xc <- paste(quote, xc, quote, sep="")
    if (ellipsis && len > max.examples)
        if (final)
            xc <- c(head(xc, -1), "...", tail(xc, 1))
        else
            xc <- c(xc, "...")
    if (!is.null(collapse))
        xc <- paste(xc, collapse=collapse)
    if (total && len > max.examples)
        if (!is.null(collapse))
            xc <- paste(xc, " (", len, " total)", sep="")
        else
            xc <- c(xc, paste("(", len, " total)", sep=""))
    xc
}
