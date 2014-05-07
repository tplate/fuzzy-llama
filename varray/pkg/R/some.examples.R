# pick some initial elements from a vector
some.examples <- function(x, max.examples=3, collapse=NULL,
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
