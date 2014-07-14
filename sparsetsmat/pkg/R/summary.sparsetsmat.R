#' @rdname standard.array.methods
#' @method print sparsetsmat
print.sparsetsmat <- function(x, ...) {
    cat(paste(dim(x), collapse=' x '),
        " sparse persistent time-series matrix of class '", class(x), "'\n", sep='')
    cat("  rownames[", nrow(x), "] class '", class(x$all.dates)[1], "', ",
        describe.values(x$all.dates, countna=F, total=F, nuniq=F),  "\n", sep="")
    cat("  colnames[", ncol(x), "] class '", class(x$ids), "', ",
        describe.values(x$ids, countna=F, total=F, nuniq=F),  "\n", sep="")
    cat("  values class '", paste(class(x$values), collapse=':'), "', ",
        describe.values(sort(unique(x$values)), countna=T, total=T, nuniq=T),  "\n", sep="")
    cat("  data.frame column names: ", paste(x$df.colnames, collapse=', '), '\n', sep='')
    invisible(x)
}

# pick some initial elements from a vector
describe.values <- function(x,
                            max.examples=3,
                            collapse=TRUE,
                            quote=FALSE,
                            ellipsis=TRUE,
                            total=TRUE,
                            nuniq=TRUE,
                            countna=TRUE)
    some.examples(x=x,
                  max.examples=max.examples,
                  collapse=collapse,
                  quote=quote,
                  ellipsis=ellipsis,
                  total=total,
                  nuniq=nuniq,
                  countna=countna)

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
