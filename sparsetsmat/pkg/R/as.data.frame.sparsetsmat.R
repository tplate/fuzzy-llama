as.data.frame.sparsetsmat <- function(x, ...) {
    # order of ids in the data frame may be different
    # than order in x$id
    # e.g., with ids=c('a','c','b'), id.idx=c(3,1,NA), id.noc=c(2,1,0)
    # the ids that line up with dates and values will be c('b','b','a')
    i <- x$id.noc > 0
    if (any(i & is.na(x$id.idx)))
        stop('have NA value in id.idx where id.noc>0')
    io <- order(ifelse(i, x$id.idx, NA), na.last=NA)
    df <- as.data.frame(list(dates=x$dates,
                             ids=rep(x$ids[io], x$id.noc[io]),
                             values=x$values),
                        row.names=NULL, stringsAsFactors=FALSE)
    if (!is.null(x$df.colnames))
        names(df) <- x$df.colnames
    attr(df, 'ids') <- x$ids
    attr(df, 'backfill') <- x$backfill
    attr(df, 'sort.ids') <- x$sort.ids
    class(df) <- c('sparsetsmat.df', 'data.frame')
    df
}
