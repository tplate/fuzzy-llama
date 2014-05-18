as.data.frame.sparsetsmat <- function(x, ...) {
    df <- list(x$date, rep(x$id, x$id.noc), x$value)
    names(df) <- x$df.colnames
    return(as.data.frame(df, row.names=NULL))
}
