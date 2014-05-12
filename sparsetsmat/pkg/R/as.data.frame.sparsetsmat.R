as.data.frame.sparsetsmat <- function(tsm) {
    df <- list(tsm$date, rep(tsm$id, diff(tsm$id.idx)), tsm$value)
    names(df) <- tsm$df.colnames
    return(as.data.frame(df, row.names=NULL))
}
