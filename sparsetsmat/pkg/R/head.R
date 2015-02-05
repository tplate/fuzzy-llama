head.sparsetsmat <- function (x, n = 6L, ...)
{
    stopifnot(length(n) == 1L)
    n <- if (n < 0L)
        max(nrow(x) + n, 0L)
    else min(n, nrow(x))
    x[seq_len(n), , drop = FALSE]
}

tail.sparsetsmat <- function (x, n = 6L, addrownums = TRUE, ...)
{
    stopifnot(length(n) == 1L)
    nrx <- nrow(x)
    n <- if (n < 0L)
        max(nrx + n, 0L)
    else min(n, nrx)
    sel <- seq.int(to = nrx, length.out = n)
    ans <- x[sel, , drop = FALSE]
    if (addrownums && is.null(rownames(x)))
        rownames(ans) <- paste0("[", sel, ",]")
    ans
}
