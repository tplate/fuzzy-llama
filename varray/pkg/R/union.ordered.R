# Compute the union, and keep the symbols in the same order as in x and y
# (provided that x and y are consistently ordered).

# Differs from union() in elements of y that are not in x can
# still appear before elements of x in the output.

# union.ordered(c("a","b","d","g"),c("a","c","e","g"))
# union.ordered(c("c","b"), c("a","c","d"))

union.ordered <- function(x, y) {
    if (is.null(x)) return(y)
    else if (is.null(y)) return(x)
    out.of.order <- any(x[-1] < x[-length(x)]) || any(y[-1] < y[-length(y)])
    if (!isTRUE(out.of.order)) {
        return(sort(union(x, y), na.last=TRUE))
    } else {
        # check if x and y are ordered consistently
        z <- intersect(x, y)
        z <- z[!is.na(z)]
        consistently.ordered <- all(diff(match(z, y))>=0) && all(diff(match(z, x))>=0)
        w <- union(x, y)
        if (!consistently.ordered)
            return(w)
        if (any(dup <- duplicated(x)))
            x <- x[!dup]
        if (any(dup <- duplicated(y)))
            y <- y[!dup]
        x.both <- is.element(x, z)
        y.both <- is.element(y, z)
        i <- 1 # count along x
        j <- 1 # count along y
        k <- 1 # count along w
        # transfer elts from x and y to w
        while (k <= length(w)) {
            while (!x.both[i] && i<=length(x)) {
                w[k] <- x[i]
                k <- k+1
                i <- i+1
            }
            while (!y.both[j] && j<=length(y)) {
                w[k] <- y[j]
                k <- k+1
                j <- j+1
            }
            if (i<=length(x)) {
                w[k] <- x[i]
                k <- k+1
                i <- i+1
                j <- j+1
            }
        }
        return(w)
    }
}
