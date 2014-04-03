shift.data <- function(x, shift, ..., along=1, fill.value=NA, persist=FALSE) UseMethod("shift.data")

shift.data.default <- function(x, shift, ..., along=1, fill.value=NA, persist=FALSE) {
    if (!is.wholenumber(shift) || length(shift)!=1 || is.na(shift))
        stop("'shift' must be a single whole number")
    if (along!=1)
        stop("not implemented for any value of 'along' other than 1")
    if (length(fill.value)!=1) {
        if (length(dim(x))==2) {
            if (length(fill.value) != dim(x)[-along])
                stop("'fill.value' must have length 1 or ", dim(x)[-along])
            # if filling in a bigger than one shift with a vector, really should
            # make fill.value into a matrix with the right number of rows
            # (or allow it to be a matrix too)
            if (abs(shift) != 1)
                stop("can only use vector 'fill.value' when shift==1")
        } else {
            stop("'fill.value' must have length 1")
        }
    }

    if (length(dim(x))==2) {
        n <- dim(x)[1]
    } else if (is(x, "vector")) {
        n <- length(x)
    } else {
        stop("x must be a matrix or vector")
    }
    k <- numeric(0)
    if (shift==0) {
        return(x)
    } else if (shift>0) {
        j <- seq(1, len=max(0, n-shift))
        if (persist)
            j <- c(rep(1, n-length(j)), j)
        else
            k <- seq(1, len=min(n, shift))
        i <- seq(to=n, len=length(j))
    } else {
        # shift < 0
        j <- seq(to=n, len=max(0, n+shift))
        if (persist)
            j <- c(j, rep(n, n-length(j)))
        else
            k <- seq(to=n, len=min(n, -shift))
        i <- seq(1, len=length(j))
    }
    if (length(dim(x))==2) {
        x[i,] <- x[j,,drop=FALSE]
        if (length(k))
            x[k,] <- fill.value
    } else if (is(x, "vector")) {
        x[i] <- x[j]
        if (length(k))
            x[k] <- fill.value
    } else {
        stop("x must be a matrix or a vector")
    }
    x
}
