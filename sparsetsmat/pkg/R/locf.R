locf.default <- function(x, initial=NA, distance=NA, toofar=initial, dims=1, fast=FALSE) {
    # for higher-d arrays, dims has the same meaning as for colSums()
    # dims=1 means carry forward only along the first dimension
    # dims=2 means carry forward around in the matrix formed by dim 1 and dim 2
    bapply <- function(...) stop('not provided here')
    if (length(initial)!=1)
        stop("initial must have length 1")
    if (length(toofar)!=1)
        stop("toofar must have length 1")
    if (length(distance)!=1)
        stop("distance must have length 1")
    if (length(dim(x))>1) {
        if (length(dims)!=1)
            stop("dims must have length 1")
        if (length(dim(x))>1 && (dims < 1 || dims>=length(dim(x))))
            stop("dims must be between 1 and ", length(dim(x))-1)
    }
    if (is.list(x) && !is.virtual.array(x)) {
        # list
        if (is.data.frame(x) || is.null(oldClass(x))) {
            x.attr <- attributes(x)
            z <- lapply(x, locf, initial, distance, toofar)
            attributes(z) <- x.attr
        } else {
            stop("no method for class '", oldClass(x), "'")
        }
    } else if (is.virtual.array(x) && length(dim(x))>1 && prod(dim(x))>0) {
        # virtual non-empty array
        if (FALSE && fast) {
            z <- as.array(x)
            dimnames(z) <- NULL
            if (length(dim(z)) > 2)
                dim(z) <- c(prod(dim(z)[seq(len=dims)]), prod(dim(z)[-seq(len=dims)]))
            # z <- .Call('locf', z, initial, as.integer(distance), toofar, dims=1L, inplace=1L)
            dim(z) <- dim(x)
            dimnames(z) <- dimnames(x)
        } else {
            z <- bapply(x, seq(dims+1, to=length(dim(x))), locf, initial, distance, toofar)
        }
    } else if (FALSE && fast && length(x) > 0) {
        # ordinary array or vector, use fast method
        z <- x
        dimnames(z) <- NULL
        if (length(dim(z)) > 2)
            dim(z) <- c(prod(dim(z)[seq(len=dims)]), prod(dim(z)[-seq(len=dims)]))
        # z <- .Call('locf', z, initial, as.integer(distance), toofar, dims=1L, inplace=0L)
        dim(z) <- dim(x)
        dimnames(z) <- dimnames(x)
    } else if (length(dim(x))>1 && prod(dim(x))>0) {
        # ordinary non-empty array
        # work out a logical vector for each 'column' with a T at the front and the rest FALSE
        col.pattern <- replace(logical(prod(dim(x)[1:dims])), 1, TRUE)
        z.pos <- which(!is.na(x) | col.pattern)
        z <- x
        if (!is.na(initial)) {
            i <- seq(1, by=length(col.pattern), len=length(x)/length(col.pattern))
            j <- which(is.na(x[i]))
            if (length(j))
                z[i] <- replace(x[i], j, initial)
        }
        runlen <- c(diff(z.pos), length(x) - z.pos[length(z.pos)] + 1)
        if (!is.na(distance) && length(i <- which(runlen > distance+1))) {
            toofarlen <- replace(numeric(length(runlen)), i, runlen[i] - (distance+1))
            runlen <- replace(runlen, i, distance+1)
            z <- rep(as.vector(rbind(z[z.pos], rep(toofar, length(z.pos)))),
                     as.vector(rbind(runlen, toofarlen)))
        } else {
            z <- rep(z[z.pos], runlen)
        }
        attributes(z) <- attributes(x)
    } else if (length(x)>0) {
        # vector with length > 0
        # don't do any check on 'dims' for a vector
        # if (dims!=1) stop("dims must be 1 for a vector")
        if (!is.na(initial) && is.na(x[1]))
            z <- replace(x, 1, initial)
        else
            z <- x
        z.pos <- which(!is.na(z))
        if (length(z.pos)==0 || z.pos[1] != 1)
            z.pos <- c(1, z.pos)
        runlen <- c(diff(z.pos), length(z) - z.pos[length(z.pos)] + 1)
        if (!is.na(distance) && length(i <- which(runlen > distance+1))) {
            toofarlen <- replace(numeric(length(runlen)), i, runlen[i] - (distance+1))
            runlen <- replace(runlen, i, distance+1)
            z <- rep(as.vector(rbind(z[z.pos], rep(toofar, length(z.pos)))),
                       as.vector(rbind(runlen, toofarlen)))
        } else {
            z <- rep(z[z.pos], runlen)
        }
        attributes(z) <- attributes(x)
    } else {
        # array or vector with length==0
        z <- x
    }
    return(z)
}

locf <- function(x, initial=NA, distance=NA, toofar=initial, dims=1) UseMethod("locf")
nocb <- function(x, initial=NA, distance=NA, toofar=initial, dims=1) UseMethod("nocb")
nocb.default <- function(x, initial=NA, distance=NA, toofar=initial, dims=1) {
    bapply <- function(...) stop('not provided here')
    if (length(initial)!=1)
        stop("initial must have length 1")
    if (length(toofar)!=1)
        stop("toofar must have length 1")
    if (length(distance)!=1)
        stop("distance must have length 1")
    if (length(dim(x))>1) {
        if (length(dims)!=1)
            stop("dims must have length 1")
        if (length(dim(x))>1 && (dims < 1 || dims>=length(dim(x))))
            stop("dims must be between 1 and ", length(dim(x))-1)
    }
    if (is.list(x) && !is.virtual.array(x)) {
        # list
        if (is.data.frame(x) || is.null(oldClass(x))) {
            x.attr <- attributes(x)
            z <- lapply(x, nocb, initial, distance, toofar)
            attributes(z) <- x.attr
        } else {
            stop("no method for class '", oldClass(x), "'")
        }
    } else if (is.virtual.array(x) && length(dim(x))>1 && prod(dim(x))>0) {
        # virtual non-empty array
        z <- bapply(x, seq(dims+1, to=length(dim(x))), nocb, initial, distance, toofar)
    } else if (length(dim(x))>1 && prod(dim(x))>0) {
        # ordinary non-empty array
        # work out a logical vector for each 'column' with a TRUE at the end and the rest FALSE
        n <- prod(dim(x)[1:dims])
        col.pattern <- replace(logical(n), n, TRUE)
        z.pos <- which(!is.na(x) | col.pattern)
        z <- x
        if (!is.na(initial)) {
            i <- seq(n, by=n, len=length(x)/n)
            j <- which(is.na(x[i]))
            if (length(j))
                z[i] <- replace(x[i], j, initial)
        }
        runlen <- c(z.pos[1], diff(z.pos))
        if (!is.na(distance) && length(i <- which(runlen > distance+1))) {
            toofarlen <- replace(numeric(length(runlen)), i, runlen[i] - (distance+1))
            runlen <- replace(runlen, i, distance+1)
            z <- rep(as.vector(rbind(rep(toofar, length(z.pos)), z[z.pos])),
                     as.vector(rbind(toofarlen, runlen)))
        } else {
            z <- rep(z[z.pos], runlen)
        }
        attributes(z) <- attributes(x)
    } else if (length(x)>0) {
        # vector with length > 0
        # if (dims!=1) stop("dims must be 1 for a vector")
        if (!is.na(initial) && is.na(x[length(x)]))
            z <- replace(x, length(x), initial)
        else
            z <- x
        z.pos <- which(!is.na(z))
        if (length(z.pos)==0 || z.pos[length(z.pos)] != length(x))
            z.pos <- c(z.pos, length(x))
        runlen <- c(z.pos[1], diff(z.pos))
        if (!is.na(distance) && length(i <- which(runlen > distance+1))) {
            toofarlen <- replace(numeric(length(runlen)), i, runlen[i] - (distance+1))
            runlen <- replace(runlen, i, distance+1)
            z <- rep(as.vector(rbind(rep(toofar, length(z.pos)), z[z.pos])),
                     as.vector(rbind(toofarlen, runlen)))
        } else {
            z <- rep(z[z.pos], runlen)
        }
        attributes(z) <- attributes(x)
    } else {
        # array or vector with length==0
        z <- x
    }
    return(z)
}

# reference implementation of nocb, which implements by simply reversing
nocb.ref <- function(x, initial=NA, distance=NA, toofar=initial, dims=1) UseMethod("nocb.ref")
nocb.ref.default <- function(x, initial=NA, distance=NA, toofar=initial, dims=1) {
    bapply <- function(...) stop('not provided here')
    if (length(initial)!=1)
        stop("initial must have length 1")
    if (length(toofar)!=1)
        stop("toofar must have length 1")
    if (length(distance)!=1)
        stop("distance must have length 1")
    if (length(dim(x))>1) {
        if (length(dims)!=1)
            stop("dims must have length 1")
        if (length(dim(x))>1 && (dims < 1 || dims>=length(dim(x))))
            stop("dims must be between 1 and ", length(dim(x))-1)
    }
    if (is.list(x) && !is.virtual.array(x)) {
        # list
        if (is.data.frame(x) || is.null(oldClass(x))) {
            x.attr <- attributes(x)
            z <- lapply(x, nocb.ref, initial, distance, toofar)
            attributes(z) <- x.attr
        } else {
            stop("no method for class '", oldClass(x), "'")
        }
    } else if (is.virtual.array(x) && length(dim(x))>1 && prod(dim(x))>0) {
        # virtual non-empty array
        z <- bapply(x, seq(dims+1, to=length(dim(x))), nocb.ref, initial, distance, toofar)
    } else if (length(dim(x))>1 && prod(dim(x))>0) {
        # ordinary non-empty array
        # work out a logical vector for each 'column' with a TRUE at the front and the rest FALSE
        x[] <- rev(x)
        col.pattern <- c(TRUE, rep(FALSE, prod(dim(x)[1:dims])-1))
        z.pos <- which(!is.na(x) | col.pattern)
        z <- x
        if (!is.na(initial)) {
            i <- seq(1, by=length(col.pattern), len=length(x)/length(col.pattern))
            j <- which(is.na(x[i]))
            if (length(j))
                z[i] <- replace(x[i], j, initial)
        }
        runlen <- c(diff(z.pos), length(x) - z.pos[length(z.pos)] + 1)
        if (!is.na(distance) && length(i <- which(runlen > distance+1))) {
            toofarlen <- replace(numeric(length(runlen)), i, runlen[i] - (distance+1))
            runlen <- replace(runlen, i, distance+1)
            z <- rep(as.vector(rbind(z[z.pos], rep(toofar, length(z.pos)))),
                     as.vector(rbind(runlen, toofarlen)))
        } else {
            z <- rep(z[z.pos], runlen)
        }
        z <- rev(z)
        attributes(z) <- attributes(x)
    } else if (length(x)>0) {
        # vector with length > 0
        x[] <- rev(x)
        # if (dims!=1) stop("dims must be 1 for a vector")
        if (!is.na(initial) && is.na(x[1]))
            z <- replace(x, 1, initial)
        else
            z <- x
        z.pos <- which(!is.na(z))
        if (length(z.pos)==0 || z.pos[1] != 1)
            z.pos <- c(1, z.pos)
        runlen <- c(diff(z.pos), length(z) - z.pos[length(z.pos)] + 1)
        if (!is.na(distance) && length(i <- which(runlen > distance+1))) {
            toofarlen <- replace(numeric(length(runlen)), i, runlen[i] - (distance+1))
            runlen <- replace(runlen, i, distance+1)
            z <- rep(as.vector(rbind(z[z.pos], rep(toofar, length(z.pos)))),
                       as.vector(rbind(runlen, toofarlen)))
        } else {
            z <- rep(z[z.pos], runlen)
        }
        z <- rev(z)
        attributes(z) <- attributes(x)
    } else {
        # array or vector with length==0
        z <- x
    }
    return(z)
}

is.virtual.array <- function(x) UseMethod('is.virtual.array')
is.virtual.array.default <- function(x) FALSE
