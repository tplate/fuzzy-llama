.onUnload <- function( libpath ) {
  library.dynam.unload( "Rigroup", libpath )
}


igroupCounts <- function(x, i, na.rm=TRUE)
{
    R_IGFCOUNTS <- as.integer(0)
    if (!is.logical(x) && !is.integer(x) && !is.numeric(x)) {
        stop("first argument must be a logical, integer or real vector")  
    }
    if (is.list(i)) stop("second argument must be a single integer vector for igroup functions")
    if (is.numeric(i)) i <- as.integer(i)
    storage.mode(i) <- "integer"
    r <- .External("igroupFuns", x, i, R_IGFCOUNTS, na.rm, PACKAGE="Rigroup")
    r
}

igroupSums <- function(x, i, na.rm=TRUE)
{
    R_IGFSUMS <- as.integer(1)
    if (!is.logical(x) && !is.integer(x) && !is.numeric(x)) {
        stop("first argument must be a logical, integer or real vector")  
    }
    if (is.list(i)) stop("second argument must be a single integer vector for igroup functions")
    if (is.numeric(i)) i <- as.integer(i)
    storage.mode(i) <- "integer"
    r <- .External("igroupFuns", x, i, R_IGFSUMS, na.rm, PACKAGE="Rigroup")
    r
}

igroupMaxs <- function(x, i, na.rm=TRUE)
{
    R_IGFMAXS <- as.integer(2)
    if (!is.logical(x) && !is.integer(x) && !is.numeric(x)) {
        stop("first argument must be a logical, integer or real vector")  
    }
    if (is.list(i)) stop("second argument must be a single integer vector for igroup functions")
    if (is.numeric(i)) i <- as.integer(i)
    storage.mode(i) <- "integer"
    r <- .External("igroupFuns", x, i, R_IGFMAXS, na.rm, PACKAGE="Rigroup")
    r
}

igroupMins <- function(x, i, na.rm=TRUE)
{
    R_IGFMINS <- as.integer(3)
    if (!is.logical(x) && !is.integer(x) && !is.numeric(x)) {
        stop("first argument must be a logical, integer or real vector")  
    }
    if (is.list(i)) stop("second argument must be a single integer vector for igroup functions")
    if (is.numeric(i)) i <- as.integer(i)
    storage.mode(i) <- "integer"
    r <- .External("igroupFuns", x, i, R_IGFMINS, na.rm, PACKAGE="Rigroup")
    r
}

igroupAnys <- function(x, i, na.rm=TRUE)
{
    R_IGFMAXS <- as.integer(2)
    if (!is.logical(x)) stop("first argument must be a logical vector")  
    if (is.list(i)) stop("second argument must be a single integer vector for igroup functions")
    if (is.numeric(i)) i <- as.integer(i)
    storage.mode(i) <- "integer"
    r <- .External("igroupFuns", x, i, R_IGFMAXS, na.rm, PACKAGE="Rigroup")
    r
}

igroupAlls <- function(x, i, na.rm=TRUE)
{
    R_IGFMINS <- as.integer(3)
    if (!is.logical(x)) stop("first argument must be a logical vector")  
    if (is.list(i)) stop("second argument must be a single integer vector for igroup functions")
    if (is.numeric(i)) i <- as.integer(i)
    storage.mode(i) <- "integer"
    r <- .External("igroupFuns", x, i, R_IGFMINS, na.rm, PACKAGE="Rigroup")
    r
}

igroupProds <- function(x, i, na.rm=TRUE)
{
    R_IGFPRODS <- as.integer(4)
    if (!is.logical(x) && !is.integer(x) && !is.numeric(x)) {
        stop("first argument must be a logical, integer or real vector")  
    }
    if (is.list(i)) stop("second argument must be a single integer vector for igroup functions")
    if (is.numeric(i)) i <- as.integer(i)
    storage.mode(i) <- "integer"
    r <- .External("igroupFuns", x, i, R_IGFPRODS, na.rm, PACKAGE="Rigroup")
    r
}

igroupMeans <- function(x, i, na.rm=TRUE)
{
    R_IGFCOUNTS <- as.integer(0)
    R_IGFSUMS <- as.integer(1)
    if (!is.logical(x) && !is.integer(x) && !is.numeric(x)) {
        stop("first argument must be a logical, integer or real vector")  
    }
    if (is.list(i)) stop("second argument must be a single integer vector for igroup functions")
    if (is.numeric(i)) i <- as.integer(i)
    storage.mode(i) <- "integer"
    sums <- .External("igroupFuns", x, i, R_IGFSUMS, na.rm, PACKAGE="Rigroup")
    cnts <- .External("igroupFuns", x, i, R_IGFCOUNTS, na.rm, PACKAGE="Rigroup")
    r <- as.numeric(sums) / as.numeric(cnts)
    is.na(r[cnts==0]) <- TRUE
    r
}

igroupRanges <- function(x, i, na.rm=TRUE)
{
    R_IGFMAXS <- as.integer(2)
    R_IGFMINS <- as.integer(3)
    if (!is.logical(x) && !is.integer(x) && !is.numeric(x)) {
        stop("first argument must be a logical, integer or real vector")  
    }
    if (is.list(i)) stop("second argument must be a single integer vector for igroup functions")
    if (is.numeric(i)) i <- as.integer(i)
    storage.mode(i) <- "integer"
    mxs <- .External("igroupFuns", x, i, R_IGFMAXS, na.rm, PACKAGE="Rigroup")
    mns <- .External("igroupFuns", x, i, R_IGFMINS, na.rm, PACKAGE="Rigroup")
    r <- as.numeric(mxs) - as.numeric(mns)
    r
}
