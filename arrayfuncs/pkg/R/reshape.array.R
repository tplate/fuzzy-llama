# reshape.array(data.frame(x=factor(c(1,1,2,4),levels=1:4),y=c("a","b","a","c"),z=1:4), 1, 2)

# reshape.array(data.frame(r=factor(c("r1","r1","r1","r2","r2","r2")),c=factor(c("X","NA","Y","X","NA","Y")),v=1:6), 1, 2)

reshape.array <- function(x, ..., sort.indices=TRUE, numeric.dimnames=TRUE, as.data.frame=FALSE, drop.unused.levels=TRUE, na.warn=TRUE, extra.dim=FALSE, extra.dim.name="columns", dimnames.names=FALSE, need.dimnames=NULL, result.mode=NULL) {
    if (!is.matrix(x) && !is.data.frame(x))
        stop("x must be a matrix or data.frame")
    # turn a dataframe or matrix of records into an array
    # first get the numeric indices of all index columns
    index.cols <- list(...)
    if (!length(index.cols))
        stop("must specify which columns contain indices")
    for (i in seq(along=index.cols)) {
        if (length(index.cols[[i]])!=1)
            stop("each index-column (...) argument must specify a single column")
        if (is.character(index.cols[[i]]))
            index.cols[[i]] <- match(index.cols[[i]], colnames(x))
        else if (!is.numeric(index.cols[[i]]))
            stop("index columns must be character or numeric")
        if (is.na(index.cols[[i]]))
            stop(list(...)[[i]], " is not a column of x")
    }
    index.cols <- unlist(index.cols)
    if (any(duplicated(index.cols)))
        stop("duplicated index columns (maybe another argument was confused for an index column -- other argument names must be specified in full)")
    if (!is.null(result.mode))
        result.mode <- match.arg(result.mode, c("single", "double", "integer", "character", "logical"))

    # make sort.indices a logical vector the same length as index.cols
    if (is.logical(sort.indices))
        sort.indices <- rep(sort.indices, len=length(index.cols))
    if (is.character(sort.indices))
        sort.indices <- is.element(colnames(x)[index.cols], sort.indices)
    else if (is.numeric(sort.indices))
        sort.indices <- is.element(index.cols, sort.indices)

    # make numeric.dimnames a logical vector the same length as index.cols
    if (is.logical(numeric.dimnames))
        numeric.dimnames <- rep(numeric.dimnames, len=length(index.cols))
    if (is.character(numeric.dimnames))
        numeric.dimnames <- is.element(colnames(x)[index.cols], numeric.dimnames)
    else if (is.numeric(numeric.dimnames))
        numeric.dimnames <- is.element(index.cols, numeric.dimnames)

    # make drop.unused.levels a logical vector the same length as index.cols
    if (is.logical(drop.unused.levels))
        drop.unused.levels <- rep(drop.unused.levels, len=length(index.cols))
    if (is.character(drop.unused.levels))
        drop.unused.levels <- is.element(colnames(x)[index.cols], drop.unused.levels)
    else if (is.numeric(drop.unused.levels))
        drop.unused.levels <- is.element(index.cols, drop.unused.levels)

    # find which columns deliver values
    value.cols <- seq(numCols(x))[-index.cols]
    if (!length(value.cols))
        stop("no value columns in x!")

    # construct the dim and dimnames of the result
    # modify x to drop unused levels in index columns that are factors
    res.dim <- integer(0)
    res.dimnames <- list()
    for (i in seq(along=index.cols)) {
        idxs <- x[,index.cols[i]]
        if (is(idxs, "timeDate") || is(idxs, "dates") || is(idxs, "POSIXt") || is(idxs, "Date")) {
            if (sort.indices[i])
                idxs <- factor(as.character(idxs), levels=as.character(sort(unique(idxs))), exclude=NULL)
            else
                idxs <- factor(as.character(idxs), levels=as.character(unique(idxs)), exclude=NULL)
            res.dimnames[[i]] <- levels(idxs)
            res.dim[i] <- length(res.dimnames[[i]])
            x[,index.cols[i]] <- as.numeric(idxs)
        } else if (is.character(idxs)) {
            if (sort.indices[i])
                idxs <- factor(idxs, levels=sort(unique(idxs)), exclude=NULL)
            else
                idxs <- factor(idxs, levels=unique(idxs), exclude=NULL)
            res.dimnames[[i]] <- levels(idxs)
            res.dim[i] <- length(res.dimnames[[i]])
            x[,index.cols[i]] <- as.numeric(idxs)
        } else if (is.wholenumber(idxs)) {
            if (numeric.dimnames[i]) {
                if (drop.unused.levels[i]) {
                    levs <- sort(unique(idxs))
                    if (length(levs) && (levs[1]!=1 || any(diff(levs)!=1)))
                        x[,index.cols[i]] <- match(idxs, levs)
                } else {
                    levs <- seq(min(1, idxs, na.rm=TRUE), max(idxs, na.rm=TRUE))
                    if (levs[1] < 1)
                        stop("cannot have numeric index < 1 -- try using drop.unused.levels=TRUE")
                }
                res.dimnames[[i]] <- levs
                res.dim[i] <- length(res.dimnames[[i]])
            } else {
                if (min(idxs, na.rm=TRUE) < 1)
                    stop("cannot have numeric index < 1 -- try using numeric.dimnames=TRUE and drop.unused.levels=TRUE")
                res.dimnames[[i]] <- empty.dimnames()
                res.dim[i] <- max(idxs, na.rm=TRUE)
            }
        } else if (is.logical(idxs)) {
            if (sort.indices[i])
                idx.values <- sort(unique(idxs))
            else
                idx.values <- unique(idxs)
            res.dimnames[[i]] <- as.character(idx.values)
            res.dim[i] <- length(res.dimnames[[i]])
            x[,index.cols[i]] <- match(idxs, idx.values)
        } else if (is.factor(idxs)) {
            if (drop.unused.levels[i])
                idxs <- idxs[,drop=TRUE]
            if (sort.indices[i])
                idxs <- reorder.levels(idxs, order(levels(idxs)))
            res.dimnames[[i]] <- levels(idxs)
            res.dim[i] <- length(res.dimnames[[i]])
            x[,index.cols[i]] <- as.numeric(idxs)
        } else {
            stop("index column ", i, " (column ", index.cols[i], " in x) is not numeric, factor, character, timeDate, or dates")
        }
    }
    if (!is.null(colnames(x)) && dimnames.names)
        names(res.dimnames) <- colnames(x)[index.cols]

    if (length(value.cols) > 1 || extra.dim) {
        res.dim[length(index.cols)+1] <- length(value.cols)
        if (!is.null(colnames(x))) {
            res.dimnames[[length(index.cols)+1]] <- colnames(x)[value.cols]
            if (dimnames.names)
                names(res.dimnames)[length(index.cols)+1] <- extra.dim.name
        } else {
            res.dimnames[[length(index.cols)+1]] <- empty.dimnames()
        }
    }

    # Split x into two parts: indices and values
    if (is.data.frame(x))
        x.indices <- as.matrix(data.frame(lapply(x[, index.cols, drop=FALSE], as.numeric), check.names=FALSE))
    else
        x.indices <- x[, index.cols, drop=FALSE]
    x.values <- x[, value.cols, drop=FALSE]

    # construct the array to hold the result (filled with appropriate NA values)
    if (is.data.frame(x.values)) {
        if (length(x.values) && any(sapply(x.values, is.factor)))
            stop("x contains factor values")
        # see what all the column types promote to
        x.first.row <- unlist(x.values[1, ], use.names=FALSE)
    } else {
        x.first.row <- x.values[1, ,drop=TRUE]
    }
    if (is.numeric(x.first.row) && is.null(result.mode))
        result.mode <- storage.mode(x.first.row)
    # make sure all the columns in x.values are of the appropriate type
    if (storage.mode(x.first.row)!=result.mode) {
        if (is.data.frame(x.values)) {
            for (i in seq(len=ncol(x.values)))
                storage.mode(x.values[[i]]) <- result.mode
        } else if (is.matrix(x.values)) {
            storage.mode(x.values) <- result.mode
        } else {
            stop("don't know how to handle x.values")
        }
    }
    x <- vector(result.mode, prod(res.dim))
    if (result.mode!="character")
        x[] <- NA
    attr(x, "dim") <- res.dim
    if (!all(sapply(res.dimnames, is.empty)))
        attr(x, "dimnames") <- res.dimnames

    # Make sure we don't have an zero indices ending up in our matrix indexing,
    # because it gets really screwed up! (zeros in the indices do not "consume"
    # values, which results in values getting put in the wrong place)
    dimnames(x.indices) <- NULL
    if (!is.data.frame(x.values))
        dimnames(x.values) <- NULL
    if (numRows(x.indices)>0) {
        x.rows.ok <- apply(x.indices, 1, function(x) all(!is.na(x) & x>=1))
        if (!all(x.rows.ok)) {
            if (na.warn)
                warning(sum(!x.rows.ok), " rows in x being dropped due to NA or non-positive index values")
            # drop rows with illegal index values
            x.indices <- x.indices[x.rows.ok,,drop=FALSE]
            x.values <- x.values[x.rows.ok,,drop=FALSE]
        }
    }

    # how many elements does a single increment at each index position count for?
    res.dim.ext <- as.integer(cumprod(c(1,res.dim[-length(res.dim)])))
    if (numRows(x.values)==0 || numCols(x.values)==0) {
        res.address <- integer(0)
    } else if (numCols(x.values)>1 || extra.dim) {
        # work out the starting address in the result for each row of x
        res.address <- as.integer(((x.indices-1) %*% res.dim.ext[-length(res.dim.ext)]) + 1)
        # and add the appropriate offset for each column
        # work hard to keep res.address as an integer
        if (lastelt(res.dim) != ncol(x.values))
            stop("expecting lastelt(res.dim) == ncol(x.values))")
        for (j in seq(ncol(x.values))) {
            col.inc <- (j-1) * lastelt(res.dim.ext)
            x[res.address+col.inc] <- x.values[,j,drop=TRUE]
        }
        # Old version of working out res.address:
        # matrix() seems to use a lot memory for this relatively simple operation
        # also, matrix() turns res.address into a double
        #    res.address <- (matrix(res.address,ncol=numCols(x.values),nrow=numRows(res.address),byrow=FALSE) +
        #                    matrix(seq(0,by=lastelt(res.dim.ext),len=lastelt(res.dim)),byrow=TRUE,ncol=numCols(x.values),nrow=numRows(x.values)))
    } else {
        # work out the address in the result for value in x
        # even if x.indices has storage mode integer, the result of %*%
        # has storage mode double, so no point in converting to integer
        # before doing the %*%
        res.address <- as.integer(drop(((x.indices-1) %*% res.dim.ext) + 1))
        x[res.address] <- x.values[,1]
    }

    if (!is.null(need.dimnames)) {
        if (length(need.dimnames) != length(dim(x)))
            stop("need.dimnames has different number of elements than dim(x)")
        along <- numeric(0)
        for (i in seq(length(dim(x))))
            if (!identical(as.character(need.dimnames[[i]]), dimnames(x)[[i]]))
                along <- c(along, i)
        if (length(along))
            x <- conform.dimnames(x, union.dimnames(need.dimnames, dimnames(x)), excess.ok=TRUE, along=along)
    }

    return(x)
}
