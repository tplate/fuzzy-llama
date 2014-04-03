reshape.records <- function(x, ...) UseMethod("reshape.records")

# reshape a wide array or dataframe into records
# 'colnames' are the names of the dims -- as they are named in the return value
reshape.records.default <- function(x, colnames=NULL, retain.dim=NULL,
                                    row.label="row", col.label="col", data.label=NULL,
                                    factors=TRUE, na.rm=FALSE, df2d=FALSE) {
    if (is.null(dim(x)))
        stop("x is missing dim")
    if (is.null(dimnames(x)) || any(sapply(dimnames(x), is.null) & dim(x)>0))
        stop("x is missing dimension names")
    if (!is.null(retain.dim) && (length(retain.dim)!=1 || (retain.dim<1 || retain.dim>length(dim(x)))))
        stop("retain.dim must identify a dimension of x")
    if (is.null(colnames)) {
        colnames <- row.label
        if (length(dim(x))>1)
            colnames <- c(colnames, col.label)
        if (length(dim(x))>2 && length(dim(x)) > length(colnames))
            colnames <- c(colnames, paste("idx", seq(from=(length(colnames)+1),to=length(dim(x))), sep=""))
        if (!is.null(retain.dim) && length(colnames)==length(dim(x)))
            colnames <- colnames[-retain.dim]
        if (!is.null(data.label)) {
            if ((is.null(retain.dim) && length(data.label)==1)
                || (!is.null(retain.dim) && length(data.label)==dim(x)[retain.dim]))
                colnames <- c(colnames, data.label)
            else
                stop("data.label is wrong length")
        }
    }
    if (!is.null(retain.dim)) {
        if (is.null(data.label))
            data.label <- dimnames(x)[[retain.dim]]
        if (is.null(data.label))
            data.label <- paste("value", seq(dim(x)[retain.dim]), sep="")
        if (length(colnames)==length(dim(x))-1)
            colnames <- c(colnames, data.label)
        if (length(colnames) != (length(dim(x))-1+length(dimnames(x)[[retain.dim]])))
            stop("wrong number of elements in colnames")
    } else {
        if (is.null(data.label))
            data.label <- "value"
        if (length(colnames) == length(dim(x)))
            colnames <- c(colnames, data.label)
        if (length(colnames) != length(dim(x))+1)
            stop("wrong number of elements in colnames")
    }

    indices <- seq(1,length(dim(x)))
    if (!is.null(retain.dim)) {
        # make the retain.dim dimension the last
        if (retain.dim!=length(dim(x)))
            x <- aperm(x, c(indices[-retain.dim], retain.dim), resize=TRUE)
        # extract a sub-matrix with just 1 (or zero) elements in the last dim
        i1 <- min(1, lastelt(dim(x)))
        if (length(dim(x))==2)
            x1 <- x[,i1,drop=FALSE]
        else if (length(dim(x))==3)
            x1 <- x[,,i1,drop=FALSE]
        else if (length(dim(x))==4)
            x1 <- x[,,,i1,drop=FALSE]
        else if (length(dim(x))==5)
            x1 <- x[,,,,i1,drop=FALSE]
        else if (length(dim(x))==6)
            x1 <- x[,,,,,i1,drop=FALSE]
        else
            stop("too many dimensions in x!")
        # don't use sapply -- doesn't work in boundary cases
        # also slice.index has bug for x having a zero-extent dimension
        if (prod(dim(x)) > 0)
            records <- do.call("cbind", lapply(indices[-length(indices)], function(i, x) as.integer(slice.index(x, i)), x1))
        else
            records <- matrix(integer(0), nrow=0, ncol=length(dim(x))-1)
        # reshape x into a rectangular matrix
        dn <- dimnames(x)[-length(dimnames(x))]
        if (!is.data.frame(x) && !is.df2d(x)) {
            attr(x, "dimnames") <- NULL
            dim(x) <- c(prod(dim(x)[-length(dim(x))]), lastelt(dim(x)))
        } else {
            rownames(x) <- seq(len=nrow(x))
        }
        if (nrow(records))
            records <- cbind(records, x)
        else
            dim(records)[2] <- dim(records)[2]+lastelt(dim(x))
    } else {
        # don't use sapply -- doesn't work in boundary cases
        # also slice.index has bug for x having a zero-extent dimension
        if (prod(dim(x)) > 0)
            records <- do.call("cbind", lapply(indices, function(i, x) as.integer(slice.index(x, i)), x))
        else
            records <- matrix(integer(0), nrow=0, ncol=length(dim(x)))
        dn <- dimnames(x)
        if (is.data.frame(x) || is.df2d(x)) {
            # be careful with timeDate columns -- they do not unlist correctly
            x <- unlist(lapply(x, function(x) if (is(x,"timeDate")) as.character(x) else x))
        } else {
            attr(x, "dimnames") <- NULL
        }
        dim(x) <- c(prod(sapply(dn, length)), 1)
        if (nrow(records))
            records <- cbind(records, x)
        else
            dim(records)[2] <- dim(records)[2]+1
    }
    colnames(records) <- colnames
    if (df2d)
        records <- df2d(records, unfold=TRUE)
    else
        records <- data.frame(records, check.names=FALSE)
    for (i in seq(len=length(dn)))
        if (factors)
            records[[i]] <- structure(records[[i]], levels=dn[[i]], class="factor")
        else
            records[[i]] <- dn[[i]][records[[i]]]
    if (na.rm==TRUE)
        na.rm <- "all"
    if (na.rm!=FALSE && nrow(records)>0) {
        # the apply() inside here gets an error if there are no records
        if (na.rm=="all")
            records.ok <- which(!apply(is.na(records[,-(seq(len=length(dn))),drop=FALSE]), 1, all))
        else if (na.rm=="any")
            records.ok <- which(!apply(is.na(records[,-(seq(len=length(dn))),drop=FALSE]), 1, any))
        if (length(records.ok))
            records <- records[records.ok, ,drop=FALSE]
    }
    records
}

