# reshape.matrix(data.frame(x=factor(c(1,1,2,4),levels=1:4),y=c("a","b","a","c"),z=1:4))

# reshape.matrix(data.frame(r=factor(c("r1","r1","r1","r2","r2","r2")),c=factor(c("X","NA","Y","X","NA","Y"),exclude=NULL),v=1:6))

reshape.matrix <- function(x, row.names=1, col.names=2, warn.missing=FALSE, rows=NULL, cols=NULL,
                           indices=NULL, sort.rows=TRUE, sort.cols=TRUE, missing.rows=FALSE,
                           presence.map=FALSE, as.data.frame=FALSE, drop.unused.levels=TRUE,
                           na.warn=TRUE, dimnames.names=FALSE, need.dimnames=NULL, direct=FALSE, sep='.', sep.fixed=TRUE) {
    if (((!missing(row.names) || !missing(col.names)) + (!is.null(rows) || !is.null(cols)) + (!is.null(indices))) > 2)
        stop("can only supply one of 'row.names'+'col.names', 'rows'+'cols', or 'indices'")
    x.names <- NULL #
    if (!is.null(rows) || !is.null(cols) || !is.null(indices)) {
        # x is a vector or a single-column matrix
        if (!is.null(dim(x))) {
            if (length(dim(x))!=2)
                stop("x must be a matrix")
            if (ncolg(x)!=1)
                stop("x must have one column when 'rows'+'cols', or 'indices' is supplied")
            values <- x[ ,1, drop=TRUE]
        } else {
            values <- x
        }
        if (!is.null(indices)) {
            if (!is.list(indices) || length(indices)!=2)
                stop("'indices' must be a list containing two vectors")
            rows <- indices[[1]]
            cols <- indices[[2]]
        }
        if (length(rows)!=length(values))
            stop("length of rows must be equal to length of 'x'")
        if (length(cols)!=length(values))
            stop("length of cols must be equal to length of 'x'")
        x.colnames <- NULL
    } else if (length(dim(x))==2) {
        # turn a 3-column dataframe or matrix into a rectangular matrix
        x.colnames <- colnames(x)
        if (dim(x)[2]!=3)
            stop("x must have 3 columns")
        if (is.character(row.names))
            row.names <- match(row.names, colnames(x))
        if (is.character(col.names))
            col.names <- match(col.names, colnames(x))
        val.col <- setdiff(1:3,c(row.names, col.names))
        if (length(val.col)!=1)
            stop("must specify row and col as columns of x")
        if (!is.null(colnames(x)))
            x.names <- colnames(x)[c(row.names, col.names, val.col)]
        if (is.list(x)) {
            # data.table object behave erratically with x[,1] indexing
            values <- x[[val.col]]
            rows <- x[[row.names]]
            cols <- x[[col.names]]
        } else {
            values <- x[, val.col]
            rows <- x[, row.names]
            cols <- x[, col.names]
        }
    } else if ((length(dim(x)) < 2) && !is.null(names(x))) {
        values <- unname(x)
        i <- regexpr(sep, names(x), fixed=sep.fixed)
        if (any(i <= 0))
            stop('sep not found in names on some x, e.g.: ', names(x)[which(i<1)[1]])
        rows <- substring(names(x), 1, pmax(i-1, 0))
        cols <- substring(names(x), i+attr(i, 'match.length'))
        if (any(nchar(rows) < 1))
            stop('nothing before sep in names on some x, e.g.: ', names(x)[which(nchar(rows)<1)[1]])
        if (any(nchar(cols) < 1))
            stop('nothing before sep in names on some x, e.g.: ', names(x)[which(nchar(cols)<1)[1]])
        x.colnames <- NULL
    } else {
        stop('x must be 2d with three columns or 1d with names')
    }
    if (is.null(x.names))
        x.names <- c("row", "col", "value")

    if (direct && (!is.numeric(rows) || !is.numeric(cols)))
        stop("rows and cols specifications must be numeric when direct=TRUE")

    # have multiple rows and columns; do a matrix indexing version
    if (is.factor(rows)) {
        if (drop.unused.levels)
            rows <- rows[,drop=TRUE]
        row.dimnames <- levels(rows)
        rows <- as.numeric(rows)
        if (sort.rows) {
            row.ord <- order(row.dimnames)
            if (!all(diff(row.ord)==1)) {
                row.dimnames <- row.dimnames[row.ord]
                rows <- order(row.ord)[rows]
            }
        }
        row.bad <- is.na(rows) | rows<=0 | rows>length(row.dimnames)
    } else if (inherits(rows, 'Date')) {
        row.bad <- is.na(rows)
        if (sort.rows)
            uniq.dates <- sort(unique(rows[!row.bad]))
        else
            uniq.dates <- unique(rows[!row.bad])
        row.dimnames <- as.character(uniq.dates)
        rows <- match(as.numeric(rows), as.numeric(uniq.dates))
    } else if (is.numeric(rows) && direct) {
        if (any(rows < 1 & !is.na(rows)))
            rows[is.true(rows<1)] <- NA
        row.dimnames <- char(seq(len=max(rows, na.rm=TRUE)))
        row.bad <- is.na(rows)
    } else {
        levs.native <- unique(rows)
        if (sort.rows)
            levs.native <- sort(levs.native, na.last=NA)
        rows <- match(rows, levs.native, incomp=NA)
        row.dimnames <- as.character(levs.native)
        row.bad <- is.na(rows)
    }
    if (na.warn && any(row.bad))
        warning("have ", sum(row.bad), " NA values in row identifiers")

    if (is.factor(cols)) {
        if (drop.unused.levels)
            cols <- cols[,drop=TRUE]
        col.dimnames <- levels(cols)
        cols <- as.numeric(cols)
        if (sort.cols) {
            col.ord <- order(col.dimnames)
            if (!all(diff(col.ord)==1)) {
                col.dimnames <- col.dimnames[col.ord]
                cols <- order(col.ord)[cols]
            }
        }
        col.bad <- is.na(cols) | cols<=0 | cols>length(col.dimnames)
    } else if (inherits(cols, 'Date')) {
        col.bad <- is.na(cols)
        if (sort.cols)
            uniq.dates <- sort(unique(cols[!col.bad]))
        else
            uniq.dates <- unique(cols[!col.bad])
        col.dimnames <- as.character(uniq.dates)
        cols <- match(as.numeric(cols), as.numeric(uniq.dates))
    } else if (is.numeric(cols) && direct) {
        if (any(cols < 1 & !is.na(cols)))
            cols[is.true(cols<1)] <- NA
        col.dimnames <- char(seq(len=max(cols, na.rm=TRUE)))
        col.bad <- is.na(cols)
    } else {
        levs.native <- unique(cols)
        if (sort.cols)
            levs.native <- sort(levs.native, na.last=NA)
        cols <- match(cols, levs.native, incomp=NA)
        col.dimnames <- as.character(levs.native)
        col.bad <- is.na(cols)
    }
    if (na.warn && any(col.bad))
        warning("have ", sum(col.bad), " NA values in col identifiers")

    # Make sure we don't have an zero indices ending up in our matrix indexing,
    # because it gets really screwed up! (zeros in the indices do not "consume"
    # values, which results in values getting put in the wrong place)
    bad <- which(row.bad | col.bad)
    if (length(bad)) {
        cols <- cols[-bad]
        rows <- rows[-bad]
        values <- values[-bad]
    }

    x.val.na <- replace(values[1], TRUE, NA)
    # Reuse x to save space
    if (is.factor(values)) {
        if (!as.data.frame) {
            x <- matrix(as.integer(NA), nrow=length(row.dimnames), ncol=length(col.dimnames),
                        dimnames=list(row.dimnames, col.dimnames))
            x[cbind(rows, cols)] <- as.integer(values)
        } else {
            x <- matrix(as.integer(NA), nrow=length(row.dimnames), ncol=length(col.dimnames),
                        dimnames=list(row.dimnames, col.dimnames))
            x[cbind(rows, cols)] <- as.numeric(values)
            # convert each column to a factor
            # the lapply stripped the row.names attribute, so put it back
            x <- data.frame(lapply(data.frame(x, check.names=FALSE), structure, levels=levels(values), class="factor"),
                            row.names=row.dimnames,
                            check.names=FALSE)
        }
    } else if (is.atomic(values) && is.null(attr(values, "class"))) {
        # generate a matrix of the appropriate mode, filled with NA
        x <- matrix(x.val.na, nrow=length(row.dimnames), ncol=length(col.dimnames),
                    dimnames=list(row.dimnames, col.dimnames))
        x[cbind(rows, cols)] <- values
        if (as.data.frame)
            x <- data.frame(x, check.names=FALSE)
    } else {
        # Some classes don't like having dimensions, e.g., "Date"
        if (!as.data.frame)
            stop("cannot return non-atomic data as a matrix")
        # This is tricky because matrix indexing doesn't work for
        # data frames with non-atomic, non-factor data.
        # Create a column the same class as values and filled with NA
        one.col <- rep(x.val.na, length(row.dimnames))
        cols.f <- structure(cols, levels=col.dimnames, class="factor")
        x <- tapply(seq(len=length(values)), cols.f,
                    function(data.idx, one.col, rows, data) {
                        one.col[rows[data.idx]] <- data[data.idx]
                        one.col
                    }, one.col, rows, values)
        # x will now be a list of the desired columns, but will have NULL
        # in columns for which there was no data
        x <- lapply(x, function(col, one.col) if (is.null(col)) one.col else col, one.col)
        x <- data.frame(x, check.names=FALSE, row.names=row.dimnames)
    }
    missing <- character(0)
    created.rows <- NULL
    if (presence.map || missing.rows || warn.missing) {
        map <- matrix(FALSE, nrow=nrow(x), ncol=ncol(x), dimnames=dimnames(x))
        map[cbind(rows, cols)] <- TRUE
        if (any(!map) && (missing.rows || warn.missing)) {
            created.rows <- which(!map, arr.ind=TRUE)
            created.rows <- data.frame(x=if (length(rownames(map))) rownames(map)[created.rows[,1]] else created.rows[,1],
                                       y=if (length(colnames(map))) colnames(map)[created.rows[,2]] else created.rows[,2],
                                       value=if (any(!map)) x.val.na else x.val.na[0])
            names(created.rows) <- x.names
            rownames(created.rows) <- paste(created.rows[,1], created.rows[,2], sep=".")
            if (nrow(created.rows)>0) {
                missing <- paste(x.names[1], "=", created.rows[,1], ", ",
                                 x.names[2], "=", created.rows[,2], sep="")
                if (warn.missing)
                    warning("some rows were missing and were inserted, e.g. for rows: ",
                            some.examples(missing),
                            "; see attr(,\"created.rows\") for a full list")
            }
        }
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

    if (presence.map)
        attr(x, "presence.map") <- map
    if (presence.map || !is.null(created.rows))
        attr(x, "created.rows") <- created.rows
    if (!is.null(x.colnames) && dimnames.names)
        names(dimnames(x)) <- x.colnames[c(row.names,col.names)]
    return(x)
}
