#' Add time-series data to an object
#'
#' Add time-series data to an object
#' The only thing that makes an object time series data is that the
#' dimnames on one dimension are character strings representing parsable
#' times or dates.
#'
#' @param x An object to add time-series data to, either specified by name or by value.
#' Must have dimnames.
#' If specified by name (as a character vector), the object is changed in place.
#'
#' @param newdata New data to incorporate in the object.
#' Should have the same number of dimensions as the object
#' being updated.  Must have dimnames.
#'
#' @param comp.name
#' Base of name to use for the component objects in the varray.
#' Optional, default value is \code{paste('.', objectName, dateblock)}.
#' @param dateblock
#' How to translate dates into blocks.  Ignored if argument
#' \code{comp.name} is supplied. Default value is \code{"\%Y"}.
#' @param format
#' Format for parsing dates (which are supplied as dimension names on the
#' \code{along} dimension).
#' @param along
#' Default is 1.
#' @param dimorder
#' Default is standard dim order.
#' @param env.name
#' Intended to be the name of the environment where the components exist,
#' not yet fully tested.
#' @param envir
#' Intended to be the environment where the components exist,
#' not yet fully tested.
#' @param clear.dim numeric: dimension indices for which to
#' clear all values when present, e.g., with clear.dim=1,
#' all rows that match a row supplied in newdata are cleared
#'
#' @param naidxok
#' Set this attribute on the varray.  Specifies whether the component objects can handle \code{NA} indices.
#' @param change.mode.ok
#' Logical.  If TRUE, the mode of new data does not need to match existing data (default FALSE).
#' @param keep.ordered
#' Logical.  Specifies which dimensions should be kept ordered.  Can be a
#' single element or a vector with length equal to the number of
#' dimensions of \code{object}.
#' @param need.dimnames Dimension names that should be included in the updated object.
#' @param umode Not used (may be used in the future)
#' @param store.env.name Not used (may be used in the future)
#' @param fill The value to fill missing data with in subset operations
#' @param \dots
#' Not used, but needed because \code{update()} is a generic.
#'
#' @details
#' Contents of \code{data} are inserted into \code{object}, expanding the
#' dimensions of \code{object} and creating new component objects if necessary.
#' \code{update.varray.ts} is an obselete name for \code{add.tsdata.varray}.
#'
#'
#' @return The altered object \code{x}.  If \code{x} was the
#' name of an object, then the altered object is also
#' updated in place.
#'
#' @seealso Related to \code{\link{add.data}}, but
#' specialized for time-series data with arguments
#' specialized for handling dates.
#' \code{add.data} is the only supplied method for changing \code{\link{varray}} objects.
#'
#' @examples
#' x <- array(1:45, dim=c(15, 3), dimnames=list(format(seq(as.Date('2001-05-01'), len=15, by='months')), c('A','B','C')))
#' y <- array(101:121, dim=c(7, 3), dimnames=list(format(seq(as.Date(rownames(x)[nrow(x)]), len=7, by='months')), c('B','C','D')))
#' z <- array(1:72, dim=c(24, 3), dimnames=list(format(seq(as.Date('2001-05-01'), len=24, by='months')), c('A','B','C')))
#' add.tsdata.varray('v', z, comp.name='vvv.\%Y', along=1)
#' as.array(v)
#' add.tsdata.varray('v', y)
#' as.array(v)

add.tsdata <- function(x, ...) UseMethod('add.tsdata')

#' @rdname add.tsdata
#' @method add.tsdata default

add.tsdata.default <- function(x, ...) {
    if (is.character(x) && length(x)==1) {
        x.pos <- find(x, numeric=TRUE)
        y <- get(x)
        z <- add.tsdata(y, ...)
        assign(x, value=z, pos=x.pos[1])
        return(invisible(z))
    } else {
        stop('cannot handle object with class ', class(x))
    }
}

#' @rdname add.tsdata
#' @method add.tsdata varray

add.tsdata.varray <- function(x, newdata, comp.name=va$comp.name, dateblock='%Y', format=va$format,
                              # dates.by='bizdays', holidays='NYSEC', vmode='single',
                              along=va$along, dimorder=va$dimorder,
                              env.name=NULL, envir=NULL, clear.dim=NULL, naidxok=va$naidxok, change.mode.ok=FALSE,
                              need.dimnames=list(NULL, NULL),
                              keep.ordered=va$keep.ordered, umode=NULL, store.env.name=FALSE,
                              fill=NA, ...) {
    # have ... args to satisfy the generic function
    if (length(list(...)))
        warning('additional arguments ignored: ', paste(names(list(...)), collapse=', '))
    # TODO:
    #   Force use of umode
    #
    # Update a varray that stores time series matrix data.
    # 'newdata' is the new data
    # Labels on the binding 'along' dimension are dates, stored in order
    # Might need to create new component arrays in the varray.
    # Might need to create new columns in the varray -- though first
    # try to use up ones that have NA labels.
    # Return a varray, which is an in-memory object that refers to its components,
    # and has complete dimnames.
    # There might be some modification of underlying objects.

    # Work out what dates in data are new, and which are old
    # For old dates in data, find the objects to assign into,
    # increase their dims if necessary, and assign values into
    # them.

    # get 'va' as NULL or the varray
    # need to set 'va' and possibly 'va.name'

    va.name <- NULL
    newObj <- FALSE
    if (is.character(x) && length(x)==1 && is.null(dim(x))) {
        va.name <- x
        if (is.null(envir)) {
            x.pos <- find(va.name, numeric=TRUE)
            if (length(x.pos)>0)
                envir <- as.environment(x.pos[1])
            else
                envir <- globalenv()
        }
        if (exists(x, envir=envir)) {
            va <- get(va.name, envir=envir, inherits=FALSE)
            if (!inherits(va, 'varray'))
                stop('object ', va.name, ' is not a varray')
            if (!change.mode.ok) {
                if (mode(newdata) != mode.varray(va) && all(is.na(newdata)))
                    storage.mode(newdata) <- vstorage.mode(va)
                if (mode(newdata) != mode.varray(va))
                    stop('not changing mode: mode(existing.varray)=', mode.varray(va), ' and mode(newdata)=', mode(newdata))
            }
        } else {
            va <- NULL
            newObj <- TRUE
        }
    } else {
        if (!inherits(va, 'varray'))
            stop('va is not a varray object')
        va <- x
    }
    # env.name is the name of the environment where components are stored
    # envir is the environment where the varray is stored (only needed
    # if va.name is non-null)
    if (is.null(env.name) && !is.null(va) && !is.null(va$env.name)) {
        env.name <- va$env.name
        comp.envir <- as.environment(va$env.name)
    } else {
        comp.envir <- globalenv()
    }

    if (is.null(dim(newdata)))
        stop('newdata must be a matrix-like object')
    # and 'adn' and 'ad'; the dimnames & dims of 'va'
    if (!is.null(va)) {
        adn <- dimnames(va)
        ad <- dim(va)
        for (i in seq(along=va$info)[-1]) {
            if (length(va$info[[i]]$dim) != length(va$info[[1]]$dim))
                stop('chunk ', i, ' has different dimensionality: ',
                     length(va$info[[i]]$dim), ' vs ', length(va$info[[1]]$dim), ' in first')
        }
    } else {
        adn <- rep(list(character(0)), length(dim(newdata)))
        ad <- rep(0, length(dim(newdata)))
    }
    if (along < 1 || along > length(non.null(va$info[[1]]$dim, dim(newdata))))
        stop('along must be in 1..', length(non.null(va$info[[1]]$dim, dim(newdata))))

    if (is.null(comp.name))
        if (!is.null(va.name))
            comp.name <- paste('.', va.name, dateblock, sep='.')
        else
            stop('must supply comp.name or va.name with new object')

    # 'ddn' and 'dd' are dimnames and dims of data (the new data)
    ddn <- dimnames(newdata)
    dd <- dim(newdata)
    if (!is.null(va) && length(va$info[[1]]$dim) != length(dd))
        stop('component 1 has different dimensionality than data: ',
                     length(va$info[[1]]$dim), ' vs ', length(dd))

    if (is.null(along))
        along <- 1
    if (is.null(dimorder))
        dimorder <- seq(length=length(dd))
    if (!identical(sort(as.numeric(dimorder)), as.numeric(seq(length(dd)))))
        stop('dimorder must be some permutation of 1:length(dim)')
    if (is.null(naidxok))
        naidxok <- NA
    if (is.null(format))
        format <- '%Y-%m-%d'
    rdimorder <- order(dimorder)

    # find existing sub-components
    ex.ai <- match(ddn[[along]], adn[[along]])
    # existing sub-component names
    ex.scn <- as.character(sapply(va$info, '[[', 'name'))
    # If we need any new slices, create and/or expand existing subcomponents
    new.slices <- which(is.na(ex.ai))
    expand.comp.i <- integer(0)
    if (length(new.slices) || is.null(va)) {
        # new.slices is integer: the slices in 'data' that need new subcomponents
        new.slices.scn <- format(strptime(ddn[[along]][new.slices], format), format=comp.name)
        if (any(is.na(new.slices.scn)))
            stop('generated NA component name for some dimnames: ', paste(ddn[[along]][new.slices][is.na(new.slices.scn)], collapse=', '))
        all.scn.u <- unique(c(ex.scn, new.slices.scn))
        new.scn.u <- setdiff(unique(new.slices.scn), ex.scn)
        expand.comp.i <- match(intersect(unique(new.slices.scn), ex.scn), ex.scn)
        comp.dn.changed <- rep(FALSE, length(all.scn.u))
        sample <- asub(newdata, rep(list(1), length(dd)))
        if (is.null(va)) {
            if (is.null(keep.ordered)) keep.ordered <- TRUE
            va <- structure(list(dim=NULL, dimnames=NULL, along=along,
                                 info=rep(list(list(name=NULL, dim=NULL, dimnames=NULL, env.name=NULL,
                                          sample=sample, naidxok=NULL, map=NULL)), length(all.scn.u)),
                                 along.idx=NULL, dimorder=dimorder, naidxok=naidxok, env.name=env.name,
                                 comp.name=comp.name, format=format,
                                 keep.ordered=keep.ordered, umode=storage.mode(sample)),
                            class='varray')
        } else {
            va$info <- c(va$info, rep(list(list(name=NULL, dim=NULL, dimnames=NULL, env.name=NULL,
                                                sample=sample, naidxok=NULL, map=NULL)), length(new.scn.u)))
        }
        # if any new sub-components are needed, create them
        for (this.new.scn in new.scn.u) {
            this.data <- asub(newdata, new.slices[new.slices.scn==this.new.scn], dims=along, drop=FALSE)
            this.data.dn <- dimnames(this.data)
            # find dimnames that don't have all NA values
            for (i in seq(length(dim(newdata))[-along]))
                this.data.dn[[i]] <- this.data.dn[[i]][apply(this.data, i, function(x) !all(is.na(x)))]
            # do we need to subset this.data down to non-NA data?
            if (!isTRUE(all.equal(dimnames(this.data), this.data.dn)))
                this.data <- asub(this.data, this.data.dn, dims=seq(length=length(dim(this.data))), drop=FALSE)
            # do we need to reverse-permute the data?
            if (all(dimorder==seq(len=length(dim)))) {
                this.datar <- this.data
            } else {
                this.datar <- aperm(this.data, rdimorder)
            }
            comp.ver <- attr(this.datar, 'comp.ver')
            comp.ver <- if (is.null(comp.ver)) 1 else comp.ver + 1
            attr(this.datar, 'comp.ver') <- comp.ver
            j <- match(this.new.scn, all.scn.u)
            va$info[[j]]$name <- this.new.scn
            va$info[[j]]$dim <- dim(this.datar)
            va$info[[j]]$dimnames <- dimnames(this.datar)
            va$info[[j]]$env.name <- env.name
            va$info[[j]]$naidxok <- naidxok
            va$info[[j]]$comp.ver <- comp.ver
            # will fix 'map' at the end
            assign(this.new.scn, envir=comp.envir, value=this.datar)
        }
        # if we need to add slices to any existing sub-components, we do that below
    } else {
        comp.dn.changed <- rep(FALSE, length(va$info))
        new.slices.scn <- character(0)
    }
    if (is.null(keep.ordered)) keep.ordered <- TRUE
    keep.ordered <- rep(keep.ordered, length.out=length(dd))
    keep.orderedr <- keep.ordered[rdimorder]
    # data.ii is the slices of 'data' that are to be found in existing components of 'va'
    data.ii <- which(!is.na(ex.ai))
    if (identical(va$env.name, FALSE))
        va$env.name <- NULL
    if (length(data.ii) || length(expand.comp.i)) {
        # work out which existing components of 'va' we need to work with
        exist.comp.i <- va$along.idx[ex.ai[data.ii]]
        for (this.i in sort(unique(c(exist.comp.i, expand.comp.i)))) {
            # working with component 'this.i' of 'va', and data slices 'data.i'
            data.i <- data.ii[this.i == exist.comp.i]
            # add in the indices that are new in the 'along' dimension but belong to this component
            data.i <- union(data.i, which(is.na(ex.ai))[new.slices.scn == va$info[[this.i]]$name])
            this.data <- asub(newdata, data.i, dims=along, drop=FALSE)
            this.data.dn <- dimnames(this.data)
            # find dimnames that don't have all NA values
            for (i in seq(length(dim(newdata))[-along]))
                this.data.dn[[i]] <- this.data.dn[[i]][apply(this.data, i, function(x) !all(is.na(x)))]
            # do we need to subset this.data down to non-NA data?
            if (!isTRUE(all.equal(dimnames(this.data), this.data.dn)))
                this.data <- asub(this.data, this.data.dn, dims=seq(length=length(dim(this.data))), drop=FALSE)
            # do we need to reverse-permute the data?
            if (all(dimorder==seq(len=length(dim)))) {
                this.datar <- this.data
                this.datar.dn <- this.data.dn
            } else {
                this.datar <- aperm(this.data, rdimorder)
                this.datar.dn <- this.data.dn[rdimorder]
            }
            need.expand <- mapply(va$info[[this.i]]$dimnames, this.datar.dn, FUN=function(old, new) !all(is.element(new, old)))
            env <- as.environment(non.null(va$info[[this.i]]$env.name, non.null(va$env.name, 1)))
            comp.data <- get(va$info[[this.i]]$name, envir=env, inherits=FALSE)
            if (any(need.expand)) {
                comp.dn.changed[this.i] <- TRUE
                new.dn <- lapply(seq(len=length(va$info[[this.i]]$dim)), function(i) {
                    dni <- union(va$info[[this.i]]$dimnames[[i]], this.datar.dn[[i]])
                    if (keep.orderedr[i])
                        dni <- sort(dni, na.last=TRUE)
                    return(dni)
                })
                comp.data <- conform(comp.data, new.dn)
                va$info[[this.i]]$dimnames <- new.dn
                va$info[[this.i]]$dim <- sapply(new.dn, length)
            }
            # load values into the component
            if (length(clear.dim)) {
                if (is.element(1, clear.dim)) {
                    ii <- intersect(rownames(comp.data), rownames(this.datar))
                    if (length(ii))
                        comp.data[ii,] <- fill
                }
                if (is.element(2, clear.dim)) {
                    ii <- intersect(rownames(comp.data), rownames(this.datar))
                    if (length(ii))
                        comp.data[ii,] <- fill
                }
            }
            afill(comp.data) <- this.datar
            # maintain the version of the component
            comp.ver <- attr(comp.data, 'comp.ver')
            comp.ver <- if (is.null(comp.ver)) 1 else comp.ver + 1
            attr(comp.data, 'comp.ver') <- comp.ver
            va$info[[this.i]]$comp.ver <- comp.ver
            # and save the component
            assign(va$info[[this.i]]$name, envir=env, value=comp.data, inherits=FALSE)
        }
    }
    alongd <- rdimorder[along]
    if (keep.ordered[along]) {
        # reorder the component objects based on the first element of their 'along' dimname
        el1 <- sapply(va$info, function(info) info$dimnames[[alongd]][1])
        el1ord <- order(el1, na.last=TRUE)
        if (!all(diff(el1ord)==1))
            va$info <- va$info[el1ord]
    }
    # Construct the dimnames of the overall object.
    # Do this by combining old and new, rather than piecing
    # together from the components, because doing it the
    # latter way loses the order of construction that we
    # want to retain when keep.ordered=FALSE.
    # dn <- lapply(seq(len=length(va$info[[1]]$dim)), function(i) unique(unlist(lapply(va$info, function(x) x$dimnames[[i]]))))
    # convert d,dn to user dimorder
    # if (!all(dimorder == seq(length(d)))) {d <- d[dimorder]; dn <- dn[dimorder]}
    # We are constructing d and dn in user space here, so don't need to apply dimorder perm
    dn <- mapply(union, adn, ddn, SIMPLIFY=FALSE)
    if (any(sapply(need.dimnames, length) > 0)) {
        if (!is.list(need.dimnames))
            stop('need.dimnames must be list')
        if (length(need.dimnames) != length(dn))
            stop('length(need.dimnames) != length(dimnames)')
        dn <- mapply(union, dn, need.dimnames, simplify=FALSE)
    }
    d <- sapply(dn, length)
    if (is.null(keep.ordered) || any(keep.ordered))
        dn[keep.ordered] <- lapply(dn[keep.ordered], sort, na.last=TRUE)
    # fix 'map' in all info components
    # eventually, record which components were changed, and only update those
    va$along.idx <- integer(d[along])
    for (i in seq(to=1, from=length(va$info))) {
        va$along.idx[match(va$info[[i]]$dimnames[[alongd]], dn[[along]])] <- i
        va$info[[i]]$map <- lapply(seq(along=dn), function(j) match(dn[[j]], va$info[[i]]$dimnames[[rdimorder[j]]]))[rdimorder]
    }
    va$dim <- d
    va$dimnames <- dn
    if (!is.null(fill) && !is.na(fill))
        va$fill <- fill
    if (!is.null(va.name)) {
        va$name <- va.name
        assign(va.name, value=va, envir=envir)
        return(invisible(va))
    } else {
        return(va)
    }
}
