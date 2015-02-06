#' Construct a virtual array
#'
#' Construct a virtual array by binding together supplied
#' arrays or matrixes.  Stores only dimension information
#' and named references to the sub-arrays; does not store
#' their actual data.
#'
#' @param \dots
#' Either a single  argument being a character vector naming the component sub-arrays, or
#' multiple arguments, each of which is a single character item or
#' unquoted name of an object.
#' Note that extra arguments are ignored for \code{as.array} (they are
#' only present because the generic has them).
#'
#' @param along
#' The dimension along which to bind (from the user's point of view).
#'
#' @param dimorder
#' The order in which dimensions are stored in the sub-arrays.  The
#' reverse of this permutation is applied to extract data from the sub-arrays.
#'
#' @param env.name
#' \code{TRUE} or \code{FALSE}, specifiying whether to record the name of
#' the environment in which the object is found.  If \code{FALSE},
#' objects are searched for in the global environment.
#'
#' @param envir
#' Where to find the sub-arrays (can be an environment name, or an
#' environment, as long as the environment can be recovered by \code{as.environment(environmentName(envir))}).
#'
#' @param naidxok
#' Logical value indicating whether the component objects can handle
#' \code{NA}'s in indices.  Notably, \code{ff} objects cannot.  The
#' default value is \code{FALSE} for objects that inherit from class
#' \code{ff} and \code{TRUE} for other objects.
#'
#' @param dimnames
#' Optional dimnames to use instead of the union of dimnames of the
#' components.
#' Must be a list of character vectors.  Null components are ignored.
#' E.g., supplying \code{dimnames=list(NULL, c('a','b','c'))} would tell
#' varray to use the usual rownames (i.e., the union of the rownames of
#' the components) but to use \code{c('a','b','c')} as column names.  If
#' some components have other column names, those other columns will be
#' inaccessible through the varray object.
#'
#' @param comp.name
#' A character string that records how to construct the name of a
#' component.  Used in \code{update.ts.varray()}.
#'
#' @param keep.ordered
#' Should dimnames be kept ordered when new elements are added?
#'
#' @param umode
#' The storage mode of the component objects.
#'
#' @param fill
#' The value that is returned instead of implicit NAs (array elements that do not
#' exist in the explicitly stored data).
#'
#' @param x a varray object
#'
#' @details
#' Component arrays are stored by reference (by name.)  At the time of subset
#' extraction the component arrays will be retrieved.  This creates the
#' possibility for retrieving an unintended object of the same name.
#' To minimize this possibility, the environments searched to
#' find the component arrays are limited to the following:
#' \itemize{
#'   \item If \code{env.name} is \code{FALSE}; the global environment and
#'   the rest of the search path
#'   \item If \code{env.name} is \code{TRUE}; the supplied environment
#'   \code{envir}, or, if that was \code{NULL}, the environment in which
#'   the component was found at the time the \code{varray} was constructed.
#' }
#' Note that this makes it currently impossible for a \code{varray} to
#' refer to objects that reside in an unnamed environment or one not on
#' the search list of environments.
#'
#' All the data associated with a single element of the binding dimension
#' (the \code{along} dimension) must be stored in a single one of the
#' component objects.  If different components given to \code{varray}
#' repeat an item of the along dimension, \code{varray} stops with an error.
#'
#' \code{rebuild.varray()} rebuilds a varray object and can be used when
#' the underlying objects have changed.
#'
#' @return   \code{varray()} returns an S3 object of class \code{varray}.
#'  \code{as.array()} returns a standard R array (which is a matrix when
#'  there are just two dimensions).
#'
#' @seealso   \code{\link{[.varray}}
#'  \code{\link{dim.varray}}
#'  \code{\link{dimnames.varray}}
#'  \code{\link{mode.varray}}
#'  \code{\link{storage.mode.varray}}
#'
#' @examples
#' a <- array(1:6, dim=c(2,3), dimnames=list(letters[1:2],letters[23:25]))
#' b <- array(7:15, dim=c(3,3), dimnames=list(letters[3:5],letters[24:26]))
#' x <- varray(a,b)
#' x1 <- varray('a','b') # equivalent
#' x2 <- varray(c('a','b')) # equivalent
#' as.array(x)
#' x
#' x[c('a'),c('x','z'),drop=FALSE]
#' x[c('d','b','c'),c('y','z'),drop=FALSE]
#' b <- b[-2,]
#' x <- rebuild.varray(x)
#'
#' # fill arg on as.matrix does not replace explicit stored NA's
#' y <- varray(cbind(A=c(a=1)), cbind(B=c(b=NA)))
#' as.matrix(y, fill=0)

varray <- function(..., along=1, dimorder=NULL, env.name=FALSE, envir=NULL, naidxok=NA, dimnames=NULL, comp.name=NULL, keep.ordered=TRUE, umode=NULL, fill=NULL) {
    # Can call like this:
    #    varray(a, b, c)
    # or varray('a', 'b', 'c')
    # or varray(c('a', 'b', 'c'))
    # Still need to figure out if need to record where to find the objects (i.e., which environments)
    # env.name is tricky, because env can be named different things in different sessions
    # might be better to use a db.name, which is like a marker stored in a particular variable in a db.
    # This could identify an attached DB regardless of its file location or attached environment name.
    # We might want to have track.attach() warn if we attach two DBs with the same name.
    dotv <- list(...)
    if (is.character(envir))
        envir <- as.environment(envir)
    if (length(dotv)==1 && is.character(dotv[[1]])) {
        # single argument that is a character vector of names of objects
        info <- lapply(dotv[[1]], FUN=function(arg) {
            name <- arg
            if (!is.null(envir)) {
                x <- get(name, envir=envir)
                x.env.name <- fixGlobalEnvName(environmentName(envir))
            } else {
                x.env.name <- fixGlobalEnvName(find(name))
                x <- get(name)
            }
            if (!env.name)
                x.env.name <- NULL
            if (length(dim(x))==0)
                stop("object '", name, "' is not an array (has no dim)")
            if (is.null(dimnames(x)))
                stop('argument ', name, ' does not refer to an array')
            sample <- asub(x, rep(list(1), length(dim(x))))
            list(name=name, dim=dim(x), dimnames=dimnames(x),
                 env.name=x.env.name, sample=sample,
                 naidxok=if (is.na(naidxok)) inherits(x, 'ff') else naidxok)
        })
    } else {
        # multiple arguments, could be names or objects
        dotargs <- match.call(expand.dots=FALSE)$...
        info <- mapply(list(...), dotargs, SIMPLIFY=FALSE, FUN=function(x, arg) {
            if (is.name(arg)) {
                have.value <- FALSE
                name <- as.character(arg)
                x.env.name <- fixGlobalEnvName(find(name))
            } else if (is.character(arg) && length(arg)==1) {
                have.value <- FALSE
                name <- arg
                if (!is.null(envir)) {
                    x <- get(name, envir=envir)
                    x.env.name <- fixGlobalEnvName(environmentName(envir))
                } else {
                    x.env.name <- fixGlobalEnvName(find(name))
                    x <- get(name)
                }
            } else {
                # store a value inside the varray
                have.value <- TRUE
                name <- deparse(arg, nlines=1)
            }
            if (!env.name)
                x.env.name <- NULL
            if (length(dim(x))==0)
                stop("object '", name, "' is not an array (has no dim)")
            if (is.null(dimnames(x)))
                stop('argument ', name, ' does not refer to an array')
            sample <- asub(x, rep(list(1), length(dim(x))))
            if (have.value)
                list(value=x, dim=dim(x), dimnames=dimnames(x),
                     env.name=x.env.name, sample=sample,
                     naidxok=if (is.na(naidxok)) inherits(x, 'ff') else naidxok)
            else
                list(name=name, dim=dim(x), dimnames=dimnames(x),
                     env.name=x.env.name, sample=sample,
                     naidxok=if (is.na(naidxok)) inherits(x, 'ff') else naidxok)
        })
    }
    if (along < 1 || along > length(info[[1]]$dim))
        stop('along must be in 1..', length(info[[1]]$dim))
    for (i in seq(along=info)[-1]) {
        if (length(info[[i]]$dim) != length(info[[1]]$dim))
            stop('chunk ', i, ' has different dimensionality: ',
                 length(info[[i]]$dim), ' vs ', length(info[[1]]$dim))
    }
    dn <- lapply(seq(len=length(info[[1]]$dim)), function(i)
                 unique(unlist(lapply(info, function(x) x$dimnames[[i]]))))
    if (!is.null(dimnames))
        for (i in seq(along=dn))
            if (!is.null(dimnames[[i]]))
                dn[[i]] <- dimnames[[i]]
    d <- sapply(dn, length)
    along.idx <- integer(d[along])
    naidxok <- all(sapply(info, '[[', 'naidxok'))
    if (is.null(dimorder))
        dimorder <- seq(length(d))
    else
        if (!identical(sort(as.numeric(dimorder)), as.numeric(seq(length(d)))))
            stop('dimorder must be 1:length(d) in some permutation')
    # convert d,dn to user dimorder
    if (!all(dimorder == seq(length(d)))) {
        d <- d[dimorder]
        dn <- dn[dimorder]
    }
    rdimorder <- order(dimorder)
    alongd <- rdimorder[along]
    for (i in seq(to=1, from=length(info))) {
        along.idx[match(info[[i]]$dimnames[[alongd]], dn[[along]])] <- i
        info[[i]]$map <- lapply(seq(along=dn), function(j) match(dn[[j]], info[[i]]$dimnames[[rdimorder[j]]]))[rdimorder]
    }
    # check whether we have repeated dim elements on the 'along' dimension
    along.idx.all <- unlist(lapply(info, function(i) i$dimnames[[alongd]]))
    if (any(duplicated(along.idx.all)))
        stop('have some repeated dimension elements on along dimension (', along, '): ',
             some.examples(along.idx.all[duplicated(along.idx.all)], collapse=', '))
    # Components of a varray object (everything at the top level is stored in terms of user dimensions)
    # dim:       the combined dim
    # dimnames:  the combined dimnames
    # along:     the binding dimension (in terms of user dimensions)
    # info:      a list of lists, one component per subcomponent, containing:
    #       NOTE: everything inside info is stored in terms of the native dimorder, not the user dimorder
    #       name:     name of object
    #       dim:      cached dim of object (in terms of the object, before applying dimorder)
    #       dimnames: cached dimnames of the object (in terms of the object, before applying dimorder)
    #       env.name: name of the environment to find the object in (NULL if not used)
    #       sample:   a sample element of the type returned by [.varray
    #       naidxok:  logical, indicating if NA indices are ok for [ (e.g., FALSE for matrix, TRUE for vector and data.frame)
    #       map:      a list of integer vectors showing how top-level dimnames map to this subcomponent.
    #                 has NA entries where the top-level dimnames are not present in this subcomponent.
    # along.idx:    a vector containing the subindex for each element of the binding dimension
    # dimorder:     how to permute the underlying dimensions to arrive at the user-visible ones
    # naidxok:      default for info[[i]]$naidxok
    # env.name:     where to store new components unless otherwise specified (can be NULL)
    # comp.name:    a format specification for how to derive the component name from the dimnames (e.g., dates)
    # keep.ordered: logical; if TRUE, always keep the user-visible dimension ordered
    # umode:        native storage mode of the returned object (e.g., 'numeric', 'integer', 'raw', 'character')
    # fill:         optional; a value to replace missing or NA values with
    # creator.args: args to supply to creator, e.g., for 'ff' list(vmode='single')
    # expander.args:
    res <- structure(list(dim=d, dimnames=dn, along=along, info=info, along.idx=along.idx,
                   dimorder=dimorder, naidxok=naidxok, env.name=env.name, comp.name=comp.name,
                   keep.ordered=keep.ordered, umode=umode),
              class='varray')
    if (!is.null(fill) && !is.na(fill))
        res$fill <- fill
    return(res)
}

#' @rdname varray
rebuild.varray <- function(x) {
    # Rebuild a varray object to update for changes in underlying objects
    info <- lapply(x$info, FUN=function(comp) {
        if (!is.null(comp$value))
            z <- comp$value
        else if (is.null(comp$env.name))
            z <- get(comp$name, pos=1)
        else
            z <- get(comp$name, envir=as.environment(comp$env.name))
        if (length(dim(z))==0)
            stop("object '", comp$name, "' is not an array (has no dim)")
        if (is.null(dimnames(z)))
            stop('argument ', comp$name, ' does not refer to an array')
        sample <- asub(z, rep(list(1), length(dim(z))))
        comp$dim <- dim(z)
        comp$dimnames <- dimnames(z)
        comp$sample <- comp$sample
        return(comp)
    })
    for (i in seq(along=info)[-1]) {
        if (length(info[[i]]$dim) != length(info[[1]]$dim))
            stop('chunk ', i, ' has different dimensionality: ',
                 length(info[[i]]$dim), ' vs ', length(info[[1]]$dim))
    }
    dn <- lapply(seq(len=length(info[[1]]$dim)), function(i)
                 unique(unlist(lapply(info, function(x) x$dimnames[[i]]))))
    along <- x$along
    dimorder <- x$dimorder
    d <- sapply(dn, length)
    along.idx <- integer(d[along])
    naidxok <- all(sapply(info, '[[', 'naidxok'))
    # convert d,dn to user dimorder
    if (!all(dimorder == seq(length(d)))) {
        d <- d[dimorder]
        dn <- dn[dimorder]
    }
    rdimorder <- order(dimorder)
    alongd <- rdimorder[along]
    for (i in seq(to=1, from=length(info))) {
        along.idx[match(info[[i]]$dimnames[[alongd]], dn[[along]])] <- i
        info[[i]]$map <- lapply(seq(along=dn), function(j) match(dn[[j]], info[[i]]$dimnames[[rdimorder[j]]]))[rdimorder]
    }
    res <- structure(list(dim=d, dimnames=dn, along=along, info=info, along.idx=along.idx,
                   dimorder=dimorder, naidxok=naidxok, env.name=x$env.name, comp.name=x$comp.name,
                   keep.ordered=x$keep.ordered, umode=x$umode),
              class='varray')
    if (!is.null(x$fill) && !is.na(x$fill))
        res$fill <- x$fill
    return(res)
}

fixGlobalEnvName <- function(name) {
    if (name=='R_GlobalEnv') '.GlobalEnv'
    else if (name=='') stop('cannot use an unnamed environment')
    else name
}


#' Common matrix and array methods for varray objects
#'
#' @param x a varray object
#' @param i,j,\dots indexing arguments, treated as for ordinary array indexing
#' @param drop should dimensions with extent 1 in the result be dropped?
#' @param fill fill value to use in subsetting
#' @param value new values to use
#' @param deep should individual components be inspected to determine the result?
#' @name varray.methods
varray.methods <- function() NULL

#' @describeIn varray.methods Convert a varray to an ordinary array
#' @method as.array varray
as.array.varray <- function(x, ..., fill=x$fill) {
    if (is.null(fill)) fill <- NA
    rdimorder <- order(x$dimorder)
    alongd <- rdimorder[x$along]
    y <- abind(along=alongd, lapply(x$info,
               function(comp) {
                   # fix this code to use x$env
                   if (!is.null(comp$value))
                       z <- comp$value
                   else if (is.null(comp$env.name))
                       z <- get(comp$name, pos=1)
                   else
                       z <- get(comp$name, envir=as.environment(comp$env.name))
                   # don't replace explicit NA's with fill
                   # if (!is.na(fill) && any(i <- is.na(z))) z[i] <- fill
                   conform(z, x$dimnames[rdimorder], along=seq(len=length(x$dim))[-alongd], fill=fill)
               }))
    if (!all(x$dimorder == seq(length(x$dim))))
        y <- aperm(y, order(x$dimorder))
    if (!isTRUE(all.equal(x$dimnames[[x$along]], dimnames(y)[[x$along]])))
        y <- conform(y, x$dimnames, along=x$along, fill=fill)
    y
}

#' @describeIn varray.methods Convert a varray to an ordinary matrix
#' @method as.matrix varray
as.matrix.varray <- function(x, ..., fill=x$fill) {
    if (is.null(fill)) fill <- NA
    if (length(x$dim)==2)
        as.array.varray(x, ..., fill=fill)
    else
        as.matrix(as.array.varray(x, ..., fill=fill))
}

non.null <- function(x, y) if (!is.null(x)) x else y

# as.array does not work on data.frame objects, so make sure to call as.matrix on 2-d objects

#' Convert to ordinary matrix or array
#'
#' Shorthand for as.matrix or as.array
#'
#' @details
#'   If \code{x} is an atomic object and is not a virtual array, returns
#'  \code{x}, otherwise returns \code{as.matrix(x)} if \code{x} has two
#'  dimensions or \code{as.array(x)} otherwise.
#' @param x the object to convert
#' @return An atomic array version of \code{x}
#' @seealso   \code{link{as.array}} \code{link{as.matrix}}
#' @examples
#' x <- varray(cbind(A=c(a=1)), cbind(B=c(b=2)))
#' x
#' M(x)
#' as.array(x, fill=0)
#' as.matrix(x)
#' M(x) + 10
M <- function(x) {
    if (is.atomic(x) && !is.virtual.array)
        return(x)
    else if (non.null(length(dim(x)), 1)>2)
        as.array(x)
    else
        as.matrix(x)
}

#' @describeIn varray.methods Print info about a varray object (but not the whole object)
#' @method print varray
print.varray <- function(x, ...) {
    dot3 <- function(n) if (n<=4) seq(len=n) else c(1,2,NA,n)
    dnsum <- function(i, d, dn) {
        # summarize dimnames
        if (is.na(i)) {
            return("...")
        } else if (is.null(dn[[i]])) {
            s <- "NULL"
        } else {
            j <- dn[[i]][dot3(d[i])]
            if (any(is.na(j)))
                j[is.na(j)] <- '...'
            s <- paste(j, collapse=', ')
        }
        return(paste('[', i, '] ', s, sep=''))
    }
    cat(paste(x$dim, collapse=' x '), 'virtual', if (length(x$dim)==2) 'matrix' else 'array')
    if (is.null(x$dimnames)) {
        cat(' with NULL dimnames\n')
    } else {
        cat(' with dimnames:\n')
        cat(paste('  ', sapply(dot3(length(x$dim)), dnsum, d=x$dim, dn=x$dimnames), '\n'), sep='')
    }
    if (!all(x$dimorder == seq(length(x$dim))))
        cat('using dims in order', paste(x$dimorder, collapse=', '), '\n')
    cat('with', length(x$info), 'sub-arrays:\n')
    for (k in dot3(length(x$info))) {
        if (is.na(k)) {
            cat('...\n')
        } else {
            cat('sub-array ', k, ': ', sep='')
            if (!is.null(x$info[[k]]$name)) {
                cat("'", x$info[[k]]$name, "' ", sep='')
                if (is.null(x$info[[k]]$env.name)) {
                    found.env <- find(x$info[[k]]$name)
                    if (length(found.env)==0)
                        found.env <- '(not found in any env) '
                    else
                        found.env <- paste("(found in env '", found.env, "') ", sep='')
                } else {
                    env.actual <- try(as.environment(x$info[[k]]$env.name))
                    if (is(env.actual, 'try-error'))
                        found.env <- paste("(no env named '", x$info[[k]]$env.name, "') ", sep='')
                    else if (exists(x$info[[k]]$name, envir=env.actual, inherits=FALSE))
                        found.env <- paste("(in env '", x$info[[k]]$env.name, "') ", sep='')
                    else
                        found.env <- paste("(not found in env '", x$info[[k]]$env.name, "') ", sep='')
                }
                cat(found.env)
            } else {
                cat("(internal value) ")
            }
            cat(paste(x$info[[k]]$dim, collapse=' x '),
                if (length(x$info[[k]]$dim)==2) 'matrix' else 'array')
            if (is.null(x$info[[k]]$dimnames)) {
                cat(' with NULL dimnames\n')
            } else {
                cat(' with dimnames:\n')
                cat(paste('  ', sapply(dot3(length(x$info[[k]]$dim)),
                                       dnsum, d=x$info[[k]]$dim,
                                       dn=x$info[[k]]$dimnames), '\n'), sep='')
            }
        }
    }
}

# length.varray <- function(x) prod(x$dim) # messes up str() if we define length, and length(x) != prod(dim(x)) for data.frame

#' @describeIn varray.methods Return dimension size of a varray object
#' @method dim varray
dim.varray <- function(x) x$dim

#' @describeIn varray.methods Return dimension names of a varray object
#' @method dimnames varray
dimnames.varray <- function(x) x$dimnames

#' @describeIn varray.methods Return 'mode' of data in a varray object
#' @method mode varray
mode.varray <- function(x) mode(sapply(x$info, '[[', 'sample'))

#' @describeIn varray.methods Return 'storage.mode' of data in a varray object
#' @method storage.mode varray
storage.mode.varray <- function(x) storage.mode(sapply(x$info, '[[', 'sample'))

#' @describeIn varray.methods Return the first part of a varray.
#' @method head varray
#' @param n the number of rows to return
head.varray <- function (x, n = 6L, ...)
{
    stopifnot(length(n) == 1L)
    n <- if (n < 0L)
        max(nrow(x) + n, 0L)
    else min(n, nrow(x))
    asub(x, list(seq_len(n)), 1, drop = FALSE)
}

#' @describeIn varray.methods Return the last part of a varray.
#' @method tail varray
#' @param addrownums should row numbers be added if there are none?
tail.varray <- function (x, n = 6L, addrownums = TRUE, ...)
{
    stopifnot(length(n) == 1L)
    nrx <- nrow(x)
    n <- if (n < 0L)
        max(nrx + n, 0L)
    else min(n, nrx)
    sel <- seq.int(to = nrx, length.out = n)
    ans <- asub(x, list(sel), 1, drop = FALSE)
    if (addrownums && is.null(rownames(x)))
        rownames(ans) <- paste0("[", sel, ",]")
    ans
}

#' @describeIn varray.methods Returns a subset of a varray objects
#' @method [ varray
"[.varray" <- function(x, ..., drop=TRUE) {
    Nidxs <- nargs() - 1 - (!missing(drop))
    d <- x$dim
    dn <- x$dimnames
    naidxok <- x$naidxok
    if (is.null(naidxok) || is.na(naidxok))
        naidxok <- FALSE
    fill <- if (is.null(x$fill)) NA else x$fill
    so <- x$dimorder
    if (is.null(so))
        so <- x$storage.order
    # permutation on the stored data to get back to user view
    ap <- order(so)
    alongd <- ap[x$along]
    # adim and adimnames are the dim and dimnames in the storage order
    ad <- d[so]
    adn <- dn[so]
    integer.max <- .Machine$integer.max
    if (Nidxs==length(d)) {
        # regular indexing
        dotargs <- match.call(expand.dots=FALSE)$...
        # In R, list(...) will stop with an error with empty indices, e.g., x[1,]
        idx.missing <- sapply(dotargs, function(a) is.name(a) && as.character(a)=="")
        ai <- vector("list", length(idx.missing))
        ai[!idx.missing] <- lapply(dotargs[!idx.missing], eval, sys.parent())
        if (length(ai)!=length(d))
            stop("strange ... thought I had ", length(d), " index args, but don't???")
        # put the indices in storage order and convert to integers
        ai <- ai[so]
        idx.missing <- idx.missing[so]
        for (j in seq(along=ai)) {
            if (idx.missing[j]) {
                ai[[j]] <- seq(len=ad[j])
            } else if (is.null(ai[[j]])) {
                # NULL is like numeric(0) in an index
                ai[[j]] <- numeric(0)
            } else if (is.numeric(ai[[j]])) {
                if (storage.mode(ai[[j]])!="integer")
                    k <- as.integer(ai[[j]])
                else
                    k <- ai[[j]]
                k <- k[k!=0]
                if (any(is.true(abs(k) > ad[j])))
                    stop("index out of range on dimension ", ap[j], ", e.g.: ", k[abs(k) > ad[j]][1])
                if (any(is.true(k < 0))) {
                    if (any(is.true(k > 0)))
                        stop("cannot mix negative and positive indices (on dimension ", ap[j], ")")
                    k <- seq(len=ad[j])[k]
                }
                ai[[j]] <- k
            } else if (is.logical(ai[[j]])) {
                if (length(ai[[j]])) {
                    if (ad[j] %% length(ai[[j]]) !=0)
                        stop("dim[j] not a multiple of length(i) for logical index i (on dimension ", ap[j], ")")
                    # need to be careful to preserve NA's in ..1
                    # and create a vector not longer than necessary
                    ai[[j]] <- as.vector(outer(seq(along=ai[[j]])[ai[[j]]],
                                               seq(0,len=ad[j]/length(ai[[j]]), by=length(ai[[j]])), "+"))
                } else {
                    ai[[j]] <- integer(0)
                }
            } else if (is.character(ai[[j]])) {
                if (is.null(adn))
                    stop("cannot use character indexing -- object has no dimension names")
                i <- match(ai[[j]], adn[[j]])
                if (any(is.na(i) & !is.na(ai[[j]])))
                    stop("character indices not in dimnames[[", ap[j], "]], e.g.: ", ai[[j]][is.na(i)][1])
                ai[[j]] <- i
            } else {
                stop("cannot use an index of class ", class(ai[[j]]))
            }
        }
        # Now we have all numeric indices
        # Names of subidx are idx's of submatrix, and values are the indices we want
        subidx <- tapply(seq(along=ai[[alongd]]),
                         factor(x$along.idx[ai[[alongd]]], levels=seq(along=x$info)),
                         FUN=c, simplify=FALSE)
        a <- lapply(which(sapply(subidx, length)>0), function(i) {
            # i is the index of the submatrix we need some data from
            ii <- replace(ai, alongd, list(ai[[alongd]][subidx[[i]]]))
            # map to indices for this submatrix
            jj <- lapply(seq(len=length(x$dim)), function(j)
                return(x$info[[i]]$map[[j]][ii[[j]]]))
            has.nas <- FALSE
            if (!naidxok) {
                # jj1 is the indices without any NA's
                # jj2 is how to find the indices including NA's in jj1
                jj1 <- jj
                jj2 <- jj
                for (k in seq(along=jj)) {
                    if (any(jj.na <- is.na(jj[[k]]))) {
                        jj1[[k]] <- jj[[k]][!jj.na]
                        has.nas <- TRUE
                    }
                    jj2[[k]] <- match(jj[[k]], jj1[[k]])
                }
            }
            if (naidxok || !has.nas) {
                if (!is.null(x$info[[i]]$value)) {
                    y <- do.call('[', c(list(x$info[[i]]$value), jj, list(drop=FALSE)))
                } else {
                    if (is.null(x$info[[i]]$env.name))
                        yy <- get(x$info[[i]]$name, pos=1)
                    else
                        yy <- get(x$info[[i]]$name, envir=as.environment(x$info[[i]]$env.name))
                    y <- do.call('[', c(list(yy), jj, list(drop=FALSE)))
                }
            } else {
                if (!is.null(x$info[[i]]$value)) {
                    y1 <- do.call('[', c(list(x$info[[i]]$value), jj1, list(drop=FALSE)))
                } else {
                    if (is.null(x$info[[i]]$env.name))
                        yy <- get(x$info[[i]]$name, pos=1)
                    else
                        yy <- get(x$info[[i]]$name, envir=as.environment(x$info[[i]]$env.name))
                    y1 <- do.call('[', c(list(yy), jj1, list(drop=FALSE)))
                }
                # we assume that the object that is result of indexing can handle NA's in indices
                # (ordinary R arrays are fine with NA's in indices, it's ff arrays that are not)
                y <- do.call('[', c(list(y1), jj2, list(drop=FALSE)))
            }
            dimnames(y) <- NULL
            y
        })
        if (length(a) > 1) {
            a <- abind(along=alongd, a)
            # are subchunks out-of-order?
            # get numeric indices of the along-dim as they appear in a
            b <- unlist(lapply(which(sapply(subidx, length)>0), function(i)
                ai[[alongd]][subidx[[i]]]), use.names=FALSE)
            # if there are NA's in ai[[alongd]], it's length will not equal b's
            if (any(is.na(ai[[alongd]])) != (length(b)!=length(ai[[alongd]])))
                stop("internal indexing inconsistency: expecting NA's in ai[[alongd]] iff length(b)!=length(ai[[alongd]])")
            if (length(b)!=length(ai[[alongd]]) || !all(b==ai[[alongd]]))
                a <- asub(a, match(ai[[alongd]], b), alongd, drop=FALSE)
        } else if (length(a) == 1) {
            a <- a[[1]]
            if (any(is.na(ai[[alongd]]))) {
                i <- which(sapply(subidx, length)>0)
                if (length(i)!=1)
                    stop("internal indexing inconsistency: expecting only one non-null subidx")
                b <- ai[[alongd]][subidx[[i]]]
                a <- asub(a, match(ai[[alongd]], b), alongd, drop=FALSE)
            }
        } else {
            a <- array(numeric(0), dim=sapply(ai, length))
        }
        if (!all(x$dimorder == seq(length(x$dim))))
            a <- aperm(a, order(x$dimorder))
        if (!is.null(dn))
            dimnames(a) <- lapply(seq(len=length(d)), function(i) dn[[i]][ai[ap][[i]]])
        if (!identical(drop, FALSE)) {
            if (identical(drop, TRUE))
                drop <- which(sapply(ai, length)==1)
            if (length(drop))
                a <- adrop(a, drop=drop)
        }
    } else if (Nidxs==1 && !missing(..1)) {
        # matrix or vector indexing
        if (!is.matrix(..1) && (is.logical(..1) || is.numeric(..1))) {
            if (is.logical(..1)) {
                if (! (length(..1) %in% cumprod(c(1,x$dim))))
                    stop("length(x) is not the length of a sub-array for logical index i")
                # need to be careful to preserve NA's in ..1
                # and create a vector not longer than necessary
                vi <- as.vector(outer(seq(along=..1)[..1],
                                      seq(0,len=prod(x$dim)/length(..1), by=length(..1)), "+"))
            } else if (is.numeric(..1)) {
                # numeric
                vi <- ..1
                if (any(abs(vi) > x$info$length, na.rm=TRUE))
                    stop("vector indices out of range")
                if (any(vi < 0, na.rm=TRUE)) {
                    # using -ve vector indices results in the allocation of a
                    # vector indexing the entire array here!
                    if (any(is.na(vi) | vi>0))
                        stop("Only 0's may be mixed with -ve indices")
                    vi <- seq(x$info$length)[vi]
                } else if (any(j <- is.true(vi==0))) {
                    vi <- vi[!j]
                }
            }
            mi <- matrix(0L, nrow=length(vi), ncol=length(d))
            ii <- vi-1
            for (j in seq(along=d)) {
                mi[,j] <- as.integer((ii %% d[j]) + 1)
                if (j < length(d))
                    ii <- floor(ii / d[j])
            }
        } else if (is.matrix(..1)) {
            # matrix indexing
            if (ncol(..1)!=length(d))
                stop("a single matrix index argument must be a ", length(d), " column matrix")
            if (mode(..1)=="numeric")
                mi <- ..1[,so,drop=FALSE]
            else if (mode(..1)=="character")
                mi <- do.call('cbind',
                              lapply(seq(len=length(dn)),
                                     function(i) match(..1[,i], dn[[i]])))[,so,drop=FALSE]
            else
                stop("matrix indexing only works with numeric or character matrices")
            vi <- NULL
        } else if (is.list(..1)) {
            # like matrix indexing but with data.frame or list
            if (length(..1)!=length(d))
                stop("a list or data frame index argument must have ", length(d), " columns")
            k <- setdiff(sapply(..1, length), 1)
            if (length(k)>1)
                stop('list index different length components')
            if (length(k)==0) k <- length(..1[[1]])
            mi <- do.call('cbind',
                          lapply(seq(len=length(dn)),
                                 function(i) {
                                     j <- match(..1[[i]], dn[[i]])
                                     if (length(j)!=k)
                                         return(rep(j, length.out=k))
                                     else
                                         return(j)
                                     }))[,so,drop=FALSE]
            vi <- NULL
        } else {
            stop("a single argument must be a ", length(d), " column matrix or a vector")
        }
        # now we have 'mi' - a matrix index
        if (any(i <- (rowSums(mi==0, na.rm=TRUE) > 0)))
            mi <- mi[!i,,drop=FALSE]
        if (storage.mode(mi)!="integer")
            storage.mode(mi) <- "integer"
        if (is.true(any(mi < 1 | (mi - matrix(ad, nrow=nrow(mi), ncol=ncol(mi), byrow=TRUE)) > 0)))
            stop("matrix indices out of range")
        mi.na <- rowSums(is.na(mi))>0
        subidx <- tapply(seq(along=mi[,alongd])[!mi.na],
                         factor(x$along.idx[mi[!mi.na,alongd]], levels=seq(along=x$info)),
                         FUN=c, simplify=FALSE)
        a <- lapply(which(sapply(subidx, length)>0), function(i) {
            # i is the index of the submatrix we need some data from
            # jj is the matrix indices just for this submatrix
            jj <- mi[subidx[[i]],,drop=FALSE]
            # map to indices for this submatrix
            for (j in seq(len=length(x$dim)))
                jj[,j] <- x$info[[i]]$map[[j]][jj[,j]]
            if (!is.null(x$info[[i]]$value)) {
                y <- x$info[[i]]$value[jj]
            } else {
                if (is.null(x$info[[i]]$env.name))
                    yy <- get(x$info[[i]]$name, pos=1)
                else
                    yy <- get(x$info[[i]]$name, envir=as.environment(x$info[[i]]$env.name))
                y <- yy[jj]
            }
            dimnames(y) <- NULL
            y
        })
        if (length(a) > 1) {
            a <- unlist(a, use.names=FALSE)
            # get numeric indices of the along-dim as they appear in 'a'
            b <- unlist(lapply(which(sapply(subidx, length)>0),
                               function(i) mi[subidx[[i]],alongd,drop=FALSE]), use.names=FALSE)
            # if there were NA's in mi[,alongd], it's length will not equal b's
            if (any(mi.na) != (length(b)!=nrow(mi)))
                stop("internal indexing inconsistency: expecting NA's in mi iff length(b)!=nrow(mi)")
            # are subchunks out-of-order?
            if (length(b)!=length(mi[,alongd]) || !all(b==mi[,alongd])) {
                # Have to reconstruct the full numerical vector index vi corresponding to mi
                # This will have NA's in it if mi has NA's.
                # Might already have vi if we started with a vector index
                if (is.null(vi)) {
                    vi <- mi[,1]
                    for (j in seq(ncol(mi))[-1])
                        vi <- vi + ((mi[,j]-1) * prod(c(1,d)[seq(len=j)]))
                }
                # ai is the vector indices of the retrieved elements corresponding to non-NA rows in mi
                ai <- vi[unlist(subidx)]
                # get elements of the result in the correct order
                # here is where NA's corresponding to NA rows in mi get introduced back into the result
                a <- a[match(vi, ai)]
            }
        } else if (length(a) == 1) {
            if (any(mi.na)) {
                i <- which(sapply(subidx, length)>0)
                if (length(i)!=1)
                    stop("internal indexing inconsistency: expecting only one non-null subidx")
                a <- replace(rep(NA, nrow(mi)), which(!mi.na), a[[1]])
            } else {
                a <- a[[1]]
            }
        } else {
            if (nrow(mi)) {
                if (any(!mi.na))
                    stop('internal error: expecting all mi to be NA')
                a <- rep(replace(x$info[[1]]$sample[1], TRUE, NA), nrow(mi))
            } else {
                a <- x$info[[1]]$sample[0]
            }
        }
    } else if (Nidxs<=0) {
        # get both [] and [,drop=F]
        stop("empty subscripts not supported for 'varray' objects")
    } else {
        stop("need ", length(d), " or 1 (matrix or vector) index arguments")
    }
    if (!is.na(fill) && any(i <- is.na(a)))
        a[i] <- fill
    a
}

is.true <- function(x) (x & !is.na(x))

#' @describeIn varray.methods Replacement method for dimnames (stops with error)
#' @method dimnames<- varray
"dimnames<-.varray" <- function(x, value) stop('dimnames for varray are read-only')
#' @describeIn varray.methods Replacement method for dim (stops with error)
#' @method dim<- varray
"dim<-.varray" <- function(x, value) stop('dim for varray is read-only')
#' @describeIn varray.methods Replacement method for length (stops with error)
#' @method length<- varray
"length<-.varray" <- function(x, value) stop('length for varray is read-only')
#' @describeIn varray.methods Replacement method for mode (stops with error)
#' @method mode<- varray
"mode<-.varray" <- function(x, value) stop('mode for varray is read-only')
# "storage.mode<-.varray" <- function(x, value) stop('storage.mode for varray is read-only')
#' @describeIn varray.methods Replacement method for subset (stops with error)
#' @method [<- varray
"[<-.varray" <- function(x, i, j, ..., value) stop('cannot replace parts a varray (varray is read-only -- you must work with the sub-arrays)')

#' Remove a varray and its component objects
#' @param x The name of a \code{varray} object to remove, either quoted or unquoted.
#' @param list A character vector of names of varray objects to be removed.
#' @details Removes all the component objects of a varray,
#' then removes the varray object itself.
#' @return Returns the vector of names of objects as an invisible object.
rm.varray <- function(x, list=NULL) {
    if (is.null(list)) {
        x.name <- substitute(x)
        if (!is.name(x.name) && !is.character(x.name))
            stop('must supply the name of a varray object')
        list <- as.character(x.name)
    }
    for (x.name in list) {
        env <- find(x.name, numeric=TRUE)
        if (length(env)==0)
            stop("'", x.name, "' not found")
        env <- env[1]
        x <- get(x.name, pos=env)
        if (!inherits(x, 'varray'))
            stop('must supply the name of a varray object')
        remove(list=x.name, inherits=FALSE, pos=env)
        for (i in seq(along=x$info)) {
            if (is.null(x$info[[i]]$value)) {
                if (!is.null(x$info[[i]]$env.name) && !identical(x$info[[i]]$env.name, FALSE)) {
                    env <- as.environment(x$info[[i]]$env.name)
                    if (exists(x$info[[i]]$name, envir=env))
                        remove(list=x$info[[i]]$name, envir=env, inherits=FALSE)
                    else
                        warning('component ', x$info[[i]]$name, ' not found in env ', x$info[[i]]$env.name)
                } else {
                    env <- find(x$info[[i]]$name, numeric=TRUE)
                    if (length(env))
                        remove(list=x$info[[i]]$name, pos=env[1], inherits=TRUE)
                    else
                        warning('component ', x$info[[i]]$name, ' not found')
                }
            }
        }
    }
    invisible(list)
}
