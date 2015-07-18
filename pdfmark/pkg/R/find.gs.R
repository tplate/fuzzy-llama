#' Find the ghostscript executable
#'
#' Internal utility function to find the ghostscript executable.
#'
#' @param mustexist stop with an error if the executable does not exist
#' @param gsexe full path or program name (env vars not used if this is supplied)
#' @return the full pathname of the ghostscript executable
#' @details If env var R_GSCMD is set (or GSC under Windows), return
#'   that value (or stop with an error if it does not exist.)
#'   If these env vars are not set, look for an executable on the path or in
#'   likely places.  R_GSCMD or GSC can be a full path or just
#'   a program name, in the latter case, it is looked for on the system PATH.
#'
#'
find.gs <- function(mustexist=TRUE, gsexe=NULL) {
    if (is.null(gsexe) || !nzchar(gsexe))
        gsexe <- Sys.getenv("R_GSCMD")
    if (.Platform$OS.type == "windows" && !nzchar(gsexe))
        gsexe <- Sys.getenv("GSC")
    if (!is.null(gsexe) && nzchar(gsexe)) {
        if (!is.absolute.path(gsexe)) {
            poss <- Sys.which(gsexe)
            if (length(poss))
                gsexe <- poss[1]
            else if (mustexist)
                stop('GS executable not found: "', gsexe, '"')
        } else if (!file.exists(gsexe) && mustexist) {
            stop('GS executable not found: "', gsexe, '"')
        }
    }
    if (is.null(gsexe) || !nzchar(gsexe)) {
        gsexe <- switch(.Platform$OS.type,
                        unix = {
                            poss <- Sys.which("gs")
                            if (length(poss)) poss[1] else "gs"
                        },
                        windows = {
                            poss <- Sys.which(c("gswin64c.exe", "gswin32c.exe"))
                            poss <- poss[nzchar(poss)]
                            if (!length(poss))
                                poss <- Sys.glob('c:/Program Files*/gs/gs*/bin/gswin64c.exe')
                            if (!length(poss))
                                poss <- Sys.glob('c:/Program Files*/gs/gs*/bin/gswin32c.exe')
                            if (length(poss)) poss[1] else "gswin32c.exe"
                        })
    }
    if (!is.null(gsexe) && nzchar(gsexe)) {
        if (.Platform$OS.type == "windows" && length(grep(" ", gsexe, fixed = TRUE)))
            gsexe <- shortPathName(gsexe)
    } else if (mustexist) {
        stop('GS executable not found: "', gsexe, '"')
    }
    return(gsexe)
}

is.absolute.path <- function(file) {
    return(regexpr("^/", file)==1
           | regexpr("^\\\\", file)==1
           | regexpr("^\\./", file)==1
           | regexpr("^[a-zA-Z]:", file)==1)
}

non.null <- function(...) {
    # return the first non-null argument
    args <- list(...)
    for (i in seq(along=args))
        if (!is.null(args[[i]]))
            return(args[[i]])
    return(args[[length(args)]])
}
