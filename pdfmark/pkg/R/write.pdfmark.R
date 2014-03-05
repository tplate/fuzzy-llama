#' Write an R structure of pdf bookmarks to a pdfmark file
#' @param marks a dataframe of pdf bookmarks (see \code{\link{pdfmark}})
#' @param file a filename or connection to write to
#' @param pageformat if non-empty, this character string format is used for appending page numbers to the titles in the table of contents, e.g., ' [\%s]'.
#' @seealso pdfmark
write.pdfmark <- function(marks, file=stdout(), pageformat='') {
    ## marks is a dataframe with these columns
    ##   title=tr, page=pageno, level=1, open=TRUE
    ## These get translated to '[/Title (', tr, ') /Page ', pageno, ' /OUT pdfmark\n'
    ## ii is the hierarchical index of the next position, with numbers as strings
    ## pageformat is

    ## $ gs -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -sOutputFile=out.pdf in-*.pdf pdfmarks

    ## Where out.pdf is the generated PDF, in-*.pdf are the input PDFs, and pdfmarks is a text file with contents like:

    ## [/Title (Title Page) /Page 1 /OUT pdfmark
    ## [/Title (Table of Contents) /Page 3 /OUT pdfmark
    ## ...

    ## For nested levels, use the /Count attribute. For example:

    ## [/Count 3 /Title (Chapter 1) /Page 1 /OUT pdfmark
    ## [/Count -2 /Title (Section 1.1) /Page 2 /OUT pdfmark
    ## [/Title (Section 1.1.1) /Page 3 /OUT pdfmark
    ## [/Title (Section 1.1.2) /Page 4 /OUT pdfmark
    ## [/Count -1 /Title (Section 1.2) /Page 5 /OUT pdfmark
    ## [/Title (Section 1.2.1) /Page 6 /OUT pdfmark
    ## [/Title (Section 1.3) /Page 7 /OUT pdfmark

    ## The argument to /Count gives the number of immediately subordinate bookmarks.
    ## The sign of the argument sets the default display (negative for closed, positive for open).

    ## You can also setup the document info dictionary with something like:

    ## [ /Title (My Test Document)
    ##   /Author (John Doe)
    ##   /Subject (pdfmark 3.0)
    ##   /Keywords (pdfmark, example, test)
    ##   /DOCINFO pdfmark

    if (is.character(file)) {
        file <- file(file, 'w')
        on.exit(close(file))
    }
    ## Translate marks to a list of lists, each containing:
    ##   list(title=tr, page=pageno, level=1, open=TRUE)
    if (is.data.frame(marks))
        marks <- lapply(seq(nrow(marks)), function(i) marks[i,,drop=FALSE])
    ## Derive the hierarchical structure of nested bookmarks
    ## so that we can write the counts in the file
    ii <- "0"
    markhr <- list()
    for (j in seq(along=marks)) {
        m <- marks[[j]]
        if (m$level == length(ii)) {
        } else if (m$level < length(ii)) {
            ii <- ii[seq(len=m$level)]
        } else if (m$level > length(ii)) {
            if (m$level > length(ii)+1)
                stop('level cannot jump by more than 1')
            ii <- c(ii, '0')
        }
        ii[length(ii)] <- as.character(as.numeric(tail(ii,1))+1)
        markhr[[ii]] <- list(contents=list(title=as.character(m$title), page=m$page, open=m$open))
    }
    cat('[/PageMode /UseOutlines /Page 1 /View [/Fit] /DOCVIEW pdfmark\n', file=file)
    ii <- "1"
    while (length(ii)) {
        if (!is.null(markhr[[ii]]$contents)) {
            m <- markhr[[ii]]$contents
            if (!is.null(pageformat) && pageformat!='')
                m$title <- paste(m$title, sprintf(pageformat, m$page), sep='')
            nesteditems <- setdiff(names(markhr[[ii]]), 'contents')
            if (length(nesteditems)) {
                count <- length(nesteditems)
                if (!is.null(m$open) && !m$open)
                    count <- -count
                cat('[/Title (', m$title, ') /Page ', m$page, ' /Count ', count, ' /OUT pdfmark\n', sep='', file=file)
                ii <- c(ii, '0')
            } else {
                cat('[/Title (', m$title, ') /Page ', m$page, ' /OUT pdfmark\n', sep='', file=file)
            }
        } else {
            ii <- ii[-length(ii)]
        }
        ii[length(ii)] <- as.character(as.numeric(tail(ii,1))+1)
    }
    invisible(markhr)
}
