#' Add bookmarks to a pdf file using ghostscript
#'
#' This function does the work of adding bookmarks to an existing PDF file that has no bookmarks.  It uses the external program \code{gs} (from 'Ghostscript') to modify the PDF file.  Bookmarks are the navigation aids in PDF viewers that give a table-of-contents-like view in a left pane and allow users to easily jump to certain pages.
#'
#' @param pdfin The name of the pdf file to add bookmarks to
#' @param pdfmark A dataframe of pdf bookmarks
#' @param pdfout (optional) if supplied, this file will contain the output.  If not supplied, pdfin will be overwritten
#' @param pdfmarkfile (optional) if supplied, the intermediate pdfmarks are written here (and the file is not removed)
#' @param pageformat if non-empty, this character string format is used for appending page numbers to the titles in the table of contents, e.g., ' [\%s]'.
#' @param verbose output verbose info?
#' @param tmpdir where to put temporary files.  Defaults to the directory where the output file will be left.
#'
#' @details Requires the 'gs' program from \href{http://www.ghostscript.com/download/gsdnld.html}{Ghostscript} to be installed and runnable on the machine.
#'
#' Runs in three steps:
#' \enumerate{\item Write out a bookmark (pdfmark) file from the pdfmark object.
#' \item Add the bookmarks to the original PDF file using \code{gs}, creating a new PDF file.
#' \item Rename the new PDF file to the original name.
#' }
#'
#' Each page in PDF file can have zero or more bookmarks.
#'
#' Output from Ghostscript is written to the console.
#'
#' @examples
#' # Very simple example, no page numbers written on pages.
#' # First: create PDF file
#' pdf('tmp1.pdf')
#' symbols(0,0,circles=1)
#' symbols(0,0,squares=1)
#' dev.off()
#' # Second: create R object containing bookmakrs
#' marks <- pdfmark('circles', page=1)
#' marks <- pdfmark('squares', page=2, marks=marks)
#' marks
#' # Third: add bookmarks to PDF file
#' pdf.pdfmark('tmp1.pdf', marks) # overwrite the original file
#'
#' # Semi automatic page-numbering on contents of PDF file
#' # Each time pdfmark() is called, it increments the pageno by 1,
#' # so be careful to synchronize calls with actual pages generated.
#' pdf('tmp2.pdf')
#' par(oma=c(2,0,2,0))
#' symbols(0,0,circles=1)
#' marks <- pdfmark('circles', pos='bottom', plot=TRUE) # don't supply marks argument for first bookmark
#' symbols(0,0,squares=1)
#' marks <- pdfmark('squares', marks=marks, plot=TRUE)
#' dev.off()
#' pdf.pdfmark('tmp2.pdf', marks)

pdf.pdfmark <- function(pdfin, pdfmark, pdfout=NULL, pdfmarkfile=NULL, pageformat='', verbose=TRUE, tmpdir=NULL) {
    # pdfmark is a list of pdfmark info, in the form accepted by write.pdfmark
    gsexe <- find.gs(mustexist=TRUE)
    if (!file.exists(pdfin))
        stop(pdfin, ' does not exist')
    if (!is.null(pdfout)) {
        rename <- FALSE
        if (file.exists(pdfout)) {
            file.remove(pdfout)
            if (file.exists(pdfout))
                stop('failed to remove existing pdfout file: ', pdfout)
        }
        if (is.null(tmpdir))
            tmpdir <- dirname(pdfout)
    } else {
        rename <- TRUE
        if (is.null(tmpdir))
            tmpdir <- dirname(pdfin)
        pdfout <- tempfile(pattern=basename(pdfin), fileext='pdf', tmpdir=tmpdir)
        on.exit(file.remove(pdfmarkfile))
    }
    if (is.null(pdfmarkfile))
        pdfmarkfile <- tempfile(pattern=basename(pdfin), fileext='pdfmark', tmpdir=tmpdir)
    write.pdfmark(pdfmark, pdfmarkfile, pageformat=pageformat)
    args <- c('-dBATCH', '-dNOPAUSE', '-dAutoRotatePages="/None"',
              '-sPAPERSIZE=letter',  '-sDEVICE=pdfwrite',
              paste('-sOutputFile=', shQuote(pdfout), sep=''),
              shQuote(pdfin),
              shQuote(pdfmarkfile))
    res <- system2(gsexe, args, stdout=TRUE, stderr=TRUE)
    if (verbose)
        cat(res)
    if (rename && ((is.integer(res) && res==0) || (is.character(res) && is.null(attr(res, 'status'))))) {
        if (file.copy(pdfout, pdfin, overwrite = TRUE))
            file.remove(pdfout)
        else
            warning('failed to rename ', pdfout, ' to ', pdfin)
    }
    invisible(res)
}
