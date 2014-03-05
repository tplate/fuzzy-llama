#' Generate a PDF bookmark, optionally writing a page number on the current device
#' @param title text of the bookmark
#' @param marks a dataframe of pdf bookmarks (a return value from this function)
#' @param page the page number to use, both in the bookmark and to write on the device
#' @param level top level is 1, bottom level is 4
#' @param open when the PDF file is viewed, are the sub-levels initially shown open or closed?
#' @param plot.it should the page number be written on the current device
#' @param pos controls writing the page number to the current device
#' @param line controls writing the page number to the current device
#' @param adj controls writing the page number to the current device
#' @param outer controls writing the page number to the current device
#' @param format controls writing the page number to the current device
#' @param mtext.args controls writing the page number to the current device
#' @return A dataframe of pdf bookmarks, suitable for passing to \code{\link{pdf.pdfmark}}
#' @details The controls for writing the page number to the current device are stored in the return value and reused for the next call to pdfmark()
pdfmark <- function(title,
                    marks=list(),
                    page=max(marks$page, 0)+1,
                    level=1,
                    open=level <= 2,
                    plot.it=FALSE,
                    pos=c('bottom', 'bottomleft', 'bottomright',
                          'top', 'topleft', 'topright'),
                    line = attr(marks, 'plot.pageno')$line,
                    adj = attr(marks, 'plot.pageno')$adj,
                    outer = non.null(attr(marks, 'plot.pageno')$outer, TRUE),
                    format = non.null(attr(marks, 'plot.pageno')$format, 'Page %s'),
                    mtext.args=non.null(attr(marks, 'plot.pageno')$args, list())) {
    # marks is the dataframe of pdfmarks so far (prev return value from this function)
    plot.pageno <- non.null(attr(marks, 'plot.pageno'), list())
    if (missing(pos)) pos <- non.null(attr(marks, 'plot.pageno')$pos, 'bottom')
    else pos <- match.arg(pos)
    if (is.null(adj))
        adj <- if (regexpr('left', pos)>0) 0.05 else if (regexpr('right', pos)>0) 0.95 else 0.5
    side <- switch(substring(pos, 1, 3), top=3, bot=1)
    # on Quartz device, 'line' must be one less than the corresponding
    # outer margin for the text to appear in the outer margin
    if (is.null(line) && plot.it)
        line <- max(-1, par('oma')[side] - 1)
    if (plot.it)
        do.call('mtext', c(list(text=sprintf(format, page), side=side, adj=adj, outer=outer), mtext.args))
    plot.pageno$pos <- pos
    plot.pageno$line <- line
    plot.pageno$adj <- adj
    plot.pageno$outer <- outer
    plot.pageno$args <- mtext.args
    res <- rbind(marks, data.frame(title=title, page=page, level=level, open=open))
    attr(res, 'plot.pageno') <- plot.pageno
    return(res)
}
