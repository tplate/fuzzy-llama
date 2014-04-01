#' Draw tables on a plot device
#'
#' @details Multiple tables:
#' Each table can be placed relative to a previous one, using the nexto= arg.
#' above=, below=, rightof=, leftof=
#' distance=n (in cex units)
#' alignref=c('table', 'figure')
#' alignto=c('center', 'top', 'bottom', 'left', 'right')
#' alignmy=c('center', 'top', 'bottom', 'left', 'right')
#'
#'

###############################################################################
# Modified from PerformanceAnalytics::textplot() Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
# PerformanceAnalytics is distributed under the terms of the GNU Public License (GPL)
###############################################################################
# # Original function from gplots package written by warnes
# $Id: textplot.R 1883 2012-03-25 00:59:31Z braverock $

# Example using format.df as a pre-processor
# > textplot(format.df(t(y), na.blanks=F,cdec=c(3,3,1)), valign.row="center", wrap.rownames=20, wrap.colnames=10, cex=1)
# > dd <- data.frame(first.col=c(first.row=1,b=2,c=3), second.row=4:6, '3rd'=6:8, '4th'=1/(7:9))
# > textplot(dd, font.rownames=1, font.colnames=3, col.colnames='limegreen', col.data=matrix(1:6,ncol=2), corner='K', col.bg='pink', bg.data='lightgreen', bg.row='lightblue', debug.align=T, cmar=0.2, bdo=2, bdi=1)

#' Display text information in a graphics plot.
#'
#' This function displays text output in a graphics window.  It is the
#' equivalent of 'print' except that the output is displayed as a plot.
#'
#' A new plot is created and the object is displayed using the largest font
#' that will fit on in the plotting region.  The \code{halign} and
#' \code{valign} parameters can be used to control the location of the string
#' within the plotting region.
#'
#' For matrixes and vectors a specialized textplot function is available, which
#' plots each of the cells individually, with column widths set according to
#' the sizes of the column elements.  If present, row and column labels will be
#' displayed in a bold font.
#'
#' textplot also uses replaceTabs, a function to replace all tabs in a string
#' with an appropriate number of spaces.  That function was also written by
#' Gregory R. Warnes and included in the 'gplots' package.
#'
#' @aliases textplot textplot.default textplot.character textplot.matrix
#' textplot.data.frame replaceTabs
#' @param object Object to be displayed.
#' @param halign Alignment in the x direction, one of "center", "left", or
#' "right".
#' @param valign Alignment in the y direction, one of "center", "top" , or
#' "bottom"
#' @param cex Character size, see \code{\link{par}} for details. If unset, the
#' code will attempt to use the largest value which allows the entire object to
#' be displayed.
#' @param mar Figure margins, see the documentation for \code{par}.
#' @param rmar,cmar Space between rows or columns, in fractions of the size of
#' the letter 'M'.
#' @param show.rownames,show.colnames Logical value indicating whether row or
#' column names will be displayed.
#' @param hadj,vadj Vertical and horizontal location of elements within matrix
#' cells.  These have the same meaning as the \code{adj} graphics paramter (see
#' \code{\link{par}}).
#' @param col.data Colors for data elements.  If a single value is provided,
#' all data elements will be the same color.  If a matrix matching the
#' dimensions of the data is provided, each data element will receive the
#' specified color.
#' @param col.rownames,col.colnames Colors for row names and column names,
#' respectively.  Either may be specified as a scalar or a vector of
#' appropriate length.
#' @param max.cex Sets the largest text size as a ceiling
#' @param valign.row Sets the vertical alignment of the row as "top", "bottom",
#' or (default) "center".
#' @param valign.heading Sets the vertical alignment of the heading as "top",
#' (default) "bottom", or "center".
#' @param wrap If TRUE (default), will wrap column names and rownames
#' @param wrap.colnames The number of characters after which column labels will
#' be wrapped.  Default is 10.
#' @param wrap.rownames The number of characters after which row headings will
#' be wrapped.  Default is 10.
#' @param add add to an existing plot, if FALSE, call plot.new() to create a new plot
#' @param text in the function 'replaceTabs', the text string to be processed
#' @param width in the function 'replaceTabs', the number of spaces to replace
#' tabs with
#' @param bdo     borders around outside of table
#' @param bdi     borders inside body of table
#' @param hbdi    horizontal border inside the table body (default = bdi)
#' @param vbdi    vertical border inside the table body (default = bdi)
#' @param vbdh    vertical borders inside the table header at top (between cells) (default = bdi)
#' @param hbdh    horizontal borders inside the tabld header at left (between cells) (default = bdi)
#' @param hbds    horizontal border separating the header (colnames) from the body (default = bdi)
#' @param vbds    vertical border separating the header (rownames) from the body (default = bdi)
#' @param lty.bd  line type to use in borders (defaults to 1)
#' @param col.bd  color to use in borders (defaults to 1)
#' @param lwd.bd  line width to use in borders (defaults to 0, which means no borders)
#' @param \dots Optional arguments passed to the text plotting command or
#' specialized object methods
#' @author Originally written by Gregory R. Warnes
#' \email{warnes@@bst.rochester.edu} for the package 'gplots', modified by
#' Peter Carl
#' @seealso \code{\link{plot}}, \cr \code{\link{text}}, \cr
#' \code{\link[utils]{capture.output}}, \cr \code{\link[gplots]{textplot}}
#' @keywords hplot
#' @examples
#'
#' # Also see the examples in the original gplots textplot function
#' data(managers)
#' textplot(table.AnnualizedReturns(managers[,1:6]))
#'
#' # This was really nice before Hmisc messed up 'format' from R-base
#' # prettify with format.df in hmisc package
#' # require("Hmisc")
#'   result = t(table.CalendarReturns(managers[,1:8]))[-1:-12,]
#'
#' #  textplot(Hmisc::format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=rep(1,dim(result)[2])), rmar = 0.8, cmar = 1,  max.cex=.9, halign = "center", valign = "top", valign.row="center", wrap.rownames=20, wrap.colnames=10, col.rownames=c("red", rep("darkgray",5), rep("orange",2)), mar = c(0,0,4,0)+0.1)
#'
#' # title(main="Calendar Returns")
#'
#'
textplot <- function(object, halign="center", valign="center", cex,
                            max.cex = 1, cmar=2, rmar=0.5,
                            show.rownames=TRUE, show.colnames=TRUE,
                            font.rownames=2, font.colnames=2,
                            hadj=1, vadj=NULL,
                            valign.row="center",
                            valign.heading = "bottom",
                            mar= c(0,0,0,0)+0.1,
                            col.data=par("col"),
                            col.rownames=par("col"),
                            col.colnames=par("col"),
                            wrap = TRUE,
                            wrap.colnames = 10,
                            wrap.rownames = 10, ... )
  UseMethod('textplot')


textplot.default <- function(object,
                             halign=c("center","left","right"),
                             valign=c("center","top","bottom"),
                             cex,
                            max.cex, cmar, rmar,
                            show.rownames, show.colnames,
                            font.rownames, font.colnames,
                            hadj, vadj,
                            valign.row,
                            valign.heading,
                            mar,
                            col.data,
                            col.rownames,
                            col.colnames,
                            wrap,
                            wrap.colnames,
                            wrap.rownames,... )
{
  # Looks like this would not pass on args to textplot.matrix,
  # but textplot.matrix is dispatched on calling textplot() on a matrix,
  # so it doesn't matter.
  if (is.matrix(object) || (is.vector(object) && length(object)>1) )
    return(textplot.matrix(object, halign, valign, cex, ... ))

  halign <- match.arg(halign)
  valign <- match.arg(valign)

  textplot.character(object, halign,  valign, cex, ...)
}


textplot.data.frame <- function(object,
                             halign=c("center","left","right"),
                             valign=c("center","top","bottom"),
                             cex,
                            max.cex = 1, cmar=2, rmar=0.5,
                            show.rownames=TRUE, show.colnames=TRUE,
                            font.rownames=2, font.colnames=2,
                            hadj=1, vadj=NULL,
                            valign.row="center",
                            valign.heading = "bottom",
                            mar= c(0,0,0,0)+0.1,
                            col.data=par("col"),
                            col.rownames=par("col"),
                            col.colnames=par("col"),
                            wrap = TRUE,
                            wrap.colnames = 10,
                            wrap.rownames = 10, ... ){
    textplot.matrix(object, halign, valign, cex,
                            max.cex, cmar, rmar,
                            show.rownames, show.colnames,
                            font.rownames, font.colnames,
                            hadj, vadj,
                            valign.row,
                            valign.heading,
                            mar,
                            col.data,
                            col.rownames,
                            col.colnames,
                            wrap,
                            wrap.colnames,
                            wrap.rownames, ... )
}

textplot.matrix <- function(object,
                            halign=c("center","left","right"),
                            valign=c("center","top","bottom"),
                            cex=NULL, max.cex = 1, cmar=2, rmar=0.5,
                            object.fit=NULL,
                            show.rownames=TRUE,
                            show.colnames=TRUE,
                            font.rownames=2,
                            font.colnames=2,
                            hadj=1, vadj=NULL,
                            valign.row="center",
                            valign.heading = "center",
                            mar= c(0,0,0,0)+0.1, # original settings: c(1,1,4,1)+0.1,
                            # mar: a numerical vector of the form c(bottom, left, top, right) which
                            # gives the number of lines of margin to be specified on the four sides
                            # of the plot. The default is c(5, 4, 4, 2) + 0.1
                            col.data=par("col"),
                            col.rownames=par("col"),
                            col.colnames=par("col"),
                            wrap = TRUE, # fix this to add other methods
                            wrap.colnames = 10, # wrap after how many characters?
                            wrap.rownames = 10, # wrap after how many characters?
                            add=FALSE,
                            x=NA,
                            y=NA,
                            width=NA,
                            height=NA,
                            xlim=NULL,
                            ylim=NULL,
                            hadj.data=hadj,
                            hadj.rownames=NA,
                            hadj.colnames=NA,
                            hadj.corner=NA,
                            corner.name='',
                            font.data=1,
                            font.corner=NA,
                            col.corner=NA,
                            bg.data=0,
                            bg.rownames=NA,
                            bg.colnames=NA,
                            bg.corner='#C0EEC0',
                            lty.bd=1,
                            col.bd=1,
                            lwd.bd=1,
                            bdo=list(lwd=lwd.bd,lty=lty.bd,col=col.bd),
                            bdi=list(lwd=lwd.bd,lty=lty.bd,col=col.bd),
                            hbdi=bdi,
                            vbdi=bdi,
                            hbds=bdi,
                            vbds=bdi,
                            vbdh=vbdi,
                            hbdh=hbdi,
                            debug.align=FALSE,
                            nan=NA,
                            na='NA',
                            colwidth=NULL,
                            rowheight=NULL,
                            cex.fit=FALSE,
                            cex.fit.cmar=0,
                            lines.caption=length(caption),
                            caption=NULL,
                            bg.caption=bg.corner,
                            font.caption=font.rownames,
                            hadj.caption=0.5,
                            valign.heading=c("center","top","bottom"),
                            ... )
{
    # @todo: add methods c("wrap", "abbreviate", "both") for handling long column and row names
    # @todo: look at postrix package 'addtable2plot' for ideas about setting borders, text, and other formats by cell
    if (is.vector(object))
        object <- t(as.matrix(object))
    else
        object <- as.matrix(object)
    # convert NaN to NA, which will then be displayed however NA objects are displayed
    obect.nan <- is.nan(object)
    if (any(obect.nan) && is.na(nan))
        object <- replace(object, is.nan(object), NA)

    # Check structure and length of border specs
    # each can be a list of the particular length, with each component
    # being a list of lwd, lty and col
    # Return NULL if no border (lwd==0)
    check.border <- function(x, len, lwd=0, lty=1, col=1, empty.null=TRUE) {
        if (is.null(x))
            if (empty.null)
                return(NULL)
            else
                x <- 0
        if (!is.list(x))
            if (!is.numeric(x) || length(x)!=1)
                stop('non-list border specification must be a single integer')
            else
                x <- list(lwd=x, lty=lty, col=col)
        # If this is just a single list like list(lwd=...), replicate
        # it to be list(list(lwd=...), list(lwd=...))
        if (!is.list(x[[1]]))
            x <- rep(list(x), len)
        x <- lapply(x, function(y) {
            if (is.null(y$lwd))
                y$lwd <- lwd
            else if (!is.numeric(y$lwd) || length(y$lwd)!=1)
                stop("border component 'lwd' must be numeric length 1")
            if (is.null(y$lty))
                y$lty <- lty
            else if (!is.numeric(y$lty) || length(y$lty)!=1)
                stop("border component 'lty' must be numeric length 1")
            if (is.null(y$col))
                y$col <- col
            else if ((!is.numeric(y$col) && !is.character(y$col)) || length(y$col)!=1)
                stop("border component 'col' must be numeric or char length 1")
            y
        })
        if (length(x) != len)
            x <- x[(seq(0, len=len) %% length(x))+1]
        if (!empty.null || any(sapply(x, function(y) y$lwd>0)))
            return(x)
        else
            return(NULL)
    }
    bdo <- check.border(bdo, len=4, lwd=lwd.bd, lty=lty.bd, col=col.bd)
    hbdi <- check.border(hbdi, len=nrow(object)-1, lwd=lwd.bd, lty=lty.bd, col=col.bd) # horizontal
    vbdi <- check.border(vbdi, len=ncol(object)-1, lwd=lwd.bd, lty=lty.bd, col=col.bd) # vertical
    vbdh <- check.border(vbdh, len=ncol(object)-1, lwd=lwd.bd, lty=lty.bd, col=col.bd) # vertical
    hbdh <- check.border(hbdh, len=nrow(object)-1, lwd=lwd.bd, lty=lty.bd, col=col.bd) # horizontal
    # hbds: hline between the colnames header and the table body,
    # need this if we have hlines either in the body or in the left hand colnames header
    hbds <- check.border(hbds, len=1,
                         empty.null=is.null(hbdi) && is.null(vbdh),
                         lwd=lwd.bd, lty=lty.bd, col=col.bd) # horizontal
    vbds <- check.border(vbds, len=1,
                         empty.null=is.null(vbdi) && is.null(hbdh),
                         lwd=lwd.bd, lty=lty.bd, col=col.bd) # vertical

    rep.matrix <- function(x, ncols, nrows) {
        # fill a matrix out to the given size, recycling columns & rows as appropriate
        # a vector x is treated as a single column
        if (is.null(dim(x)))
            x <- matrix(x, ncol=1)
        if (ncol(x) != ncols)
            x <- x[, rep(seq(len=ncol(x)), len=ncols), drop=FALSE]
        if (nrow(x) != nrows)
            x <- x[rep(seq(len=nrow(x)), len=nrows), , drop=FALSE]
        x
    }

    # check dimensions of col.data, col.rownames, col.colnames
    col.data <- rep.matrix(col.data, nrow=nrow(object), ncol=ncol(object))

    if (length(col.rownames)==1)
        col.rownames <- rep(col.rownames, nrow(object))

    if (length(col.colnames)==1)
        col.colnames <- rep(col.colnames, ncol(object))

    # check dimensions of font.data, font.rownames, font.colnames
    font.data <- rep.matrix(font.data, nrow=nrow(object), ncol=ncol(object))

    if (length(font.rownames)==1)
        font.rownames <- rep(font.rownames, nrow(object))

    if (length(font.colnames)==1)
        font.colnames <- rep(font.colnames, ncol(object))

    if (all(is.na(bg.colnames)) && all(is.na(bg.rownames)) && all(is.na(bg.corner)))
        bg.colnames <- bg.rownames <- bg.corner <- 0
    if (is.na(bg.colnames))
        bg.colnames <- non.na(bg.corner, bg.rownames[1])
    if (is.na(bg.rownames))
        bg.rownames <- non.na(bg.corner, bg.colnames[1])

    # check dimensions of bg.data, bg.rownames, bg.colnames
    bg.data <- rep.matrix(bg.data, nrow=nrow(object), ncol=ncol(object))

    if (length(bg.rownames)==1)
        bg.rownames <- rep(bg.rownames, nrow(object))

    if (length(bg.colnames)==1)
        bg.colnames <- rep(bg.colnames, ncol(object))

    # check dimensions of hadj.data, hadj.rownames, hadj.colnames
    hadj.data <- rep.matrix(hadj.data, nrow=nrow(object), ncol=ncol(object))

    if (!is.na(hadj.corner) && length(hadj.rownames)==1 && is.na(hadj.rownames))
        hadj.rownames <- hadj.corner
    if (length(hadj.rownames)==1)
        hadj.rownames <- rep(hadj.rownames, nrow(object))

    if (length(hadj.colnames)==1)
        hadj.colnames <- rep(hadj.colnames, ncol(object))

    halign=match.arg(halign)
    valign=match.arg(valign)

    opar <- par()[c("mar","xpd","cex")]
    on.exit( par(opar) )
    par(xpd=FALSE)

    if (is.list(x)) {
        if (is.null(x$y) || is.null(x$x))
            stop("both x and y coordinates must be given in list x")
        y <- x$y
        x <- x$x
    }

    width.dsrd.orig <- width
    height.dsrd.orig <- height
    # setup plot area
    if (!add) {
        if (!is.null(mar))
            par(mar=mar)
        plot.new()
        if (is.null(xlim))
            xlim <- c(0,1)
        if (is.null(ylim))
            ylim <- c(0,1)
        xpos <- if (is.na(x)) xlim[1] else x
        if (is.na(width))
            width <- diff(xlim)
        ypos <- if (is.na(y)) ylim[2] else y
        if (is.na(height))
            height <- diff(ylim)
        plot.width <- diff(xlim)
        plot.height <- diff(ylim)
        plot.window(xlim=xlim,ylim=ylim, log = "", asp=NA)
    } else {
        plot.height <- diff(par('usr')[3:4])
        plot.width <- diff(par('usr')[1:2])
    }

    # add 'r-style' row and column labels if not present
    if ( is.null(colnames(object) ) )
        colnames(object) <- paste( "[,", 1:ncol(object), "]", sep="" )
    if ( is.null(rownames(object)) )
        rownames(object) <- paste( "[", 1:nrow(object), ",]", sep="")

    # extend the matrix to include row and column labels
    if ( show.rownames ) {
        if (wrap) # wrap row labels
            row.names = sapply(rownames(object), function(x) paste(strwrap(x,wrap.rownames), collapse = "\n"), USE.NAMES=FALSE)
        else
            row.names = rownames(object)
        object <- cbind( row.names, object )
        col.data <- cbind( col.rownames, col.data )
        font.data <- cbind( font.rownames, font.data )
        bg.data <- cbind( bg.rownames, bg.data )
        if (ncol(object) && any(is.na(hadj.rownames)))
            hadj.rownames <- replace(hadj.rownames, is.na(hadj.rownames), hadj.data[is.na(hadj.rownames), 1])
        hadj.data <- cbind(hadj.rownames, hadj.data)
        if (!is.null(object.fit))
            object.fit <- abind(matrix(row.names, ncol=ntril(object.fit), nrow=nrow(object.fit)), object.fit, along=2)
    }
    if ( show.colnames ) {
        if (wrap) # wrap column labels
            column.names = sapply(colnames(object), function(x) paste(strwrap(x,wrap.colnames), collapse = "\n"), USE.NAMES=FALSE)
        else
            column.names = colnames(object)
        object <- rbind(column.names, object )
        if (ncol(object) && any(is.na(hadj.colnames)))
            hadj.colnames <- replace(hadj.colnames, is.na(hadj.colnames), hadj.data[1, 1])
        if (!is.null(object.fit))
            object.fit <- abind(matrix(column.names, ncol=ntril(object.fit), nrow=ncol(object.fit)), object.fit, along=1)

        if (show.rownames) {
            object[1,1] <- corner.name
            if (!is.null(object.fit))
                object.fit[1,1,] <- trilnames(object.fit)
            if (is.na(col.corner))
                col.corner <- col.rownames[1]
            if (is.na(font.corner))
                font.corner <- font.rownames[1]
            if (is.na(bg.corner))
                bg.corner <- bg.rownames[1]
            if (is.na(hadj.corner))
                hadj.corner <- hadj.rownames[1]
            col.data <- rbind( c(col.corner, col.colnames), col.data )
            font.data <- rbind( c(font.corner, font.colnames), font.data )
            bg.data <- rbind( c(bg.corner, bg.colnames), bg.data )
            hadj.data <- rbind( c(hadj.corner, hadj.colnames), hadj.data)
        } else {
            col.data <- rbind( col.colnames, col.data )
            font.data <- rbind( font.colnames, font.data )
            bg.data <- rbind( bg.colnames, bg.data )
            hadj.data <- rbind(hadj.colnames, hadj.data)
        }
    }

    width.dsrd <- width
    height.dsrd <- height

    if (!is.null(colwidth)) {
        if (length(colwidth) != ncol(object))
            stop('supplied colwidth must have length ', ncol(object))
        colwidth.fixed <- TRUE
    } else {
        colwidth.fixed <- FALSE
    }

    if (!is.null(rowheight)) {
        if (length(rowheight) != nrow(object))
            stop('supplied rowheight must have length ', nrow(object))
        rowheight.fixed <- TRUE
    } else {
        rowheight.fixed <- FALSE
    }

    # set the character size
    if ( missing(cex) || is.null(cex)) {
        cex <- max.cex
        lastloop <- FALSE
    } else {
        lastloop <- TRUE
    }

    # If we have a 3-d array of strings to fit, then work out the
    # longest elt in each 2-d cell, and use that
    if (!is.null(object.fit)) {
        object.maxwid <- replace(object, TRUE,
                                 apply(cbind(as.integer(row(object)), as.integer(col(object))),
                                       1, function(idxrc) {
                                           strs <- object.fit[idxrc[1], idxrc[2], ]
                                           i <- which.max(strwidth(strs, cex=cex,
                                                                   font=font.data[idxrc[1], idxrc[2]]))
                                           strs[i]
                                       }))
        object.maxhgt <- replace(object, TRUE,
                                 apply(cbind(as.integer(row(object)), as.integer(col(object))),
                                       1, function(idxrc) {
                                           strs <- object.fit[idxrc[1], idxrc[2], ]
                                           i <- which.max(strheight(strs, cex=cex,
                                                                   font=font.data[idxrc[1], idxrc[2]]))
                                           strs[i]
                                       }))
    } else {
        object.maxwid <- object
        object.maxhgt <- object
    }
    # try to find the right cex to print at
    for (i in 1:20) {
        oldcex <- cex

        if (!colwidth.fixed) {
            colwidth = (strwidth("W",cex=cex) * cmar
                        +
                        sapply(seq(len=ncol(object.maxwid)), function(i)
                               if (length(unique(font.data[,i]))==1)
                               max(strwidth(object.maxwid[,i], cex=cex, font=unique(font.data[,i])))
                               else max(sapply(seq(len=nrow(object.maxwid)),
                                               function(j) strwidth(object.maxwid[j,i], cex=cex,font=font.data[j,i]))))
                        )
        }
        width = sum(colwidth)

        if (!rowheight.fixed) {
            rowheight = (strheight("(",cex=cex) * (1 + rmar )
                         +
                         sapply(seq(len=nrow(object.maxhgt)), function(j)
                                if (length(unique(font.data[j,]))==1)
                                max(strheight(object.maxhgt[j,], cex=cex, font=unique(font.data[j,])))
                                else max(sapply(seq(len=ncol(object.maxhgt)),
                                                function(i) strheight(object.maxhgt[j,i], cex=cex,font=font.data[j,i]))))
                         )
        }
        height=sum(rowheight)
        if (lastloop) break

        cex <- cex / max(width/width.dsrd,height/height.dsrd)

        if (abs(oldcex - cex) < 0.001) {
            lastloop <- TRUE
        }
    }
    # reset to maximum size if "discovered" size is too large
    if (cex>max.cex) {
        cex = max.cex

        if (!colwidth.fixed) {
            colwidth = (strwidth("W",cex=cex) * cmar
                        +
                        sapply(seq(len=ncol(object.maxwid)), function(i)
                               if (length(unique(font.data[,i]))==1)
                               max(strwidth(object.maxwid[,i], cex=cex, font=unique(font.data[,i])))
                               else max(sapply(seq(len=nrow(object.maxwid)),
                                               function(j) strwidth(object.maxwid[j,i], cex=cex,font=font.data[j,i]))))
                        )
        }
        width = sum(colwidth)

        if (!rowheight.fixed) {
            rowheight = (strheight("(",cex=cex) * (1 + rmar )
                         +
                         sapply(seq(len=nrow(object.maxhgt)), function(j)
                                if (length(unique(font.data[j,]))==1)
                                max(strheight(object.maxhgt[j,], cex=cex, font=unique(font.data[j,])))
                                else max(sapply(seq(len=ncol(object.maxhgt)),
                                                function(i) strheight(object.maxhgt[j,i], cex=cex,font=font.data[j,i]))))
                         )
        }
        height=sum(rowheight)
    }

    if (is.na(x)) {
        # setup x alignment for the table xpos will be left of table
        if (halign=="left")
            x <- xpos
        else if (halign=="center")
            x <- xpos + (plot.width - width)/2
        else #if (halign=="right")
            x <- xpos + plot.width - width
    }

    # caption height is always in terms of first row (which will be header row, if used)
    caption.height <- abs(lines.caption) * rowheight[1]

    if (is.na(y)) {
        # setup y alignment for the table ypos will be top of table
        # allow for caption lines at the top or bottom
        if (valign=="top")
            y <- ypos - ifelse(lines.caption>0, lines.caption * rowheight[1], 0)
        else if (valign=="center")
            y <- ypos - plot.height/2 + (height + caption.height)/2
        else #if (valign=="bottom")
            y <- ypos - plot.height + height + ifelse(lines.caption < 0, caption.height, 0)
    }

    # @todo: apply hadj and vadj differently to headers, body; cell-by-cell control of alignment

    # Uncomment these lines to see the dimensions of the box for the table, where rect(xleft, ybottom, xright, ytop)
    # points(xpos,ypos)
    # rect(xpos,ypos-height,xpos+width,ypos)

    # Draw backgrounds
    xpos <- x
    if (any(bg.data!=0 & !is.na(bg.data)))
        for(i in 1:ncol(object)) {
            for(j in 1:nrow(object)) {
                ypos <- y - sum(rowheight[0:(j-1)])
                rect(xpos, ypos - rowheight[j], xpos + colwidth[i], ypos, col= bg.data[j,i], border=0)
            }
            xpos <- xpos + colwidth[i]
        }

    # Draw caption background and borders
    if (caption.height != 0 && !is.na(bg.caption) && bg.caption!=0) {
        if (lines.caption > 0)
            rect(x, y, x + width, y + caption.height, col=bg.caption,
                 border = non.null(bdo[[1]]$col, 0),
                 lty=non.null(bdo[[1]]$lty, 0),
                 lwd=non.null(bdo[[1]]$lwd, 0))
        else
            rect(x, y - height - caption.height, x + width, y - height, col=bg.caption,
                 border = non.null(bdo[[1]]$col, 0),
                 lty=non.null(bdo[[1]]$lty, 0),
                 lwd=non.null(bdo[[1]]$lwd, 0))
    }

    # Draw borders
    xpos <- x
    ypos <- y
    cat('xpos=', xpos, 'ypos=', ypos, '\n')
    if (!is.null(bdo)) {
        if (all(sapply(bdo[-1], function(b) isTRUE(all.equal(b, bdo[[1]]))))) {
            # all outside borders the same
            rect(xpos, ypos-height, xpos+width, ypos,
                 border=bdo[[1]]$col, lty=bdo[[1]]$lty,
                 lwd=bdo[[1]]$lwd)
        } else {
            # not all outside borders the same; draw with lines
            for (i in 1:4)
                if (bdo[[i]]$lwd > 0)
                    lines(x=c(c(xpos,xpos,xpos,xpos+width)[i],
                          c(xpos+width,xpos,xpos+width,xpos+width)[i]),
                          y=c(c(ypos-height,ypos-height,ypos,ypos)[i],
                          c(ypos-height,ypos,ypos,ypos-height)[i]),
                          col=bdo[[i]]$col, lty=bdo[[i]]$lty, lwd=bdo[[i]]$lwd)
        }
    }
    if (show.rownames) {
        # draw the border at the right of the rownames column -- left to the body of the table
        vbdi <- c(vbds, vbdi)
    }
    if (show.colnames) {
        # draw the border under the header row and the body of the table
        # hbds is on top of hbdi
        hbdi <- c(hbds, hbdi)
    }
    # Draw horizontal row separators inside the body of the table
    xpos <- if (show.rownames) x+colwidth[1] else x
    ypos <- y
    for (i in seq(len=length(hbdi))) {
        ypos <- ypos - rowheight[i]
        lines(c(xpos, x+width), c(ypos, ypos), lty=hbdi[[i]]$lty, lwd=hbdi[[i]]$lwd, col=hbdi[[i]]$col)
    }
    # Draw vertical column separators inside the body of the table
    xpos <- x
    ypos <- if (show.colnames) y-rowheight[1] else y
    for (i in seq(len=length(vbdi))) {
        xpos <- xpos + colwidth[i]
        lines(c(xpos, xpos), c(ypos, y - height), lty=vbdi[[i]]$lty, lwd=vbdi[[i]]$lwd, col=vbdi[[i]]$col)
    }
    # Draw vertical lines inside the colnames header
    if (show.colnames && !is.null(vbds)) {
        xpos <- x
        bs <- c(vbds, vbdh)
        for (i in seq(len=length(bs))) {
            xpos <- xpos + colwidth[i]
            lines(c(xpos, xpos), c(y, y - rowheight[1]),
                  lty=bs[[i]]$lty, lwd=bs[[i]]$lwd, col=bs[[i]]$col)
        }
    }
    # Draw horizontal lines inside the rownames header
    if (show.rownames && !is.null(hbds)) {
        ypos <- y
        bs <- c(hbds, hbdh)
        for (i in seq(len=length(bs))) {
            ypos <- ypos - rowheight[i]
            lines(c(x, x + colwidth[1]), c(ypos, ypos),
                  lty=bs[[i]]$lty, lwd=bs[[i]]$lwd, col=bs[[i]]$col)
        }
    }

    if (any(is.na(object)))
        object <- replace(object, is.na(object), na)

    # Draw cell contents
    w.wid <- strwidth("W",cex=cex)
    xpos <- x
    for(i in 1:ncol(object)) {
        # format the header separately here

        for (j in 1:nrow(object)) {
            cex.ji <- cex
            cmar.ji <- cmar
            if (cex.fit) {
                # if we are having trouble fitting, shrink the font, and use a different cmar (usu 0)
                width.ji <- strwidth(object[j,i], cex=cex, font=font.data[j,i])
                if (width.ji+cmar*w.wid > colwidth[i] & width.ji > 0 & colwidth[i] > cmar*w.wid) {
                    cex.ji <- cex * min(1, (colwidth[i] - cex.fit.cmar*w.wid) / width.ji)
                    if (width.ji+cmar/2*w.wid > colwidth[i])
                        cmar.ji <- cex.fit.cmar
                }
            }
            if (hadj.data[j,i] < 0.25)
                xoff <- w.wid * cmar.ji/2
            else if (hadj.data[j,i] > 0.75)
                xoff <- colwidth[i] - w.wid * cmar.ji/2
            else
                xoff <- colwidth[i]/2
            # set new vertical alignment cases here.  This doesn't give cell-by-cell control.
            # will have to do col headings and rownames separately

            if ( show.colnames && j==1 && object[j,i]!=''){
                # create the header
                if (valign.heading=="top") { # This works for valign "top" but not for "centered" or "bottom"
                    ypos = y - strheight("(",cex=cex) * rmar/2
                    vadj = 1
                }
                if (valign.heading=="bottom") {
                    ypos = y - rowheight[1] + strheight("(",cex=cex) * rmar/2
                    vadj = 0
                }
                if (valign.heading=="center") {
                    ypos = y - rowheight[1]/2 # + strheight("(",cex=cex) * (1 + rmar)/2
                    vadj = .5
                }
                if (debug.align)
                    points(xpos+xoff, ypos)
                text(xpos+xoff, ypos, object[j,i], adj=c(hadj.data[j,i],vadj), cex=cex.ji,
                     font=font.data[j,i], col=col.data[j,i], ... )
            }
            else if (object[j,i]!='') {
                if (valign.row=="top") {
                    ypos = y - sum(rowheight[0:(j-1)]) - strheight("(",cex=cex) * rmar/2
                    vadj = 1
                }
                if (valign.row=="bottom") {
                    ypos = y - sum(rowheight[1:(j)]) + strheight("(",cex=cex) * rmar/2
                    vadj = 0
                }
                if (valign.row=="center") {
                    ypos = y - (sum(rowheight[1:(j)]) + sum(rowheight[0:(j-1)]))/2 # + strheight("(",cex=cex) * (1 + rmar)/2
                    vadj = .5
                }
                if (debug.align)
                    points(xpos+xoff, ypos)
                text(xpos+xoff, ypos, object[j,i], adj=c(hadj.data[j,i],vadj), cex=cex.ji, font=font.data[j,i], col=col.data[j,i], ... )
            }
        }
        xpos <- xpos + colwidth[i]

    }
    font.caption <- font.
    if (caption.height != 0) {
        # for the caption, copy the code for cell j,i
        cex.ji <- cex
        cmar.ji <- cmar
        if (cex.fit) {
            # if we are having trouble fitting, shrink the font, and use a different cmar (usu 0)
            width.ji <- max(strwidth(caption, cex=cex, font=font.caption))
            if (width.ji+cmar*w.wid > width & width.ji > 0 & width > cmar*w.wid) {
                cex.ji <- cex * min(1, (width - cex.fit.cmar*w.wid) / width.ji)
                if (width.ji+cmar/2*w.wid > width)
                    cmar.ji <- cex.fit.cmar
            }
        }
        if (hadj.caption < 0.25)
            xoff <- w.wid * cmar.ji/2
        else if (hadj.caption > 0.75)
            xoff <- width - w.wid * cmar.ji/2
        else
            xoff <- width/2

        stop('working here on caption')
        if (valign.heading=="top") { # This works for valign "top" but not for "centered" or "bottom"
            ypos = y - strheight("(",cex=cex) * rmar/2
            vadj = 1
        }
        if (valign.heading=="bottom") {
            ypos = y - rowheight[1] + strheight("(",cex=cex) * rmar/2
            vadj = 0
        }
        if (valign.heading=="center") {
            ypos = y - rowheight[1]/2 # + strheight("(",cex=cex) * (1 + rmar)/2
            vadj = .5
        }
        if (debug.align)
            points(xpos+xoff, ypos)
        text(xpos+xoff, ypos, caption, adj=c(hadj.caption,vadj), cex=cex.ji,
             font=font.caption, col=col.caption, ... )
    }

    par(opar)
    invisible(list(topleft=c(x, y),
                   size=c(width=width, height=height),
                   cex=cex,
                   colwidth=colwidth,
                   rowheight=rowheight,
                   rmar=rmar,
                   cmar=cmar))
}

textplot.character <- function (object,
                                halign = c("center", "left", "right"),
                                valign = c("center", "top", "bottom"),
                                cex,
                                max.cex = 1, cmar=2, rmar=0.5,
                                show.rownames=TRUE, show.colnames=TRUE,
                                hadj=1, vadj=NULL,
                                valign.row="center",
                                valign.heading = "bottom",
                                mar= c(0,0,3,0)+0.1,
                                col.data=par("col"),
                                col.rownames=par("col"),
                                col.colnames=par("col"),
                                wrap = TRUE,
                                wrap.colnames = 10,
                                wrap.rownames = 10,
                                fixed.width=TRUE,
                                cspace=1,
                                lspace=1,
                                tab.width=8,
                                ...)
{
    object <- paste(object,collapse="\n",sep="")
    object <- PerformanceAnalytics:::replaceTabs(object, width=tab.width)

    halign = match.arg(halign)
    valign = match.arg(valign)
    plot.new()

    opar <- par()[c("mar","xpd","cex","family")]
    on.exit( par(opar) )

    par(mar=mar,xpd=FALSE )
    if (fixed.width)
        par(family="mono")

    plot.window(xlim = c(0, 1), ylim = c(0, 1), log = "", asp = NA)

    slist   <- unlist(lapply(object, function(x) strsplit(x,'\n')))
    slist   <- lapply(slist, function(x) unlist(strsplit(x,'')))

    slen    <- sapply(slist, length)
    slines  <- length(slist)

    if (missing(cex)) {
        lastloop <- FALSE
        cex <- 1
    } else
        lastloop <- TRUE

    for (i in 1:20) {
        oldcex <- cex
        #cat("cex=",cex,"\n")
        #cat("i=",i,"\n")
        #cat("calculating width...")
        cwidth  <- max(sapply(unlist(slist), strwidth,  cex=cex)) * cspace
        #cat("done.\n")
        #cat("calculating height...")
        cheight <- max(sapply(unlist(slist), strheight, cex=cex)) * ( lspace + 0.5 )
        #cat("done.\n")

        width <- strwidth(object, cex=cex)
        height <- strheight(object, cex=cex)

        if (lastloop) break

        cex <- cex  / max(width, height)

        if (abs(oldcex - cex) < 0.001) {
            lastloop <- TRUE
        }

    }

    if (halign == "left")
        xpos <- 0
    else if (halign == "center")
        xpos <- 0 + (1 - width)/2
    else xpos <- 0 + (1 - width)

    if (valign == "top")
        ypos <- 1
    else if (valign == "center")
        ypos <- 1 - (1 - height)/2
    else ypos <- 1 - (1 - height)

    text(x=xpos, y=ypos, labels=object, adj=c(0,1),
         cex=cex, ...)

    par(opar)
    invisible(cex)
}

textplot3 <- function(object, ..., bg.data=0, cex=NULL, caption=NULL, lines.caption=0) {
    # plot slices of 3d array as tables, with a caption at the top or bottom
    # haven't really implmenented caption yet...
    lines.caption <- sign(lines.caption) * length(caption)
    if (length(dim(object))!=3)
        stop('expected length(dim(object))==3')
    frac <- nrow(object)/(ntril(object) * nrow(object) + abs(lines.caption))
    # must pass the full object as object.fit in case there are wider values
    # in slices other than the first
    h <- textplot(adrop(object[,,1,drop=FALSE], 3), corner=trilnames(object)[1],
                  bg.data=(if (length(dim(bg.data))==3) adrop(bg.data[,,1,drop=FALSE], 3) else bg.data),
                  object.fit=object, ...,
                  xlim=c(0,1), ylim=c(0,1), height=frac, rmar=0,
                  cex=cex, valign='top', halign='center', lines.caption=max(-lines.caption, 0))
    for (i in seq(2, len=ntril(object)-1)) {
        textplot(adrop(object[,,i,drop=FALSE], 3), corner=trilnames(object)[i], ...,
                 bg.data=(if (length(dim(bg.data))==3) adrop(bg.data[,,i,drop=FALSE], 3) else bg.data),
                 rmar=h$rmar, cmar=h$cmar,
                 x=h$topleft[1], y=h$topleft[2] - (i-1) * h$size['height'],
                 rowheight=h$rowheight, colwidth=h$colwidth, cex=h$cex,
                 valign='top', halign='left', add=TRUE)
    }
}
