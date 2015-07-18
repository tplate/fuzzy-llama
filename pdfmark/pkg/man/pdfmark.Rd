\name{pdfmark}
\alias{pdfmark}
\title{Generate a PDF bookmark, optionally writing a page number on the current device}
\usage{
pdfmark(title, marks = list(), page = max(marks$page, 0) + 1, level = 1,
  open = level <= 2, plot.it = FALSE, pos = c("bottom", "bottomleft",
  "bottomright", "top", "topleft", "topright"), line = attr(marks,
  "plot.pageno")$line, adj = attr(marks, "plot.pageno")$adj,
  outer = non.null(attr(marks, "plot.pageno")$outer, TRUE),
  format = non.null(attr(marks, "plot.pageno")$format, "Page \%s"),
  mtext.args = non.null(attr(marks, "plot.pageno")$args, list()))
}
\arguments{
  \item{title}{text of the bookmark}

  \item{marks}{a dataframe of pdf bookmarks (a return value
  from this function)}

  \item{page}{the page number to use, both in the bookmark
  and to write on the device}

  \item{level}{top level is 1, bottom level is 4}

  \item{open}{when the PDF file is viewed, are the
  sub-levels initially shown open or closed?}

  \item{plot.it}{should the page number be written on the
  current device}

  \item{pos}{controls writing the page number to the
  current device}

  \item{line}{controls writing the page number to the
  current device}

  \item{adj}{controls writing the page number to the
  current device}

  \item{outer}{controls writing the page number to the
  current device}

  \item{format}{controls writing the page number to the
  current device}

  \item{mtext.args}{controls writing the page number to the
  current device}
}
\value{
A dataframe of pdf bookmarks, suitable for passing to
\code{\link{pdf.pdfmark}}
}
\description{
Generate a PDF bookmark, optionally writing a page number
on the current device
}
\details{
The controls for writing the page number to the current
device are stored in the return value and reused for the
next call to pdfmark()
}

