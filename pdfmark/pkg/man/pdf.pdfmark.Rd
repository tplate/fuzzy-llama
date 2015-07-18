\name{pdf.pdfmark}
\alias{pdf.pdfmark}
\title{Add bookmarks to a pdf file using ghostscript}
\usage{
pdf.pdfmark(pdfin, pdfmark, pdfout = NULL, pdfmarkfile = NULL,
  pageformat = "", verbose = TRUE, tmpdir = NULL)
}
\arguments{
  \item{pdfin}{The name of the pdf file to add bookmarks
  to}

  \item{pdfmark}{A dataframe of pdf bookmarks}

  \item{pdfout}{(optional) if supplied, this file will
  contain the output.  If not supplied, pdfin will be
  overwritten}

  \item{pdfmarkfile}{(optional) if supplied, the
  intermediate pdfmarks are written here (and the file is
  not removed)}

  \item{pageformat}{if non-empty, this character string
  format is used for appending page numbers to the titles
  in the table of contents, e.g., ' [\%s]'.}

  \item{verbose}{output verbose info?}

  \item{tmpdir}{where to put temporary files.  Defaults to
  the directory where the output file will be left.}
}
\description{
Add bookmarks to a pdf file using ghostscript
}
\examples{
# No labeling of pages
pdf('tmp1.pdf')
symbols(0,0,circles=1)
symbols(0,0,squares=1)
dev.off()
marks <- pdfmark('circles', page=1)
marks <- pdfmark('squares', page=2, marks=marks)
marks
pdf.pdfmark('tmp1.pdf', marks, pdfout='tmp1b.pdf')
pdf.pdfmark('tmp1.pdf', marks) # overwrite the original file
# Semi automatic page-numbering on contents of PDF file
# Each time pdfmark() is called, it increments the pageno by 1,
# so be careful to synchronize calls with actual pages generated.
pdf('tmp2.pdf')
par(oma=c(2,0,2,0))
symbols(0,0,circles=1)
marks <- pdfmark('circles', pos='bottom', plot=TRUE)
symbols(0,0,squares=1)
marks <- pdfmark('squares', marks=marks, plot=TRUE)
dev.off()
pdf.pdfmark('tmp2.pdf', marks)
}

