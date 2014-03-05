\name{write.pdfmark}
\alias{write.pdfmark}
\title{Write an R structure of pdf bookmarks to a pdfmark file}
\usage{
write.pdfmark(marks, file = stdout(), pageformat = "")
}
\arguments{
  \item{marks}{a dataframe of pdf bookmarks (see
  \code{\link{pdfmark}})}

  \item{file}{a filename or connection to write to}

  \item{pageformat}{if non-empty, this character string
  format is used for appending page numbers to the titles
  in the table of contents, e.g., ' [\%s]'.}
}
\description{
Write an R structure of pdf bookmarks to a pdfmark file
}
\seealso{
pdfmark
}

