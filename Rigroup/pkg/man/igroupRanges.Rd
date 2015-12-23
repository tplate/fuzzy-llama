\name{igroupRanges}
\alias{igroupRanges}
\concept{groups}
\concept{grouping}
\concept{subsets}
\concept{by}
\concept{range}
\title{calculate ranges for small integer groups}
\description{
  This function allows a user to quickly calculate the range for 
  each small integer group of a data vector .
}
\usage{
igroupRanges(x,i,na.rm=TRUE)
}
\arguments{
  \item{x}{A numeric, integer, or logical data vector}
  \item{i}{A small integer vector indicating group membership 1:ngroups}
  \item{na.rm}{if TRUE remove NAs before use (defaults to TRUE)}
}
\details{
  This package provides a fast implementation for calculating
  ranges for each group in a data vector where a small integer 
  vector (1:number of groups) indicates group membership.
  na.rm is used to determine how NAs are handled.
  The return value is a vector with length(number of groups).
}
\value{
  igroupRanges:  Returns a vector with length equal to the number of groups.
}
\seealso{
  \code{\link{igroupAlls}}
  \code{\link{igroupAnys}}
  \code{\link{igroupCounts}}
  \code{\link{igroupMaxs}}
  \code{\link{igroupMeans}}
  \code{\link{igroupMins}}
  \code{\link{igroupProds}}
  \code{\link{igroupSums}}
}
\references{ None }
\author{K. Hendricks with lots of help from B. Dunlap}
\examples{
x <- rnorm(100)
i <- rep(1:25,4)
rngs <- igroupRanges(x,i)
rngs
}
\keyword{utilities}
