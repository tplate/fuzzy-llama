\name{igroupAlls}
\alias{igroupAlls}
\concept{groups}
\concept{grouping}
\concept{subsets}
\concept{by}
\concept{all}
\concept{logical AND}
\title{calculates logical All for small integer groups of logical vectors}
\description{
  This function allows a user to quickly calculate the logical All for 
  each small integer group of a logical vector .
}
\usage{
igroupAlls(x,i,na.rm=TRUE)
}
\arguments{
  \item{x}{A logical data vector}
  \item{i}{A small integer vector indicating group membership 1:ngroups}
  \item{na.rm}{if TRUE remove NAs before use (defaults to TRUE)}
}
\details{
  This package provides a fast implementation for calculating
  Alls for each group in a logical data vector where a small integer 
  vector (1:number of groups) indicates group membership.
  na.rm is used to determine how NAs are handled.
  The return value is a vector with length(number of groups).
}
\value{
  igroupAlls:  Returns a vector with length equal to the number of groups.
}
\seealso{
  \code{\link{igroupAnys}}
  \code{\link{igroupCounts}}
  \code{\link{igroupMaxs}}
  \code{\link{igroupMeans}}
  \code{\link{igroupMins}}
  \code{\link{igroupProds}}
  \code{\link{igroupRanges}}
  \code{\link{igroupSums}}
}
\references{ }
\author{K. Hendricks with lots of help from B. Dunlap}
\examples{
y <- rnorm(100)
i <- rep(1:25,4)
x <- (y > 1.0)
alls <- igroupAlls(x,i)
alls
}
\keyword{utilities}
