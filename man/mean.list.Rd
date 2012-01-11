\name{mean.list}
\alias{mean.list}
\title{ Calculate mean matrix }
\description{
  Calculates mean matrix from a list of matrices
}
\usage{
\method{mean}{list}(x, \dots)
}
\arguments{
  \item{x}{ A list of two or more matrices }
  \item{\dots}{ Additional arguments passed to \code{\link{rowMeans}} }

}
\details{
   Returns the mean matrix from a list of matrices using a combination
   of \code{\link{unlist}} and \code{\link{rowMeans}}.  See example
   for details.
}
\value{
   The mean matrix
}
%\references{ }
\author{Chris Stubben }
\note{ S3 method for the \code{\link{mean}} of a list of
  matrices. }
\seealso{ \code{\link{var2}}  }
\examples{
data(hudsonia)
mean(hudsonia)
## or
x <- matrix(unlist(hudsonia), ncol=length(hudsonia) )
matrix(rowMeans(x), 6, 6)
}

\keyword{ survey }
