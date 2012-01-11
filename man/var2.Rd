\name{var2}
\alias{var2}
\title{ Calculate a variance matrix }
\description{
  Calculates the variances from a list of matrices
}
\usage{
var2(x)
}
\arguments{
  \item{x}{ A list of two or more matrices }
}
\details{
   Returns a matrix containing variances from a list of matrices
   using a combination of \code{\link{unlist}} and \code{\link{apply}}.
}
\value{
   A matrix containing variances
}
%\references{ }
\author{Chris Stubben }
%\note{  }
%\seealso{  }
\examples{
data(hudsonia)
var2(hudsonia)

}

\keyword{ survey }
