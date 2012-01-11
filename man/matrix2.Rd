\name{matrix2}
\Rdversion{1.1}
\alias{matrix2}
\title{ Square matrices
}
\description{  Create a square matrix from a given set of values}
\usage{
matrix2(x, stages, byrow = TRUE)
}
\arguments{
  \item{x}{ a vector of matrix elements }
  \item{stages}{ a vector of row names (also  assigned to columns) }
  \item{byrow}{ fill matrix by rows , default TRUE}
}
%\details{ }
\value{ a square matrix }
%\references{ }
\author{ Chris Stubben}
%\note{ }


\seealso{ \code{\link{matrix}} }
\examples{
#Centaurea corymbosa from Freville 2004
ceco<-c(0,0,5.905,0.368,0.639, 0.025, 0.001, 0.152, 0.051)
stages <- c("seedling", "vegetative", "flowering")
# shortcut for
#matrix(ceco, nrow=3, byrow=TRUE,  dimnames=list(stages,stages))
matrix2(ceco, stages)


}
\keyword{ methods}
