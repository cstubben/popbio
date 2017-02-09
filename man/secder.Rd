\name{secder}
\alias{secder}

\title{secder }
\description{
  Calculates the second derivatives of the dominant eigenvalue of the
  demographic projection matrix for all non-zero transitions with
  respect to one specified transition.
}
\usage{
secder(A, k, l)
}

\arguments{
  \item{A}{  projection matrix }
  \item{k}{ row index for the specified transition }
  \item{l}{ column index for the specified transition }
}
\details{
Function copied from demogR package since it was removed from CRAN.
See section 9.7 in Caswell 2001. 
}
\value{

  A square matrix of the same rank as A where each element \eqn{s_ij} is the
  second derivative of the dominant eigenvalue of A, \eqn{\partial^2
  \lambda/\partial a_{ij} \partial a_{kl}}.

}
\references{  

Caswell, H. 1996. Second derivatives of population growth rate:
Calculation and applications. Ecology 77 (3):870-879.

Caswell, H. 2001. Matrix population models: Construction, analysis,
and interpretation. 2nd ed. Sunderland, MA: Sinauer.
}
\author{  James Holland Jones }


\note{

The eigenvalue second derivatives are essential for calculating both
perturbation analyses of the eigenvalue elasticities and stochastic
sensitivities.  \code{secder} is used in functions to calculate both
these quantities.

}

\seealso{  \code{\link{eigen.analysis}}  } 
\examples{

## eigenvalue second derivatives of the US projection matrix from 1967
## with respect to infant survival

x1 <- c(0, 0.0010478, 0.0820086, 0.2884376, 0.3777064, 
  0.2647110, 0.1405144, 0.0585568, 0.0134388, 0.0003327)
x2 <- diag(c(0.9972036, 0.9983625, 0.9978063, 0.9967535, 
  0.9961039, 0.9948677, 0.9923658, 0.9885968, 0.9828676))

usa <- rbind(x1, cbind(x2,0))
sd21 <- secder(usa,2,1)
sd21

}


\keyword{ algebra }

