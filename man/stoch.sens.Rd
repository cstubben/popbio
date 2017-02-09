\name{stoch.sens}
\alias{stoch.sens}

\title{ stoch.sens }
\description{
  Calculates the sensitivity of the stochastic growth rate to
  perturbations in the mean demographic projection matrix.
}
\usage{
stoch.sens(A, tlimit=100)
}

\arguments{

  \item{A}{ a list of matrices }
\item{tlimit}{ time limit, default 100 }

}
\details{ Function copied from demogR package since it was removed from CRAN.
See section 14.4.1 in Caswell 2001.

}


\value{
  A list with two elements:

  \item{sensitivities }{ sensitivities of the stochastic growth rate }
  \item{elasticities }{ elasticities of the stochastic growth rate }
  
}
\references{ 

Caswell, H. 2001. Matrix population models: Construction, analysis,
and interpretation. 2nd ed. Sunderland, MA: Sinauer. 

Haridas, C. V., and S. Tuljapurkar. 2005. Elasticities in variable
environments: Properties and implications. American Naturalist 166
(4):481-495. 

Tuljapurkar, S. 1990. Population dynamics in variable environments.
Vol. 85, Lecture notes in biomathematics. Berlin: Springer-Veralg.

Tuljapurkar, S., and C. V. Haridas. 2006. Temporal autocorrelation and
stochastic population growth. Ecology Letters 9 (3):324-334. 

}

\author{ James Holland Jones}


\seealso{ \code{\link{eigen.analysis}} }

\examples{

data(hudsonia)
stoch.sens(hudsonia)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ algebra }

