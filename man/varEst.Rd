\name{varEst}
\alias{varEst}
\title{ Estimate the variance of beta-binomial vital rates using
  approximation method of Akcakaya }
\description{
  This function finds the best estimates of mean and environmental
  variance for beta-binomial vital rates, using the approximation
  method of Akcakaya (2002). 
}
\usage{
varEst(rates, weighted=1)
}

\arguments{
  \item{rates}{a matrix or dataframe with four columns: Rate identifier, Year, Total number of starting
  individuals, Number surviving (or growing)}
  \item{weighted}{either 1 for weighted average demographic variance, or 0 for unweighted average, default is 1.}
}

%\details{ }

\value{
  A matrix with 3 columns: (1) total observed variance,
  (2) estimate of variance due to demographic stochasticity, and
  (3) estimate of variance due to environmental stochasticity.
}
\references{Akcakaya, H. R. 2002. Estimating the variance of survival rates and
  fecundities. Animal Conservation 5: 333-336.

  Kendall, B. E. 1998. Estimating the magnitude of environmental stochasticity
  in survivorship data. Ecological Applications 8(1): 184-193.}

\author{Patrick Nantel, 20 June 2005. Last modified May 1st 2007. }
%\note{}

\seealso{ \code{\link{Kendall}} }

\examples{
data(woodpecker) 
varEst(woodpecker)
}
\keyword{ survey }
