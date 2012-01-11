\name{countCDFxt}
\alias{countCDFxt}
\title{ Count-based extinction probabilities and bootstrap
  confidence intervals  }
\description{
  This function takes parameters derived from population counts and
  calculates the probability of extinction with bootstrap confidence
  intervals for a density-independent model, using a diffusion approximation.
}
\usage{
countCDFxt(mu, sig2, nt, Nc, Ne, tq=nt, tmax=50, Nboot=500, plot=TRUE)
}
\arguments{
  \item{mu }{ estimated value of mean mu}
  \item{sig2}{estimated value of sample variance }
  \item{nt}{number of transitions in the data set}
  \item{Nc}{current population size}
  \item{Ne }{quasi-extinction threshold}
  \item{tq}{length of the census (in years), default is number of
      transitions }
  \item{tmax}{latest time to calculate extinction probability, default
    50}
  \item{Nboot}{ number of bootstrap samples for calculating confidence intervals
   for extinction probabilities, default 500)}
  \item{plot }{draw extinction time CDF plot with log-scale on y-axis }
}
%\details{  }
\value{
  The function plots the cumulative probabilities of quasi-extinction through
  time with 95\% confidence intervals. It also returns a data frame with
  the extinction time CDF for the best parameter estimates (Gbest), and the
  lower and upper bootstrap confidence limits for extinction probabilites (Glo, Gup).
}

\source{converted Matlab code from  Box 3.4 in Morris and Doak (2002)}

\references{ Dennis et al. 1991, Ecological Monographs 61: 115-143.

Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
biology: Theory and practice of population viability analysis.
Sinauer, Sunderland, Massachusetts, USA. }

\author{ Adapted to R by Patrick Nantel, 4 May 2005,
         from program 'extprob' of Morris \& Doak (2002: 79-86) }

%\note{}
\seealso{  \code{\link{extCDF}} }

\examples{
## plot like Figure 3.8 in Morris and Doak (2002).
data(grizzly)
logN<-log(grizzly$N[-1]/grizzly$N[-39])
countCDFxt(mu=mean(logN), sig2=var(logN), nt=38, tq=38, Nc=99, Ne=20)
}

\keyword{survey }
