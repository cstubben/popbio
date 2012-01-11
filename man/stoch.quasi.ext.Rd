\name{stoch.quasi.ext}
\alias{stoch.quasi.ext}
\title{ Calculate quasi-extinction threshold }
\description{
  Estimate the quasi-extinction probability by simulation for a
  structured population in an an independently and identically
  distributed stochastic environment

}
\usage{
stoch.quasi.ext(matrices, n0, Nx, tmax = 50, maxruns = 10, nreps = 5000, 
                         prob = NULL, sumweight = NULL, verbose=TRUE)
}
\arguments{
  \item{matrices}{ a \code{\link{list}} with two or more projection
      matrices, or a matrix with one projection matrix per column,
      with elements filled by columns }
  \item{n0}{ initial population vector }
  \item{Nx}{ quasi-extinction threshold  }
  \item{tmax}{ number of time steps or projection intervals  }
  \item{maxruns}{ number of times to simulate cumulative distribution function  }
  \item{nreps}{ number of iterations. }
  \item{prob}{ a vector of probability weights used by
      \code{\link{sample}} for selecting the projection matrices.}
  \item{sumweight}{ A vector of ones and zeros used to omit stage
      classes when checking quasi-extinction threshold.  Default is to
      sum across all stage classes. }
  \item{verbose}{ Print comment at start of run 1,2,3,etc.}
}
%\details{}
\value{
  A matrix with quasi-extinction probabilities for each run by columns
}
\source{ converted Matlab code from  Box 7.5 in Morris and Doak (2002) }
\references{ 
Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
biology: Theory and practice of population viability
analysis. Sinauer, Sunderland, Massachusetts, USA.
}
\author{ Chris Stubben }

\seealso{\code{\link{stoch.projection}} }
\examples{
data(hudsonia)
n<-c(4264, 3,30,16,25,5)
names(n)<-c("seed",  "seedlings", "tiny", "small", "medium" , "large")
## exclude seeds using sumweight
x<-stoch.quasi.ext(hudsonia, n, Nx=10, nreps=500, sumweight=c(0,1,1,1,1,1))
matplot(x, xlab="Years", ylab="Quasi-extinction probability", 
 type='l', lty=1, col=rainbow(10), las=1,
 main="Time to reach a quasi-extinction threshold 
of 10 above-ground individuals")
}

\keyword{ survey }
