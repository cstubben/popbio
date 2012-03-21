\name{stoch.projection}
\alias{stoch.projection}

\title{ Simulate stochastic growth from a sequence of matrices
}
\description{
  Simulates stochastic growth by projection using whole matrix selection techniques in an independently and identically distributed (iid) environment from a set of 2 or more projection matrices
}
\usage{
stoch.projection(matrices, n0, tmax = 50, nreps = 5000, prob = NULL, 
                           nmax = NULL, sumweight = rep(1, length(n0)), verbose=FALSE)
}
\arguments{
  \item{matrices}{ a \code{\link{list}} with two or more projection matrices }
   \item{n0}{ initial population vector }
  \item{tmax}{ number of time steps or projection intervals to predict future population size }
  \item{nreps}{ number of iterations }
  \item{prob}{  a vector of probability weights used by \code{\link{sample}} for selecting the projection matrices, defaults  to equal probabilities}
  \item{nmax}{ a maximum number of individuals beyond which population projections cannot exceed.  Default is no density dependence}
  \item{sumweight}{ A vector of ones and zeros used to omit stage
  classes when checking density threshold.  Default is to sum across
  all stage classes}
  \item{verbose}{ Print comments at start of  iteration 1, 100, 200,
  300, etc.}
}
%details{}
\value{
  A matrix listing final population sizes by stage class with one iteration per row.
}

\source{converted Matlab code from  Box 7.3 in Morris and Doak
(2002) with nmax option added to introduce simple density dependence }

\references{ 
Morris, W. F., and D. F. Doak. 2002. Quantitative conservation biology: Theory and practice of population viability analysis. Sinauer, Sunderland, Massachusetts, USA. }
\author{ Chris Stubben }

\examples{
data(hudsonia)
n<-c(4264, 3,30,16,25,5)
names(n)<-c("seed",  "seedlings", "tiny", "small", "medium" , "large")

### use equal and unequal probabilities for matrix selection 
x.eq<-stoch.projection(hudsonia, n, nreps=1000)
x.uneq<-stoch.projection(hudsonia, n, nreps=1000, prob=c(.2,.2,.2,.4))

hist(apply(x.eq, 1, sum), xlim=c(0,5000), ylim=c(0,200), col="green", 
breaks=seq(0,5000, 100), xlab="Final population size at t=50", main='')

par(new=TRUE)

## use transparency for overlapping distributions - may not work on all systems
hist(apply(x.uneq, 1, sum), xlim=c(0,5000), ylim=c(0,200), col = rgb(0, 0, 1, 0.2), 
xaxt='n', yaxt='n', ylab='', xlab='', 
breaks=seq(0,10000, 100), main='')

legend(2500,200,  c("equal", "unequal"),fill=c("green", rgb(0, 0, 1, 0.2)))
title(paste("Projection of stochastic growth for Hudsonia
using equal and unequal probabilities"), cex.main=1)

## initial pop size  
sum(n)
abline(v=sum(n), lty=3)



}
\keyword{ survey }
