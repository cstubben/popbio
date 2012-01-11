\name{betaval}
\alias{betaval}
\title{ Generate beta-distributed random numbers }
\description{
  This function calculates a random number
  from a beta distribution and uses the R function pbeta(x,vv,ww).
}
\usage{
betaval(mn, sdev, fx=runif(1))
}

\arguments{
  \item{mn}{ mean rate between 0 and 1}
  \item{sdev}{ standard deviation }
  \item{fx}{cumulative distribution function, default is a random
  number between 0 and 1}
}
\details{
  This function is used by \code{\link{vitalsim}} .
}
\value{
 Returns a random beta value
}
\source{converted Matlab code from  Box 8.3 in Morris and Doak (2002)
}

\references{ Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
biology: Theory and practice of population viability analysis.
Sinauer, Sunderland, Massachusetts, USA. }

\author{Original MATLAB code by Morris and Doak (2002: 277- 278),
 adapted to R by Patrick Nantel, 20 June 2005.}

%\note{}
\seealso{Beta Distribution \code{\link{rbeta}} }

\examples{

betaval(.5, sd=.05)
betaval(.5, sd=.05)

## histogram with mean=0.5 and sd=0.05
x<-numeric(100)
for (i in 1:100)
{
   x[i]<-betaval(.5,.05)
}
hist(x, seq(0,1,.025), col="green", ylim=c(0,25), xlab="Value",
main="Beta distribution with mean=0.5 and sd=0.05")

# generates a graph similar to Figure 8.2 A in Morris & Doak (2002:264)
# a much simpler version of BetaDemo in Box 8.3


x<-matrix(numeric(3*1000), nrow=3)
sd <-c(.05, .25, .45)
for (i in 1:3)
{
  for (j in 1:1000)
  {
    x[i,j]<-betaval(.5,sd[i])
  }
}
plot(0,0,xlim=c(0,1), ylim=c(0,0.4), type='n', ylab='Frequency',
xlab='Value', main="Examples of beta distributions")
for (i in 1:3)
{
   h<-hist(x[i,], plot=FALSE, breaks=seq(0,1,.02)  )
   lines(h$mids, h$counts/1000, type='l', col=1+i, lwd=2, lty=i)
}
legend(0.5,0.4, c("(0.50, 0.05)", "(0.50, 0.25)", "(0.50, 0.45)"),
lty=1:3, lwd=2, col=2:4, title="mean and sd")
}

\keyword{ survey }
