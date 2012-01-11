\name{extCDF}
\alias{extCDF}
\title{ Count-based extinction time cumulative distribution function }
\description{
  Returns the extinction time cumulative distribution function using
  parameters derived from population counts.
}
\usage{
extCDF(mu, sig2, Nc, Ne, tmax = 50)
}
\arguments{
  \item{mu}{ estimated value of mean mu }
  \item{sig2}{ estimated value of sample variance }
  \item{Nc}{ current population size }
  \item{Ne}{ quasi-extinction threshold }
  \item{tmax}{ latest time to calculate extinction probability, default
    50 }
}
%\details{}
\value{
 A vector with the cumulative probabilities of quasi-extinction from
 t=0 to t=tmax.
}

\source{converted Matlab code from  Box 3.3 and equation 3.5 in Morris and Doak
  (2002) }

\references{ Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
biology: Theory and practice of population viability analysis.
Sinauer, Sunderland, Massachusetts, USA. }
\author{ Chris Stubben }
%\note{ }
\seealso{ \code{\link{countCDFxt}} for bootstrap confidence intervals}
\examples{
data(grizzly)
logN<-log(grizzly$N[-1]/grizzly$N[-39])
mu<-mean(logN)
sig2<-var(logN)
## grizzly cdf (log scale)
ex<-extCDF(mu, sig2, Nc=99, Ne=20)
plot(ex, log='y', type='l', pch=16, col="blue", yaxt='n',
xlab="Years", ylab="Quasi-extinction probability",
main="Yellowstone Grizzly bears")
pwrs<-seq(-15,-5,5)
axis(2, at = 10^pwrs, labels=parse(text=paste("10^", pwrs, sep = "")),
las=1)
##plot like fig 3.10  (p 90)
n<-seq(20, 100, 2)
exts<-numeric(length(n))
for (i in 1:length(n) )
{
   ex<-extCDF(mu, sig2, Nc=n[i], Ne=20)
   exts[i]<-ex[50]
}
plot(n, exts, type='l', las=1,
xlab="Current population size",
ylab="Probability of quasi-extinction by year 50")
}
\keyword{ survey }
