\name{Kendall}
\alias{Kendall}
\title{ Find the best Kendall's estimates of mean and environmental
variance for beta-binomial vital rates.}
\description{
This function finds the best estimates of mean and environmental
variance for beta-binomial vital rates,
using a brute force search for the best adjusted
estimates from a very large number of combinations of different
possible mean and variance values.
}
\usage{
Kendall(rates, grades=1000, maxvar=0.2,minvar=0.00001, maxmean=1, minmean=0.01)
}

\arguments{
  \item{rates}{a matrix or dataframe with four columns: Rate identifier, Year, Total number of starting
  individuals, Number growing (or surviving).}
  \item{grades}{number of different levels of means and
  variances to try, default is 1000 }
  \item{maxvar}{maximum variance to search over, default is 0.20. The maximum
  ever possible is 0.25 and searching a narrower range will improve the
  accuracy of the answer.}
  \item{minvar}{minimum variance to search, default is 0.00001. }
  \item{maxmean}{maximum limit on the mean values to search, default 1}
  \item{minmean}{minimum limit on the mean values to search, default 0.01 }
}
\value{
  A list with estimates and confidence intervals
\item{est}{a matrix with 5 columns: (1) estimated mean,
  (2) Kendall's MLE mean, (3) estimated variance, (4) Kendall's MLE
  variance, (5) Kendall's unbiased MLE variance.}
\item{ci}{ a matrix with  95\% confidence limits for the
    Kendall's mean and unbiased variance estimates with 4 columns:
  (1) low and (3) high mean limits, (3) low and (4) high variance limits.}
}

\source{converted Matlab code from  Box 8.2 in Morris and Doak (2002)}


\references{Kendall, B. E. 1998. Estimating the magnitude of environmental stochasticity
  in survivorship data. Ecological Applications 8(1): 184-193.

  Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
  biology: Theory and practice of population viability analysis.
  Sinauer, Sunderland, Massachusetts, USA.}
\author{Adapted to R from Morris \& Doak (2002: 267-270) by Patrick Nantel.}
\note{
Note that it may deliver warning messages of :
'no finite arguments to min; returning Inf', indicating use of very low
values for variance, but this is not a malfunction.
}
\seealso{  \code{\link{varEst}} }

\examples{
## desert tortoise input from Box 8.2 - compare results to Table 8.3
tor<-data.frame(rate=rep(c("g4","g5","g6"),each=3),
year=rep(1:3,3),      ## representing 70s, early 80s, late 80s
start=c(17,15,7,22,19,4,32,31,10),
grow=c(8,1,0,5,5,0,2,1,0))
## use fewer grades for faster loop
tor.est<-Kendall(tor, grades=200)
tor.est

data(woodpecker) 
wp.est <- Kendall(woodpecker, grades=200)
wp.est
}
\keyword{ survey }
