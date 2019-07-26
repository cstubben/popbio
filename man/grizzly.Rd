\name{grizzly}
\alias{grizzly}
\docType{data}
\title{ Population sizes of grizzly bears in Yellowstone from 1959-1997 }
\description{
  Estimated number of adult female grizzly bears in the Greater
  Yellowstone population from 1959-1997.
}
\usage{data(grizzly)}
\format{
  A data frame with 39 observations on the following 2 variables.
  \describe{
    \item{\code{year}}{ Year of census}
    \item{\code{N}}{ Estimated number of female grizzlies}
  }
}
\details{
  The grizzly bear data set is used in count based PVAs in chapter 3
  in Morris and Doak 2002.

}
\source{
  Table 3.1 in Morris and Doak 2002. Original data from Eberhardt et
  al. 1986 and Haroldson 1999.
}
\references{
  Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
biology: Theory and practice of population viability analysis.
Sinauer, Sunderland, Massachusetts, USA.
}
\examples{
data(grizzly)
grizzly
## plot like Fig 3.6 (p. 66)
plot(grizzly$year, grizzly$N, type='o', pch=16, las=1, xlab="Year",
  ylab="Adult females", main="Yellowstone grizzly bears")
## calcualte  log(Nt+1/Nt)
nt <- length(grizzly$N)  ## number transitions
logN <- log(grizzly$N[-1]/grizzly$N[-nt])
## Mean and var
c(mean=mean(logN), var=var(logN))
## or using linear regression
## transformation for unequal variances (p. 68)
x <- sqrt(grizzly$year[-1]-grizzly$year[-length(grizzly$year)])
y <- logN/x
mod <- lm(y ~ 0 + x )
summary(mod)
## plot like Fig 3.7
plot(x, y, xlim=c(0,1.2), ylim=c(-.3,.3), pch=16, las=1,
  xlab=expression(paste("Sqrt time between censuses ", (t[t+1]-t[i])^{1/2})),
  ylab=expression(log(N[t+1]/N[t]) / (t[t+1]-t[i])^{1/2}) ,
  main=expression(paste("Estimating ", mu, " and ", sigma^2, " using regression")))
abline(mod)
## MEAN (slope)
mu <- coef(mod)
## VAR (mean square in analysis of variance table)
sig2 <- anova(mod)[["Mean Sq"]][2]
c(mean= mu , var= sig2)
## Confidence interval for mean  (page 72)
confint(mod,1)
## Confidence interval for sigma 2  (equation 3.13)
df1 <- length(logN) - 1
df1 * sig2 / qchisq(c(.975, .025), df= df1)
## test for outliers using dffits (p.74)
dffits(mod)[dffits(mod) > 2 * sqrt(1/38) ]
## plot like  fig 3.11
plot(grizzly$N[-nt], logN, pch=16, xlim=c(20,100), ylim=c(-.3, .3),las=1,
  xlab="Number of females in year T",
  ylab=expression(log(N[t+1]/N[t])),
  main="Grizzly log population growth rates")
cor(grizzly$N[-nt], logN)
abline(lm(logN ~ grizzly$N[-nt]), lty=3 )
}
\keyword{datasets}
