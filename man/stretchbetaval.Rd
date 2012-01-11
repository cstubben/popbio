\name{stretchbetaval}
\alias{stretchbetaval}
\title{ Generate stretched beta-distributed random numbers
 }
\description{Generate a stretched beta number with
 mean, standard deviation, minimum and maximum
 values and CDF value for bounded fertility estimates
}
\usage{
stretchbetaval(mn, std, minb, maxb, fx)
}
\arguments{
  \item{mn}{ mean of a fertility rate }
  \item{std}{ standard deviation}
  \item{minb}{ minimum value  }
  \item{maxb}{ maximum value  }
  \item{fx}{ Cumulative Distribution Function value }
}
\details{
  This function calls function 'betaval'.
}
\value{
 Returns a stretched beta number with
 mean mn, standard deviation std, minimum and maximum
 values (minb, maxb) and CDF value fx.
}

\source{converted Matlab code from Box 8.5 in Morris and Doak (2002)}

\references{ Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
biology: Theory and practice of population viability analysis.
Sinauer, Sunderland, Massachusetts, USA. }

\author{Original MATLAB script by Morris & Doak (2002: 283).
Adapted to R by Patrick Nantel, 11 July 2005. }

\seealso{ \code{\link{betaval}} }
\examples{
stretchbetaval(3, 1.2, 1, 20, runif(1))

 # Generates stretchbeta random
 # fertilities for a population of 1000 mature individuals (Ni) with mean
 # fertility (f) of 3.0 and inter-individual variance in fertility (varF) of 1.5.
 
 Ni   <- 1000
 f    <-    2.5
 varF <-    1
 fmin <- 1
 fmax <- 5
 rndfert<-numeric(Ni) 
for(i in 1:Ni)
{
   rndfert[i]  <- stretchbetaval(f, sqrt(varF), fmin, fmax, runif(1)) 
}
 hist(rndfert,20, main="Stretched beta-distributed random fertilities", 
xlab="Fertility rate", , col="blue") 
}
\keyword{ survey }
