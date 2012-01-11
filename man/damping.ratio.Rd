\name{damping.ratio}
\alias{damping.ratio}

\title{ Damping ratio }
\description{
 Calculates the damping ratio of a projection matrix
}
\usage{
damping.ratio(A)
}

\arguments{
  \item{A}{ A projection matrix }

}
\details{  see section 4.7 in Caswell (2001).
 }

\note{ The damping ratio is calculated by dividing the dominant
  eigenvalue by the eigenvalue with the second largest magnitude.   }

\value{ Damping ratio }

\references{
Caswell, H. 2001. Matrix population models: construction, analysis,
and interpretation, Second edition. Sinauer, Sunderland,
Massachusetts, USA.

 }
\author{ Chris Stubben }

\seealso{ \code{\link{lambda}}  }
\examples{

## whale converges slowly to stable stage distribution
data(whale)
matplot2(pop.projection(whale, c(1,1,1,1), 60)$stage.vectors,
prop=TRUE, legend=NA, 
main=paste("whale damping ratio = ", round(damping.ratio(whale),3) ) )

# Calathea - compare to Table 12 in Horvitz and Schemske (1995)
data(calathea)
x<-sapply(calathea[-17], damping.ratio)
x<-matrix(x, nrow=4, byrow=TRUE, dimnames= list(paste("plot", 1:4), 1982:1985))
x
matplot2(x, type='b', ylab="Damping ratio", main="Calathea")

}
\keyword{ survey }
