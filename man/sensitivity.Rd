\name{sensitivity}
\alias{sensitivity}

\title{ Sensitivity analysis of a projection matrix }
\description{
 Calculate the sensitivities of eigenvalues to changes in the
 projection matrix elements
}
\usage{
sensitivity(A, zero=FALSE)
}

\arguments{
  \item{A}{ A projection matrix }
  \item{zero}{ Set sensitivities for unobserved transitions to zero,
      default is false }
}
\details{ see section 9.1 in Caswell (2001). }

%\note{ }

\value{
 A sensitivity matrix 
}
\references{
Caswell, H. 2001. Matrix population models: construction, analysis,
and interpretation, Second edition. Sinauer, Sunderland,
Massachusetts, USA.

 }
\author{  Chris Stubben }

\seealso{ \code{\link{elasticity}}  }
\examples{
data(teasel)

sens<-sensitivity(teasel)
## IMAGE plot with smaller boxes 
image2(sens, mar=c(1,3.5,5,1), box.offset=.1)
 title("Sensitivity matrix using image2", line=2.5)
## MATPLOT
matplot2(sens, log='y', type='b', yaxt='n', ltitle="Fate",
ylab=expression(paste("Sensitivity of ",lambda)), 
main="Sensitivity matrix using matplot2")
pwrs<- -4:1
axis(2, 10^pwrs, parse(text=paste("10^", pwrs, sep = "")), las=1)



}
\keyword{ survey }
