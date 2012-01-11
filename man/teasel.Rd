\name{teasel}
\alias{teasel}
\docType{data}
\title{Projection matrix for teasel}
\description{
  Projection matrix for the plant teasel
}
\usage{data(teasel)}
\format{
  A projection matrix 
}
\source{
   Example 5.2
}
\references{
Caswell, H. 2001. Matrix population models: construction, analysis,
and interpretation, Second edition. Sinauer, Sunderland,
Massachusetts, USA.
}
\examples{
data(teasel)
image2(teasel, mar=c(1,3.5,5,1) , box.offset=.1)
 title("Teasel projection matrix", line=2.5)
# fertilities for a monocarpic plant in a prebreeding census in last column
splitA(teasel,  r=1:6, c=6)
lambda(teasel)



}
\keyword{datasets}
