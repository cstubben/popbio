\name{hudsonia}
\alias{hudsonia}
\docType{data}
\title{ Projection matrices for mountain golden heather }
\description{
  Projection matrices for the mountain golden heather (\emph{Hudsonia montana}) for the years 1985 through 1988
}
\usage{data(hudsonia)}
\format{
A list of 4 matrices from 1985-1988
}
\details{
    A projection matrix with 6 size classes: seeds, seedlings, and 4 size classes divided by plant area.
}
\source{
   Table 6.7 in Morris and Doak (2002).  The original data is from Frost (1990).

Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
biology: Theory and practice of population viability
analysis. Sinauer, Sunderland, Massachusetts, USA.

}

\examples{
data(hudsonia)
sapply(hudsonia, lambda)

## mean matrix 
x<-mean(hudsonia)
image2(x, mar=c(1,4,5.5,1))
title("Hudsonia mean matrix", line=2.5)
lambda(x)
# variance
var2(hudsonia)
}
\keyword{datasets}
