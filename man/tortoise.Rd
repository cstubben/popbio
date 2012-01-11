\name{tortoise}
\alias{tortoise}
\docType{data}
\title{ Projection matrices for desert tortoise}
\description{
   Projection matrices for the desert tortoise \emph{Gopherus agassizii}
}
\usage{data(tortoise)}
\format{
 A list of 4 projeciton matrices with 4 different fertility estimates (low, medium low, medium high, and high)

}
\source{
   Table 5 in Doak et al (1994).  Used by Caswell (2001) in chapter 9 on sensitivity analysis.
}
\references{
Caswell, H. 2001. Matrix population models: construction, analysis,
and interpretation, Second edition. Sinauer, Sunderland,
Massachusetts, USA.

Doak, D., P. Kareiva, and B. Kleptetka. 1994. Modeling population
viability for the desert tortoise in the Western Mojave
Desert. Ecological Applications 4:446-460.

}
\examples{
data(tortoise)
A<-tortoise[["med.high"]]
# log color scale not needed
image2(A, mar=c(1,3.5, 5,1), log=FALSE, box.off=.1)
title("Tortoise projection matrix", line=3)

splitA(A)
lambda(A)
sapply(tortoise, lambda)



}

\keyword{datasets}
