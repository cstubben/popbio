\name{calathea}
\alias{calathea}
\docType{data}
\title{  Projection matrices for a tropical understory herb}
\description{
 Projection matrices for a tropical understory herb (\emph{Calathea
   ovandensis}) for plots 1-4 and years 1982-1985 and the pooled matrix 
}
\usage{data(calathea)}
\format{
  A list of 17 matrices ordered by plot then year, with the pooled
  matrix last.

}
\details{
   A projection matrix constructed using a post-breeding census with 8 size classes: seed, seedling, juvenile,
   pre-reproductive, and 4 reproductive classes divided by leaf area. 
}
\source{
  Table 7 in Horvitz and Schemske (1995).  The pooled matrix is from
  Table 8.
}
\references{
  Horvitz, C.C. and D.W. Schemske. 1995.  Spatiotemporal variation in
  demographic transitions of a tropical understory herb: Projection
  matrix analysis.  Ecological Monographs 65:155-192.
}
\examples{
data(calathea)
## Single matrix
calathea[[11]]
image2(calathea[[11]], text.cex=.8)
title( paste("Calathea", names(calathea[11])), line=3)

## MEAN matrix (exclude pooled matrix)
mean(calathea[-17])

## all plot 1
calathea[1:4]
## all 1982 matrices 
calathea[ grep("1982", names(calathea)) ]
# OR
# calathea[seq(1,16,4)]
# split(calathea, 1:4)[[1]]

## Growth rates -see Figure 7
x<-sapply(calathea[-17], lambda)
x<-matrix(x, nrow=4, byrow=TRUE, dimnames= list(paste("plot", 1:4), 1982:1985))
x
matplot2(x, type='b', ylab='Growth rate', main='Calathea growth rates')


}
\keyword{datasets}
