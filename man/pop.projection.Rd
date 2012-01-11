\name{pop.projection}
\alias{pop.projection}
\title{ Calculate population growth rates by projection }
\description{
  Calculates the population growth rate and stable stage distribution by repeated projections of the equation \code{n(t+1)=An(t)}.
}
\usage{
pop.projection(A,n,iterations=20)
}

\arguments{
  \item{A}{ A projection matrix  }
  \item{n}{ An initial age or stage vector }
  \item{iterations}{ Number of iterations }

}
\details{  Eventually, structured populations will convergence to a stable stage distribution where each new stage vector is changing by the same proportion (lambda).  

}
\value{
A list with 5 items
  \item{lambda }{Estimate of lambda using change between the last two population counts}
  \item{stable.stage }{Estimate of stable stage distribution using proportions in last stage vector}
   \item{stage.vector}{A matrix with the number of projected individuals in each stage class}
    \item{pop.sizes}{Total number of projected individuals}
    \item{pop.changes}{Proportional change in population size}
 
}
\references{see section 2.2 in Caswell 2001

}
\author{ Chris Stubben }

\seealso{ \code{\link{stage.vector.plot}} to plot stage vectors}
\examples{
## mean matrix from Freville et al 2004
stages<-c("seedling", "vegetative", "flowering")
A<-matrix(c(
    0,     0,  5.905,
0.368, 0.639,  0.025,
0.001, 0.152,  0.051
), nrow=3, byrow=TRUE, 
    dimnames=list(stages,stages) 
)

n<-c(5,5,5)
p<-pop.projection(A,n, 15)
p
damping.ratio(A)
stage.vector.plot(p$stage.vectors, col=2:4)


####

data(whale)
A<-whale
#n<-c(4,38,36,22)
n<-c(5,5,5,5)
p<-pop.projection(A,n, 15)
p
stage.vector.plot(p$stage.vectors, col=2:4, ylim=c(0, 0.6))
## convergence is slow with damping ratio close to 1
damping.ratio(A)
pop.projection(A,n, 100)$pop.changes
}
\keyword{ survey }
