\name{multiresultm}
\alias{multiresultm}
\title{ Incorporate demographic stochasticity into population projections}
\description{
  This function generates multinomial random numbers for state transitions
  and lognormal or binomial (for clutch size=1) random numbers for
  fertilities  and returns a vector of the number of individuals per
  stage class at \emph{t+1}.
}
\usage{
multiresultm(n, T, F, varF=NULL)
}
\arguments{
  \item{n}{the vector of numbers of individuals per class at t }
  \item{T}{ a transition T matrix}
  \item{F}{ a fertility F matrix }
  \item{varF}{ a matrix of inter-individual variance in fertilities,
      default is NULL for simulating population where clutch size = 1, 
      so that fertilities give the probabilities of birth.}
}
%\details{}
\value{
 The function returns a vector of the number of individuals per class at t+1.
}

\source{Adapted from Matlab code in Box 8.11 in Morris and Doak (2002)
  and section 15.1.3 in Caswell (2001)}


\references{Caswell, H. 2001. Matrix population models. Construction, Analysis
and interpretation. 2nd ed. Sinauer, Sunderland, Massachusetts. 

Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
biology: Theory and practice of population viability analysis.
Sinauer, Sunderland, Massachusetts, USA.

}

\author{Adapted to R by Patrick Nantel.}

%\seealso{ \code{\link{vitalsim}}}
\examples{
data(whale)
x<-splitA(whale)
whaleT<-x$T
whaleF<-x$F

multiresultm(c(1,9,9,9),whaleT, whaleF)
multiresultm(c(1,9,9,9),whaleT, whaleF)

## create graph similar to Fig 15.3 a
reps <- 10    # number of trajectories
tmax <- 200   # length of the trajectories
totalpop <- matrix(0,tmax,reps)  # initializes totalpop matrix to store trajectories
nzero <- c(1,1,1,1) # starting population size
for (j in 1:reps) 
{
   n <- nzero
   for (i in 1:tmax) 
   {
      n <- multiresultm(n,whaleT,whaleF)
      totalpop[i,j] <- sum(n)
   } 
} 
matplot(totalpop, type = 'l', log="y",
        xlab = 'Time (years)', ylab = 'Total population')
}
\keyword{survey}
