\name{QPmat}
\alias{QPmat}
\title{ Build a projection matrix from a time series of individuals (or densities) per stage. }
\description{
This function builds one projection matrix from a time series of number
(or densities) of individuals per stage (size classes or life stages) using
Wood's quadratic programming method. The matrix model also requires a constraint matrix C, vector b, and vector
listing nonzero elements of desired projection matrix.
}
\usage{
QPmat(nout, C, b, nonzero)
}
\arguments{
  \item{nout}{A time series of population vectors}
  \item{C}{C constraint matrix}
  \item{b}{b vector }
  \item{nonzero}{indices of the non-zero elements of the transition
      matrix (counting by column)}
}


%\details{ }

\value{
  A projection matrix.
}

\source{converted Matlab code from Example 6.3 in Caswell (2001)}


\references{Caswell, H. 2001. Matrix population models. Construction, Analysis
and interpretation. 2nd ed. Sinauer, Sunderland, Massachusetts.}

\author{Original Matlab code in Caswell (2001: 148).
Adapted to R by Patrick Nantel}

\note{This function uses package 'quadprog', which should be 
installed and loaded on the user's system.
}



\examples{
data(nematode)
## list nonzero elements
nonzero<- c( 1, 2, 5, 6, 7, 9)
## create C matrix
C<- rbind(diag(-1,6), c(1,1,0,0,0,0), c(0,0,1,1,0,0), c(0,0,0,0,0,1))
## calculate b (transpose is not necessary - either way works)
b<-apply(C, 1, max)
QPmat(nematode, C,b,nonzero)
}

\keyword{ survey }

