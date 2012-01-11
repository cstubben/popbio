\name{net.reproductive.rate}
\alias{net.reproductive.rate}

\title{ Net reproductive rate }
\description{
  Calculates the net reproductive rate of a stage classified matrix using the dominant eigenvalue of the matrix R.
}
\usage{
net.reproductive.rate(A, \dots)
}

\arguments{
  \item{A}{ projection matrix }
  \item{\dots}{ additional items are passed to \code{\link{splitA}}
      and are used to split A into T and F matrices }
}
\details{ see section 5.3.4 in Caswell (2001). }
\value{
  Net reproductive rate.  If the transition matrix is singular, then
  NA is returned.
}
\references{ Caswell, H. 2001. Matrix population models: construction, analysis, and interpretation, Second edition. Sinauer, Sunderland, Massachusetts, USA. }
\author{ Chris Stubben }
\note{ Previous versions required separate T and F matrices as input}

\seealso{ see \code{\link{fundamental.matrix}} and \code{\link{generation.time}} for other age-specific traits }
\examples{
data(whale)
net.reproductive.rate(whale)
## fertilities in last column
data(teasel)
net.reproductive.rate(teasel, r=1:6, c=6)
## Plot 3 from Calathea - values are not the same as p. 105 in Caswell.
data(calathea)
sapply(calathea[9:12], net.reproductive.rate)

}
\keyword{ survey }

