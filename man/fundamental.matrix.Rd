\name{fundamental.matrix}
\alias{fundamental.matrix}

\title{ Fundamental matrix and age-specific survival }
\description{

Age-specific survival calculations from stage-classified matrices.  Includes the mean, variance and coefficient of variation (cv) of the time spent in each stage class and the mean and variance of the time to death

}
\usage{
fundamental.matrix(A, \dots)
}
\arguments{
  \item{A}{ projection matrix }
  \item{\dots}{ additional items are passed to \code{\link{splitA}}
      and are used to split A into T and F matrices }
}
\details{
 see section 5.3.1 in Caswell (2001).
}
\value{
  A list with 5 items
  \item{ N }{ fundamental matrix or mean of the time spent in each stage class}
 \item{ var }{ variance of the time spent in each stage class}
 \item{ cv }{ coefficient of variation (sd/mean) }
 \item{ meaneta }{ mean of time to death}
 \item{ vareta }{ variance of time to death }
}
\references{ Caswell, H. 2001. Matrix population models: construction, analysis, and interpretation, Second edition. Sinauer, Sunderland, Massachusetts, USA. }
\author{ Chris Stubben }
%\note{ }
\seealso{ see \code{\link{generation.time}} and \code{\link{net.reproductive.rate}} for other age-specific traits }
\examples{

data(whale)
fundamental.matrix(whale)

}
\keyword{ survey }
