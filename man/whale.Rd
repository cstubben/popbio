\name{whale}
\alias{whale}
\docType{data}
\title{Projection matrix for killer whale}
\description{
  Projection matrix for killer whales
}
\usage{data(whale)}
\format{
  A projection matrix. 
}
\source{
  Projection matrix from Example 5.1 in Caswell (2001) 

}
\references{
Caswell, H. 2001. Matrix population models: construction, analysis,
and interpretation, Second edition. Sinauer, Sunderland,
Massachusetts, USA.
}

\examples{
data(whale)
whale
splitA(whale)
lambda(whale)
sensitivity(whale)
# plot sensitivity 
matplot2(sensitivity(whale), type='b', legend='topleft', ltitle='Fate',
main='Killer Whale sensitivity')


}
\keyword{datasets}
