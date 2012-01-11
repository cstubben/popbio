\name{reproductive.value}
\alias{reproductive.value}

\title{ Stable stage distribution }
\description{
 Calculates the reproductive values of a projection matrix
}
\usage{
reproductive.value(A)
}

\arguments{
  \item{A}{ A projection matrix }

}
\details{  see section 4.5 in Caswell (2001).
 }

%\note{   }

\value{ A vector containing the reproductive values  scaled so v[1]=1 }

\references{
Caswell, H. 2001. Matrix population models: construction, analysis,
and interpretation, Second edition. Sinauer, Sunderland,
Massachusetts, USA.

 }
\author{ Chris Stubben }

%\seealso{   }
\examples{

data(teasel)
v<-reproductive.value(teasel)
v
dotchart(log10(v), pch=16, xlab="Reproductive value (log10)")

}
\keyword{ survey }
