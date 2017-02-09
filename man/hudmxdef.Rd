\name{hudmxdef}
\alias{hudmxdef}

\title{ Matrix definition program for Hudsonia vital rates }
\description{
  Creates a projection matrix from \emph{Hudsonia} vital rates (survival,
  growth, and reproduction).  Growth rates are defined as a set of
  binomial choices as in Table 8.4 B in Morris and Doak (2002).
}
\usage{
hudmxdef(vrs)
}
\arguments{
  \item{vrs}{ Vital rate means in \code{\link{hudvrs}} }
}
%\details{  Used by the program \code{\link{vitalsim}} }
\value{
  A  projection matrix
}
\source{  Original MATLAB in Morris and Doak 2002 }

\references{ 
Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
biology: Theory and practice of population viability analysis.
Sinauer, Sunderland, Massachusetts, USA.

 }
%\author{  }

\seealso{  \code{\link{vitalsim}} }
\examples{
data(hudvrs)
hudmxdef(hudvrs$mean)
}

\keyword{ survey }
