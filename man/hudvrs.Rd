\name{hudvrs}
\alias{hudvrs}
\docType{data}
\title{  Best Kendall estimates of Hudsonia vital rate means and variances}
\description{
  Best Kendall estimates of vital rate means (9 growth, 4
    survival, and 11 fertility rates) for \emph{Hudsonia montana}.
}
\usage{data(hudvrs)}
\format{
  A data frame with 24 observations on the following 2 variables.
  \describe{
    \item{\code{mean}}{ vital rate means}
    \item{\code{var}}{ vital rate variances}
  }
}
%\details{ }
\source{
  Data  listed in Box 8.10 for the
  \code{\link{vitalsim}} function.  See also Table 8.5 in Morris and
  Doak (2002).
}
\references{
  Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
biology: Theory and practice of population viability analysis.
Sinauer, Sunderland, Massachusetts, USA.
}


\examples{
data(hudvrs)
hudvrs

hudmxdef(hudvrs$mean)
}
\keyword{datasets}
