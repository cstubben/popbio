\name{head2}
\alias{head2}
\title{ Return the first and last part of a matrix or dataframe}
\description{
  Returns the first and last rows using output from both \code{\link{head}} and
  \code{\link{tail}} and separates the two parts with dots.  Useful for
  viewing ordered datasets such as longitudinal census data.
}
\usage{
head2(x, head = 3, tail = 1, dotrows = 1)
}
\arguments{
  \item{x}{ A matrix or dataframe }
  \item{head}{ The number of first rows }
  \item{tail}{ The number of last rows }
  \item{dotrows}{ The number of rows of dots }
}
%\details{}
\value{
 A smaller object like \code{x} with first and last rows only
}
\author{ Chris Stubben }

\examples{
data(aq.trans)
head2(aq.trans)
}
\keyword{ manip }
