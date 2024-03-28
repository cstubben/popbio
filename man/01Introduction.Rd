\name{01.Introduction}
\alias{01.Introduction}
\alias{popbio}
\title{Introduction to the popbio Package}
\description{
\code{Popbio} is a package for the construction and analysis of matrix
population models.   First, the package consists  of the \R translation of
\code{Matlab} code found in Caswell (2001) or Morris and Doak
(2002).  A list of converted functions within each book can be accessed
using \code{help(Caswell)} and \code{help(Morris)} within R,  or by following
the links to  \link{02.Caswell} and \link{03.Morris} from the help content pages.

Second, the \code{popbio} package includes functions to estimate vital
rates and construct projection matrices from raw census data typically
collected in plant demography studies. In these studies, vital rates
can often be estimated directly from annual censuses of tagged
individuals using transition frequency tables.  To estimate vital
rates in animal demography using capture-recapture methods, try the
\code{Rcapture} or \code{mra} package instead.

Finally, the package includes plotting methods and sample datasets
consisting of either published projection matrices or annual census
data from demography studies.  Three sample demonstrations
illustrate some of the package capabilities (\code{Caswell, fillmore
and stage.classify}).  A description of the package
in the Journal of Statistical Software is available at
\url{https://www.jstatsoft.org/article/view/v022i11}.
}

\references{
To cite the popbio package in publications, type
\code{citation('popbio')}.  For details on matrix population models, see

Caswell, H. 2001. Matrix population models: Construction,
analysis, and interpretation, Second edition. Sinauer, Sunderland,
Massachusetts, USA.

Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
biology: Theory and practice of population viability
analysis. Sinauer, Sunderland, Massachusetts, USA.

}


\author{Chris Stubben}

\keyword{documentation}
