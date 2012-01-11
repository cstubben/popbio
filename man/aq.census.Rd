\name{aq.census}
\alias{aq.census}
\docType{data}
\title{ Annual census data for Aquilegia in the southwestern US}
\description{
  Demography census data from \emph{Aquilegia chrysantha}  in Fillmore Canyon,
  Organ Mountains, New Mexico, 1996-2003.
}
\usage{data(aq.census)}
\format{
  A data frame with 2853 observations on the following 8 variables.
  \describe{
    \item{\code{plot}}{Plot number}
    \item{\code{year}}{Year of census}
    \item{\code{plant}}{Plant id number}
    \item{\code{status}}{Plant status recorded in field: dead,
    dormant, recruit0 (with cotyledons only), recruit1, flowering or vegetative. }
    \item{\code{rose}}{Total number of rosettes}
    \item{\code{leaf}}{Total number of leaves}
    \item{\code{infl}}{Total number of infloresences or flowering stalks}
    \item{\code{fruits}}{Total number of mature fruits}
  }
}
\details{  This sample data set includes census data from 10 of the 15 total
demography plots established in 1995.  Please contact the data set owners to access the
complete census data from 1995-2006.


}
\source{ Data set owners: Brook Milligan, Chris Stubben, Allan Strand}
%\references{}
\seealso{ \code{\link{aq.trans}} for annual transitions with stage and fate in same row }
\examples{
data(aq.census)
sv<-table(aq.census$status, aq.census$year)
sv
stage.vector.plot(sv[-1,], prop=FALSE)
}
\keyword{datasets}

