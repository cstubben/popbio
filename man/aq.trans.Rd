\name{aq.trans}
\alias{aq.trans}

\docType{data}
\title{ Annual transition data for Aquilegia in the southwestern US}
\description{
  Transition data listing stages and fates from \emph{Aquilegia chrysantha}  in Fillmore Canyon,
  Organ Mountains, New Mexico, 1996-2003.
}
\usage{data(aq.trans)}
\format{
  A data frame with 1637 observations on the following 9 variables.
  \describe{
    \item{\code{plot}}{Plot number}
    \item{\code{year}}{Staring year of census}
    \item{\code{plant}}{Plant id number}
     \item{\code{stage}}{Initial stage class with ordered factor
     levels \code{seed} < \code{recruit} < \code{small} < \code{large}
     < \code{flower}.   }
    \item{\code{leaf}}{Total number of leaves}
   \item{\code{rose}}{Total number of rosettes}
    \item{\code{fruits}}{Total number of mature fruits}
    \item{\code{fate}}{Final stage class or fate with levels
    \code{seed} < \code{recruit} < \code{small} < \code{large} <
    \code{flower} < \code{dead}}
    \item{\code{rose2}}{Final number of rosettes}
  }
}
\details{
    The five stage classes include seeds in the seed bank, new recruits or
     seedlings, small vegetative plants with 1 rosette, large
     vegetative plants with 2 or more rosettes, and flowering plants.
     Stage classes were assigned to census plants
    using a combination of status and size data recorded in the
    field.  See \code{demo(stage.classify)} for more details.
}
\source{Data set owners: Brook Milligan, Chris Stubben, Allan Strand}
%\references{}
\seealso{ \code{\link{aq.census}} }
\examples{
data(aq.trans)
head(aq.trans,3)

sv<-table(aq.trans$stage, aq.trans$year)
addmargins(sv)
stage.vector.plot(sv[-1,], prop=FALSE, main="Aquilegia stage vectors")

## plot proportions with barplot
## use xpd to draw legend outside plot boundaries
op<-par(mar=c(5,4,4,1), xpd=TRUE)
x<-barplot(prop.table(sv[-1,],2), las=1,
xlab="Year", ylab="Proportion in stage class", 
col=rainbow(4), ylim=c(0,1), xaxt='n', space=.5)
yrs<-substr(colnames(sv),3,4)
axis(1,x, yrs)
legend(2.7,1.25, rev(rownames(sv)[-1]), fill=rev(rainbow(4)), bty='n', ncol=2)
par(op)


}
\keyword{datasets}

