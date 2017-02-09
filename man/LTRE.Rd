\name{LTRE}
\alias{LTRE}
\title{ Life Table Response Experiment }
\description{
  Function to evaluate sensitivities in a fixed Life Table Response Experiment (LTRE).
}
\usage{
LTRE(trts, ref)
}
\arguments{
  \item{trts}{ A treatment matrix or a list of two or more treatment matrices }
  \item{ref}{ A reference matrix }
}
\details{
 Sensitivities are evaluated midway between the treatment and
 reference matrices as described in section 10.1.1 in Caswell (2001).
}
\value{
  A matrix of contributions (equation 10.4 in Caswell) or a list of
  matrices with one matrix of contributions per treatment

}
\references{ 
Caswell, H. 2001. Matrix population models: construction, analysis,
and interpretation, Second edition. Sinauer, Sunderland,
Massachusetts, USA.


 }
\author{ Chris Stubben }
\note{ The examples of a fixed LTRE are from 

Horvitz, C. C., D. W. Schemske, and H. Caswell. 1997. The
relative importance of life-history stages to population
growth: prospective and retrospective analyses. Pages 247-271 in S. Tuljapurkar and H. Caswell, editors. Structured
population models in marine, terrestrial and freshwater systems.
Chapman and Hall, New York.

A.L. Angert. 2006. Demography of central and marginal
populations of monkeyflowers (Mimulus cardinalis and
M. lewisii). Ecology 87:2014-2025.

Check the \code{demo(Caswell)} for variance decomposition in a random
design using killer whale.


}
% \seealso{   }
\examples{
#######  Calathea ovandensis
data(calathea)
calathea_pool<-calathea[['pooled']]

## Create plots like FIGURE 7 in Horvitz et al 1997
##PLOTS
plots<- split(calathea[-17], rep(1:4,each=4))
## use Mean matrix since pooled not available by plot
plots<- lapply(plots, mean)
Cm<-LTRE(plots, calathea_pool)
pe<-sapply(Cm, sum)
barplot(pe, xlab="Plot", ylab="Plot effect" , ylim=c(-.25, .25),
col="blue", las=1)
abline(h=0)
box()
title(expression(italic("Calathea ovandensis")))

##YEARS -- split recycles vector
yrs<-split(calathea[-17], 1:4)
yrs <- lapply(yrs, mean)
names(yrs)<-1982:1985
Cm<-LTRE(yrs, calathea_pool)
ye<-sapply(Cm, sum)
barplot(ye, xlab="Year", ylab="Year effect" , ylim=c(-.25, .25), col="blue", las=1)
abline(h=0)
box()
title(expression(italic("Calathea ovandensis")))

## INTERACTION
Cm<-LTRE(calathea[-17], calathea_pool)
ie<-sapply(Cm, sum)
## minus plot, year effects
ie<- ie - rep(pe, each=4) - rep(ye, 4)
names(ie)<-NULL
names(ie)[seq(1,16,4)]<-1:4
barplot(ie, xlab="Plot (years 82-83 to 85-86)", ylab="Interaction effect" , 
  ylim=c(-.25, .25), col="blue", las=1)
abline(h=0)
box()
title(expression(italic("Calathea ovandensis")))


#######  Mimulus 
## Pooled M. cardinalis reference matrix kindly provided by Amy Angert 1/2/2008.
m_card_pool<-matrix( c(
1.99e-01, 8.02e+02, 5.82e+03, 3.05e+04,
2.66e-05, 7.76e-02, 2.31e-02, 1.13e-03,
7.94e-06, 8.07e-02, 3.22e-01, 2.16e-01,
2.91e-07, 1.58e-02, 1.15e-01, 6.01e-01), byrow=TRUE, nrow=4)

## Population effects using pooled population matrices 
data(monkeyflower)
card<-subset(monkeyflower,  species=="cardinalis" & year=="pooled")
## split rows into list of 4 matrices 
Atrt<-lapply(split(as.matrix(card[,4:19]), 1:4),  matrix, nrow=4, byrow=TRUE)
names(Atrt)<-card$site
Cm<-LTRE(Atrt, m_card_pool)
x<-sapply(Cm, sum)
x
names(x)<-c("BU", "RP", "WA", "CA")

## Plot like Figure 2A in Angert (2006)
op<-par(mar=c(5,5,4,1))
barplot(x, xlab="Population", ylab="", xlim=c(0,6.5), ylim=c(-.4, .4), 
  las=1, space=.5, col="blue")
abline(h=0)
mtext(expression(paste(sum(a[ij]), " contributions")), 2, 3.5)
title(expression(paste(italic("M. cardinalis"), " Population effects")))
box()

## and Plot like Figure 3A
x<-matrix(unlist(Cm), nrow=4, byrow=TRUE)
colnames(x)<-paste("a", rep(1:4, each=4), 1:4, sep="")
bp<-barplot(x[1:2,], beside=TRUE, ylim=c(-.2,.2), las=1,
xlab="Transition", ylab="", xaxt='n')
mtext(expression(paste("Contribution of ", a[ij], "to variation in ", lambda)), 2, 3.5)
## rotate labels
text(bp[1,]-0.5, -.22, labels=colnames(x), srt=45, xpd=TRUE)
title(expression(paste(italic("M. cardinalis"), " Range center")))
box()
par(op)


}
\keyword{ survey }

