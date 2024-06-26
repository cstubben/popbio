% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vitalsim.R
\name{vitalsim}
\alias{vitalsim}
\title{Stochastic vital rate simulations}
\usage{
vitalsim(
  vrmeans,
  vrvars,
  corrin,
  corrout,
  makemx,
  n0,
  yrspan,
  Ne = 500,
  tmax = 50,
  runs = 500,
  vrtypes = NULL,
  vrmins = NULL,
  vrmaxs = NULL,
  sumweight = NULL
)
}
\arguments{
\item{vrmeans}{means of vital rates}

\item{vrvars}{variance of vital rates}

\item{corrin}{within year correlation}

\item{corrout}{between year correlations}

\item{makemx}{a function that creates a square projection matrix from a
vector of \code{vrmeans}}

\item{n0}{initial population vector}

\item{yrspan}{the number of years of correlations to build into the M12 matrix}

\item{Ne}{quasi-extinction threshold}

\item{tmax}{latest time to calculate extinction probability, default 50}

\item{runs}{the number of trajectories, default is 500.  1000 is recommended}

\item{vrtypes}{identifies the distribution for each rate in vrmeans where
1 =beta, 2 = stretched beta, 3 = lognormal, default is all ones}

\item{vrmins}{minimum value for each vital rate; use zeros for rates that are
not stretched betas, default is all zeros}

\item{vrmaxs}{maximum value for each vital rate; use zeros for rates that are
not stretched betas, default is all zeros}

\item{sumweight}{a vector of weights, with 0 to omit a class and 1 to include
it when computing the summed density to compare to the quasi-extinction
threshold, default is to include all classes}
}
\value{
Plots a histogram of log stochastic growth rates and the cumulative
probability of quasi-extinction and returns a list with 4 items:
\item{detLambda}{the deterministic population growth rate computed from
the mean matrix}
\item{stochlambda}{the mean stochastic growth rate with 95\% confidence intervals.}
\item{logLambdas}{ a vector of all log stochastic growth rates in first plot}
\item{CDFExt}{a vector of cumulative probabilities of quasi-extinction in second plot}
}
\description{
Calculates the extinction time CDF and stochastic growth rate by sampling vital
rates from a beta, stretched beta, or lognormal distribution and includes
within-year, auto- and cross-correlations
}
\details{
Vital rates used must be either fertility values or binomial probabilities,
i.e., probabilities for events with only two possible outcomes (such as
survival). Means and variances of the vital rates should preferably be
corrected to remove sampling errors and demographic stochasticity.  Note that
this version of the function does not simulate demographic stochasticity and
is density-independent.
}
\note{
The correlation matrices for \emph{Hudsonia} in Morris and Doak 2002
include some correlations > 1.  A corrected set of correlations was sent by
D. Doak on 8/4/2007.  Therefore the results from the simulation below are
different than the book.
}
\examples{
## set vrtypes
hudvrtypes <- c(rep(1,13), rep(3,5), rep(1,6))
## run Full model- using 100 runs here for speed
full <- vitalsim(hudvrs$mean, hudvrs$var, hudcorrs$corrin,
 hudcorrs$corrout, hudmxdef, vrtypes=hudvrtypes,
 n0=c(4264,3,30,16,25,5), yrspan=20 , runs=100)
## deterministic and stochastic lambda
full[1:2]
## log stochastic lambda
log(full$stochLambda)
sd(full$logLambdas)
## SKIP the next two simulations- however, sample output is included for plotting
#NO between year correlations so corrout = diag(0,13)  - all zeros
# no.between <- vitalsim(hudvrs$mean, hudvrs$var, hudcorrs$corrin,
# diag(0,13), hudmxdef, vrtypes=hudvrtypes,
# n0=c(4264,3,30,16,25,5), yrspan=20 )
no.between <- list(CDFExt=c(rep(0,40),0.01,0.04,0.12,0.15,
0.20,0.31,0.49,0.58,0.72,0.78))
#NO correlations so corrout = diag(0,13) AND corrin=diag(13) - ones on diagonal
# no.corr<-vitalsim(hudvrs$mean, hudvrs$var, diag(13),
# diag(0,13), hudmxdef, vrtypes=hudvrtypes,
# n0=c(4264,3,30,16,25,5), yrspan=20 )
no.corr <- list(CDFExt=c(rep(0,39),0.03,0.03,0.06,0.12,0.20,
0.30,0.42,0.52,0.65,0.76,0.83))
## Figure 8.3 with corrected correlation matrices for full model
matplot(cbind(a=full$CDFExt, no.between$CDFExt, no.corr$CDFExt), type='l',
 ylim=c(0,1), lty=1:3, col=2:4, lwd=2, las=1,
 xlab="Years into the future", ylab="Cumulative probability of quasi-extinction")
legend(2,1, c("Full model", "No between-year correlations", "No correlations"),
 lty=1:3, col=2:4, lwd=2)

}
\seealso{
\code{\link{hudmxdef}}, \code{\link{hudvrs}} and \code{\link{hudcorrs}}
}
\author{
Original MATLAB code from Box 8.10 in Morris and Doak (2002).
Adapted to R by Patrick Nantel, 12 July 2005
}
