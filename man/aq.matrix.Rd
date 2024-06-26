% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aq.matrix.R
\name{aq.matrix}
\alias{aq.matrix}
\title{Create a projection matrix for Aquilegia}
\usage{
aq.matrix(
  trans,
  recruits,
  summary = TRUE,
  seed.survival = 0.126,
  seed.bank.size = 10000,
  seeds.per.fruit = 120,
  ...
)
}
\arguments{
\item{trans}{A data frame with transitions listing \code{\link{ordered}}
stages and fates and counts of mature fruits}

\item{recruits}{The number of observed recruits in year \code{t + 1}.}

\item{summary}{Output projection matrix and summaries.  Otherwise output
transition table with added individual fertilities.}

\item{seed.survival}{Estimated seed survival rate for both new seeds and seed
bank.  Default is 12.6 percent survival.}

\item{seed.bank.size}{Estimated size of the seed bank.  Seed bank and new
seeds contribute to a common germinant pool with equal chance for
germination. Default is 10,000 seeds in seed bank.}

\item{seeds.per.fruit}{The number of seeds produced per mature fruit.
Default is 120 seeds.}

\item{\dots}{additional arguments passed to \code{\link{projection.matrix}}}
}
\value{
If summary is TRUE, a list with
  \item{recruits}{total number of recruits}
  \item{seed.survival}{seed survival rate}
  \item{seed.bank}{total number of seeds in seed bank }
  \item{seeds.from.plants}{ total number of new seeds just released from fruits }
  \item{recruitment.rate}{ recruitment rate calculated as recruits/(seed.bank.size + seeds.from.plants)}
  \item{A}{projection matrix}
  \item{lambda}{population growth rate}
  \item{n}{initial population vector}
  \item{n1}{final population vector}
If summary is FALSE, a data frame with individual fertilities added
  to the transition data frame only.
}
\description{
Creates a projection matrix for \emph{Aquilegia} from annual transition data,
assuming new seeds and seed bank seeds have an equal chance for successful
germination and equal survival rates.
}
\details{
Adds individual fertilites to annual transitions using a prebreeding census.
}
\examples{
x <- subset(aq.trans, year==1996)
## number of recruits in 1997
rec <- nrow(subset(aq.trans, year==1997 & stage == "recruit"))
aq.matrix(x, recruits=rec)
aq.matrix(x, recruits=rec, seed.survival=.7, seed.bank=3000)

}
\seealso{
\code{\link{projection.matrix}}
}
\author{
Chris Stubben
}
