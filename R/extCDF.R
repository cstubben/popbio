#' Count-based extinction time cumulative distribution function
#'
#' Returns the extinction time cumulative distribution function using
#' parameters derived from population counts.
#'
#' converted Matlab code from Box 3.3 and equation 3.5 in Morris and Doak 2002
#'
#' @param mu estimated value of mean mu
#' @param sig2 estimated value of sample variance
#' @param Nc current population size
#' @param Ne quasi-extinction threshold
#' @param tmax latest time to calculate extinction probability, default 50
#'
#' @return A vector with the cumulative probabilities of quasi-extinction from
#' t=0 to t=tmax.
#'
#' @references Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
#' biology: Theory and practice of population viability analysis. Sinauer,
#' Sunderland, Massachusetts, USA.
#'
#' @seealso \code{\link{countCDFxt}} for bootstrap confidence intervals
#'
#' @author Chris Stubben
#'
#' @examples
#' data(grizzly)
#' logN <- log(grizzly$N[-1]/grizzly$N[-39])
#' mu <- mean(logN)
#' sig2 <- var(logN)
#' ## grizzly cdf (log scale)
#' ex <- extCDF(mu, sig2, Nc=99, Ne=20)
#' plot(ex, log='y', type='l', pch=16, col="blue", yaxt='n',
#' xlab="Years", ylab="Quasi-extinction probability",
#' main="Yellowstone Grizzly bears")
#' pwrs <- seq(-15,-5,5)
#' axis(2, at = 10^pwrs, labels=parse(text=paste("10^", pwrs, sep = "")), las=1)
#' ##plot like fig 3.10  (p 90)
#' n <- seq(20, 100, 2)
#' exts <- numeric(length(n))
#' for (i in 1:length(n) ){
#'    ex <- extCDF(mu, sig2, Nc=n[i], Ne=20)
#'    exts[i] <- ex[50]
#' }
#' plot(n, exts, type='l', las=1,
#' xlab="Current population size",
#' ylab="Probability of quasi-extinction by year 50")
#'
#' @export

extCDF <- function(mu, sig2, Nc, Ne, tmax = 50) {
  ## some functions from box 3.3 (erf is matlab error function)
  erf <- function(y) 2 * pnorm(y * sqrt(2)) - 1
  phi <- function(z) 0.5 * (1 + (erf(z / sqrt(2))))
  # log distance from the quasi-extinction threshold
  d <- log(Nc / Ne)
  G <- numeric(tmax)
  for (x in 1:tmax)
  {
    G[x] <- phi((-d - mu * x) / sqrt(sig2 * x)) +
      exp(-2 * mu * d / sig2) * phi((-d + mu * x) / sqrt(sig2 * x))
  }
  G
}
