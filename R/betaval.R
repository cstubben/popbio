#' Generate beta-distributed random numbers
#'
#' Calculates a random number from a beta distribution and uses the R function
#' pbeta(x,vv,ww).
#'
#' This function is used by \code{\link{vitalsim}}
#'
#' @param mn mean rate between 0 and 1
#' @param sdev standard deviation
#' @param fx cumulative distribution function, default is a random number
#' between 0 and 1
#'
#' @return a random beta value
#'
#' @source converted Matlab code from Box 8.3 in Morris and Doak (2002)
#'
#' @references Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
#' biology: Theory and practice of population viability analysis. Sinauer,
#' Sunderland, Massachusetts, USA.
#'
#' @seealso Beta Distribution \code{\link{rbeta}}
#'
#' @author Original MATLAB code by Morris and Doak (2002: 277- 278), adapted to
#' R by Patrick Nantel, 20 June 2005
#'
#' @examples
#' betaval(.5, sd=.05)
#' betaval(.5, sd=.05)
#' ## histogram with mean=0.5 and sd=0.05
#' x <- sapply(1:100, function(x) betaval(0.5, 0.05))
#' hist(x, seq(0,1,.025), col="green", ylim=c(0,25), xlab="Value",
#' main="Beta distribution with mean=0.5 and sd=0.05")
#' # generates a graph similar to Figure 8.2 A in Morris & Doak (2002:264)
#' # a much simpler version of BetaDemo in Box 8.3
#' x <- matrix(numeric(3*1000), nrow=3)
#' sd <-c(.05, .25, .45)
#' for (i in 1:3){
#'   for (j in 1:1000){
#'     x[i,j]<-betaval(.5,sd[i])
#'   }
#' }
#' plot(0,0,xlim=c(0,1), ylim=c(0,0.4), type='n', ylab='Frequency',
#' xlab='Value', main="Examples of beta distributions")
#' for (i in 1:3){
#'    h <- hist(x[i,], plot=FALSE, breaks=seq(0,1,.02)  )
#'    lines(h$mids, h$counts/1000, type='l', col=1+i, lwd=2, lty=i)
#' }
#' legend(0.5,0.4, c("(0.50, 0.05)", "(0.50, 0.25)", "(0.50, 0.45)"),
#' lty=1:3, lwd=2, col=2:4, title="mean and sd")
#'
#' @export

betaval <- function(mn, sdev, fx = runif(1)) {
  if (mn > 1 || mn < 0) {
    stop("Please select a mean beta between 0 and 1")
  }
  # Original MATLAB script by Morris & Doak (2002: 277- 278),
  # adapted to R by Patrick Nantel, 20 June 2005.
  if (sdev == 0) {
    bb <- mn
  }
  else {
    toler <- 0.0001 # this is tolerance of answer: how close
    # the CDF value of the answer must be to the input value (Fx)

    # this checks that the input mean and st. deviation
    # are possible for a beta.
    vari <- sdev^2
    if (vari >= (1 - mn) * mn) {
      stop("Standard deviation too high for beta distribution")
    }
    # start with a beginning guess x; the use of runif
    # adds wiggle to the search start to avoid pathologies
    vv <- mn * ((mn * (1 - mn) / (vari)) - 1) # calculate the beta parameters
    ww <- (1 - mn) * ((mn * (1 - mn) / (vari)) - 1)
    upval <- 1
    lowval <- 0
    x <- 0.5 + 0.02 * runif(1)
    i <- pbeta(x, vv, ww) # find the CDF value for x
    # the following while loop searches for ever better
    # values of x, until the value has a CDF within the
    # toler of Fx (unless the value
    # is very close to 0 or 1, which will also terminate
    # the search)
    while ((toler < abs(i - fx)) & (x > 1e-6) & ((1 - x) > 1e-6)) {
      if (fx > i) {
        lowval <- x
        x <- (upval + lowval) / 2
      }
      else {
        upval <- x
        x <- (upval + lowval) / 2
      }
      i <- pbeta(x, vv, ww)
    }
    # This makes values of x somewhat random to eliminate
    # pathologies when variance is very small or large.
    # It also truncates values of x, with the
    # smallest values equal to toler and the biggest
    # equal to 1 - toler.
    bbb <- x + toler * 0.1 * (0.5 - runif(1))
    if (bbb < toler) bbb <- toler
    if (bbb > 1) bbb <- 1 - toler
    bb <- bbb
  }
  bb
}
