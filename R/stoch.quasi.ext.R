#' Quasi-extinction threshold
#'
#' Estimate the quasi-extinction probability by simulation for a structured
#' population in an an independently and identically distributed stochastic
#' environment
#'
#' converted Matlab code from  Box 7.5 in Morris and Doak (2002)
#'
#' @param matrices a \code{\link{list}} with two or more projection matrices, or
#' a matrix with one projection matrix per column, with elements filled by columns
#' @param n0 initial population vector
#' @param Nx quasi-extinction threshold
#' @param tmax number of time steps or projection intervals
#' @param maxruns number of times to simulate cumulative distribution function
#' @param nreps number of iterations
#' @param prob a vector of probability weights used by \code{\link{sample}} for
#' selecting the projection matrices
#' @param sumweight A vector of ones and zeros used to omit stage classes when
#' checking quasi-extinction threshold.  Default is to sum across all stage classes
#' @param verbose Print comment at start of run 1,2,3,etc.
#'
#' @return A matrix with quasi-extinction probabilities for each run by columns
#'
#' @references Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
#' biology: Theory and practice of population viability analysis. Sinauer,
#' Sunderland, Massachusetts, USA.
#'
#' @seealso \code{\link{stoch.projection}}
#'
#' @author Chris Stubben
#'
#' @examples
#' n <- c(4264, 3,30,16,25,5)
#' names(n) <- c("seed",  "seedlings", "tiny", "small", "medium" , "large")
#' ## exclude seeds using sumweight.  Using 100 nreps for speed
#' x <- stoch.quasi.ext(hudsonia, n, Nx=10, nreps=100, sumweight=c(0,1,1,1,1,1))
#' matplot(x, xlab="Years", ylab="Quasi-extinction probability",
#'  type='l', lty=1, col=rainbow(10), las=1,
#'  main="Time to reach a quasi-extinction threshold
#' of 10 above-ground individuals")
#'
#' @export

stoch.quasi.ext <- function(matrices, n0, Nx, tmax = 50, maxruns = 10, nreps = 5000,
   prob = NULL, sumweight = NULL, verbose = TRUE) {
  if (is.list(matrices)) {
    matrices <- matrix(unlist(matrices), ncol = length(matrices))
  }
  x <- length(n0)
  if (is.null(sumweight)) {
    sumweight <- rep(1, x)
  }
  y <- dim(matrices)[2]
  ext <- matrix(numeric(maxruns * tmax), ncol = maxruns)
  for (h in 1:maxruns)
  {
    if (verbose) {
      message("Calculating extinction probability for run ", h)
    }
    prob.ext <- numeric(tmax)
    for (i in 1:nreps)
    {
      n <- n0
      for (t in 1:tmax)
      {
        col <- sample(1:y, 1, prob = prob)
        A <- matrix(matrices[, col], nrow = x)
        n <- A %*% n
        N <- sum(sumweight * round(n))
        if (N < Nx) {
          prob.ext[t] <- prob.ext[t] + 1
          break
        }
      }
    }
    prob.ext <- cumsum(prob.ext / nreps)
    ext[, h] <- prob.ext
  }
  ext
}
