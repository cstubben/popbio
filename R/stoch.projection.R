#' Project stochastic growth from a sequence of matrices
#'
#' Projects stochastic growth using whole matrix selection techniques in an
#' independently and identically distributed (iid) environment from a set of two
#' or more projection matrices
#'
#' converted Matlab code from  Box 7.3 in Morris and Doak (2002) with nmax option
#' added to introduce simple density dependence
#'
#' @param matrices a \code{\link{list}} with two or more projection matrices
#' @param n0 initial population vector
#' @param tmax number of time steps or projection intervals to predict future population size
#' @param nreps number of iterations
#' @param prob a vector of probability weights used by \code{\link{sample}} for
#' selecting the projection matrices, defaults  to equal probabilities
#' @param nmax a maximum number of individuals beyond which population projections
#' cannot exceed. Default is no density dependence
#' @param sumweight A vector of ones and zeros used to omit stage classes when
#' checking density threshold. Default is to sum across all stage classes
#' @param verbose Print comments at start of  iteration 1, 100, 200, 300, etc.
#'
#' @return A matrix listing final population sizes by stage class with one iteration per row.
#'
#' @references Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
#' biology: Theory and practice of population viability analysis. Sinauer,
#' Sunderland, Massachusetts, USA.
#'
#' @author Chris Stubben
#'
#' @examples
#' n <- c(4264, 3,30,16,25,5)
#' names(n) <- c("seed",  "seedlings", "tiny", "small", "medium" , "large")
#' ## use equal and unequal probabilities for matrix selection
#' x.eq   <- stoch.projection(hudsonia, n, nreps=1000)
#' x.uneq <- stoch.projection(hudsonia, n, nreps=1000, prob=c(.2,.2,.2,.4))
#' hist(apply(x.eq, 1, sum), xlim=c(0,5000), ylim=c(0,200), col="green",
#' breaks=seq(0,5000, 100), xlab="Final population size at t=50", main='')
#' par(new=TRUE)
#' ## use transparency for overlapping distributions - may not work on all systems
#' hist(apply(x.uneq, 1, sum), xlim=c(0,5000), ylim=c(0,200), col=rgb(0, 0, 1, 0.2),
#'  xaxt='n', yaxt='n', ylab='', xlab='', breaks=seq(0,10000, 100), main='')
#' legend(2500,200,  c("equal", "unequal"),fill=c("green", rgb(0, 0, 1, 0.2)))
#' title(paste("Projection of stochastic growth for Hudsonia
#' using equal and unequal probabilities"), cex.main=1)
#' ## initial pop size
#' sum(n)
#' abline(v=sum(n), lty=3)
#'
#' @export

stoch.projection <- function(matrices, n0, tmax = 50, nreps = 5000, prob = NULL,
   nmax = NULL, sumweight = rep(1, length(n0)), verbose = FALSE) {
  if (!is.list(matrices)) {
    stop("Please use a list of matrices as input")
  }
  ## initialize empty matrix to store results
  est <- matrix(numeric(nreps * length(n0)), nrow = nreps)
  colnames(est) <- names(n0)
  for (i in 1:nreps)
  {
    # random sample of matrices
    A <- sample(matrices, tmax, replace = TRUE, prob = prob)
    n <- n0
    if (verbose) {
      if (i == 1 || i %% 100 == 0) {
        message("Starting projections for nrep ", i)
      }
    }
    for (j in 1:tmax) {
      n <- A[[j]] %*% n
      ## simple density dependence
      if (!is.null(nmax)) {
        if (sum(n * sumweight) > nmax) n <- n * (nmax / sum(n * sumweight))
      }
    }
    est[i, ] <- n
  }
  est
}
