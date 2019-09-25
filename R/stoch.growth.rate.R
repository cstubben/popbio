#' Log stochastic growth rate
#'
#' Calculates the log stochastic growth rate by Tuljapukar's approximation and by simulation
#'
#' converted Matlab code from  Box 7.4 in Morris and Doak (2002)
#'
#' @param matrices a \code{\link{list}} with two or more projection matrices, or
#' a matrix with one projection matrix per column, with elements filled by columns
#' @param prob a vector of probability weights used by \code{\link{sample}} for
#' selecting the projection matrices, defaults to equal probabilities
#' @param maxt number of time intervals, default 50000
#' @param verbose Print comment at start of time 1, 10000, 20000, etc.
#'
#' @return A list with 3 items
#'   \item{approx}{ log stochastic growth rate by  Tuljapukar's approximation }
#'   \item{sim}{ log stochastic growth rate by simulation }
#'   \item{sim.CI}{ confindence interval for simulation}
#'
#' @references Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
#' biology: Theory and practice of population viability analysis. Sinauer,
#' Sunderland, Massachusetts, USA.
#'
#' @seealso \code{\link{stoch.projection}} to output population sizes from simulation
#'
#' @author Chris Stubben
#'
#' @examples
#' sgr <- stoch.growth.rate(hudsonia)
#' sgr
#' exp(sgr$approx)
#'
#' @export

stoch.growth.rate <- function(matrices, prob = NULL, maxt = 50000, verbose = TRUE) {
  if (is.list(matrices)) {
    matrices <- matrix(unlist(matrices), ncol = length(matrices))
  }
  s <- sqrt(dim(matrices)[1]) ## number of stage classes
  n <- dim(matrices)[2] ## number of matrixes
  # default equal probabilities
  if (is.null(prob)) {
    prob <- rep(1 / n, n)
  }
  Abar <- numeric(s^2)
  Exy <- numeric(s^4)
  ## for each matrix, add values to Abar and Exy weighted by probabilities in prob
  for (i in 1:n)
  {
    A <- matrices[, i]
    Exy <- Exy + prob[i] * kronecker(A, A)
    Abar <- Abar + prob[i] * A
  }
  ## Covariance matrix
  C <- (Exy - kronecker(Abar, Abar)) * n / (n - 1)
  C <- matrix(C, nrow = s^2)
  Abar <- matrix(Abar, nrow = s)
  ## code from eigen.analysis for lambda and Sensitivity matrix
  ev <- eigen(Abar)
  lmax <- which(Re(ev$values) == max(Re(ev$values)))
  lambda <- Re(ev$values[lmax])
  W <- ev$vectors
  w <- abs(Re(W[, lmax]))
  V <- Conj(solve(W))
  v <- abs(Re(V[lmax, ]))
  S <- v %o% w
  ##  Simulation
  r <- numeric(maxt)
  n0 <- w
  for (t in 1:maxt)
  {
    if (verbose) {
      if (t == 1 || t %% 10000 == 0) {
        message("Calculating stochastic growth at time ", t)
      }
    }
    col <- sample(1:n, 1, prob = prob)
    A <- matrix(matrices[, col], nrow = s)
    n0 <- A %*% n0
    N <- sum(n0)
    r[t] <- log(N)
    n0 <- n0 / N
  }
  loglsim <- mean(r)
  dse <- 1.96 * sqrt(var(r) / maxt)
  CI <- c(loglsim - dse, loglsim + dse)
  ## Tuljapurkar approximation
  Svec <- matrix(S, ncol = 1)
  tau2 <- t(Svec) %*% C %*% Svec
  loglams <- log(lambda) - tau2 / (2 * lambda^2)
  ## output...
  stoch <- list(
    approx = as.numeric(loglams),
    sim = loglsim,
    sim.CI = CI
  )
  stoch
}
