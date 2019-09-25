#' Calculate population growth rates by projection
#'
#' Calculates the population growth rate and stable stage distribution by
#' repeated projections of the equation \eqn{n(t+1)=An(t)}
#'
#' Eventually, structured populations will convergence to a stable stage
#' distribution where each new stage vector is changing by the same proportion (lambda).
#'
#' @param A A projection matrix
#' @param n An initial age or stage vector
#' @param iterations Number of iterations
#'
#' @return A list with 5 items
#'   \item{lambda}{Estimate of lambda using change between the last two population counts}
#'   \item{stable.stage}{Estimate of stable stage distribution using proportions in last stage vector}
#'   \item{stage.vector}{A matrix with the number of projected individuals in each stage class}
#'   \item{pop.sizes}{Total number of projected individuals}
#'   \item{pop.changes}{Proportional change in population size}
#'
#' @references see section 2.2 in Caswell 2001
#'
#' @seealso \code{\link{stage.vector.plot}} to plot stage vectors
#'
#' @author Chris Stubben
#'
#' @examples
#' ## mean matrix from Freville et al 2004
#' stages <- c("seedling", "vegetative", "flowering")
#' A <- matrix(c(
#'     0,     0,  5.905,
#' 0.368, 0.639,  0.025,
#' 0.001, 0.152,  0.051
#' ), nrow=3, byrow=TRUE,
#'     dimnames=list(stages,stages))
#' n <- c(5,5,5)
#' p <- pop.projection(A,n, 15)
#' p
#' damping.ratio(A)
#' stage.vector.plot(p$stage.vectors, col=2:4)
#' data(whale)
#' A <- whale
#' #n <- c(4,38,36,22)
#' n <- c(5,5,5,5)
#' p <- pop.projection(A,n, 15)
#' p
#' stage.vector.plot(p$stage.vectors, col=2:4, ylim=c(0, 0.6))
#' ## convergence is slow with damping ratio close to 1
#' damping.ratio(A)
#' pop.projection(A, n, 100)$pop.changes
#'
#' @export

pop.projection <- function(A, n, iterations = 20) {
  x <- length(n)
  t <- iterations
  stage <- matrix(numeric(x * t), nrow = x) ## initialize a matrix to store projected stage vectors
  pop <- numeric(t) ## and numeric vectors for projected population size
  change <- numeric(t - 1) ## and proportional changes in pop sizes

  for (i in 1:t)
  {
    stage[, i] <- n
    pop[i] <- sum(n)
    if (i > 1) {
      change[i - 1] <- pop[i] / pop[i - 1]
    } ## calculates proportional changes in pop size
    n <- A %*% n ## multiply matrix A by size vector n and
  } ## set n equal to new vector


  rownames(stage) <- rownames(A) ## and add row names.
  colnames(stage) <- 0:(t - 1) ## start counting at zero?
  w <- stage[, t] ## Estimate stable stage from last size iteration

  pop.proj <- list(
    lambda = pop[t] / pop[t - 1], ## Change between the LAST two population counts
    stable.stage = w / sum(w),
    stage.vectors = stage,
    pop.sizes = pop,
    pop.changes = change
  )
  pop.proj
}
