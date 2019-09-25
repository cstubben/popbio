#' Second derivatives of the dominant eigenvalue
#'
#' Calculates the second derivatives of the dominant eigenvalue of the
#' demographic projection matrix for all non-zero transitions with respect to
#' one specified transition
#'
#' Function copied from demogR package after it was removed from CRAN.
#' See section 9.7 in Caswell 2001.
#'
#' @param A projection matrix
#' @param k row index for the specified transition
#' @param l column index for the specified transition
#'
#' @return A square matrix of the same rank as A where each element \eqn{s_ij}
#' is the second derivative of the dominant eigenvalue of A.
#'
#' @note The eigenvalue second derivatives are essential for calculating both
#' perturbation analyses of the eigenvalue elasticities and stochastic
#' sensitivities.
#'
#' @seealso \code{\link{eigen.analysis}}
#'
#' @references Caswell, H. 2001. Matrix population models: construction,
#' analysis, and interpretation, Second edition. Sinauer, Sunderland,
#' Massachusetts, USA.
#'
#' Caswell, H. 1996. Second derivatives of population growth rate:
#' Calculation and applications. Ecology 77 (3):870-879.
#'
#' @author James Holland Jones
#'
#' @examples
#' ## eigenvalue second derivatives of the US projection matrix from 1967
#' ## with respect to infant survival
#' x1 <-   c(0, 0.0010478, 0.0820086, 0.2884376, 0.3777064,
#'   0.2647110, 0.1405144, 0.0585568, 0.0134388, 0.0003327)
#' x2 <- diag(c(0.9972036, 0.9983625, 0.9978063, 0.9967535,
#'   0.9961039, 0.9948677, 0.9923658, 0.9885968, 0.9828676))
#' usa <- rbind(x1, cbind(x2,0))
#' sd21 <- secder(usa,2,1)
#' sd21
#'
#' @export

secder <- function(A, k, l) {
  n <- dim(A)[1]
  svec <- matrix(0, nrow = n^2, ncol = n)
  scalesens <- matrix(0, nrow = n^2, ncol = n - 1)
  d2 <- matrix(0, nrow = n, ncol = n)
  ev <- eigen(A)
  L <- ev$values
  o <- order(Mod(L))
  W <- ev$vectors
  V <- solve(Conj(W))
  V <- t(Conj(V))
  ### have to re-order V only to match matlab
  o <- order(Mod(L))
  V <- V[, rev(o)]
  for (i in 1:n) {
    senmat <- Conj(V[, i]) %*% t(W[, i])
    svec[, i] <- matrix(senmat, nrow = n^2, ncol = 1)
  }
  s1 <- svec[, 1]
  for (m in 2:n) {
    scalesens[, m - 1] <- svec[, m] / (L[1] - L[m])
  }
  vecsum <- t(apply(scalesens, 1, sum))
  for (i in 1:n) {
    for (j in 1:n) {
      x1 <- (l - 1) * n + 1 + (i - 1)
      x2 <- (j - 1) * n + 1 + (k - 1)
      d2[i, j] <- s1[x1] * vecsum[x2] + s1[x2] * vecsum[x1]
    }
  }
  d2 <- Re(d2)
  d2[A == 0] <- 0
  return(d2)
}
