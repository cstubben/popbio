#' Population growth rate
#'
#' Calculates the population growth rate of a projection matrix
#'
#' see section 4.4 in Caswell (2001)
#'
#' @param A A projection matrix
#'
#' @return The dominant eigenvalue
#'
#' @note The built-in \code{\link{eigen}} function returns eigenvalues in
#' descreasing order of magnitude or modulus.  The dominant eigenvalue of
#' imprimitive matrices with \emph{d} eigenvalues of equal modulus is the one
#' with the largest real part (\code{which.max(Re(eigen(A)$values))}).
#'
#' @seealso \code{\link{eigen}} and \code{\link{pop.projection}}
#'
#' @references Caswell, H. 2001. Matrix population models: construction,
#' analysis, and interpretation, Second edition. Sinauer, Sunderland,
#' Massachusetts, USA.
#'
#' @author Chris Stubben
#'
#' @examples
#' A<-matrix(c(0,0,2,.3,0,0,0,.6,0), nrow=3,byrow=TRUE)
#' lambda(A)
#' Re(eigen(A)$values)
#' data(hudsonia)
#' sapply(hudsonia, lambda)
#'
#' @export

lambda <- function(A) {
  ev <- eigen(A)
  # R sorts eigenvalues in decreasing order, according to Mod(values)
  #  ususally dominant eigenvalue is first (ev$values[1]), except for
  #  imprimitive matrices with d eigenvalues of equal modulus
  # this should work for most cases
  lmax <- which.max(Re(ev$values))
  lambda <- Re(ev$values[lmax])
  lambda
}
