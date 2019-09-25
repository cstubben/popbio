#' Reproductive value
#'
#' Calculates the reproductive values of a projection matrix
#'
#' see section 4.5 in Caswell (2001)
#'
#' @param A A projection matrix
#'
#' @return A vector containing the scaled reproductive values so v[1]=1
#'
#' @references Caswell, H. 2001. Matrix population models: construction,
#' analysis, and interpretation, Second edition. Sinauer, Sunderland,
#' Massachusetts, USA.
#'
#' @author Chris Stubben
#'
#' @examples
#' v <- reproductive.value(teasel)
#' v
#' dotchart(log10(v), pch=16, xlab="Reproductive value (log10)")
#'
#' @export

reproductive.value <- function(A) {
  ev <- eigen(A)
  lmax <- which.max(Re(ev$values))
  W <- ev$vectors
  w <- abs(Re(W[, lmax]))
  V <- try(Conj(solve(W)), silent = TRUE)
  if (class(V) == "try-error") {
    stop("matrix A is singular")
  }
  v <- abs(Re(V[lmax, ]))
  names(v) <- colnames(A)
  v / v[1]
}
