#' Sensitivity analysis of a projection matrix
#'
#' Calculate the sensitivities of eigenvalues to changes in the projection
#' matrix elements
#'
#' see section 9.1 in Caswell (2001)
#'
#' @param A A projection matrix
#' @param zero Set sensitivities for unobserved transitions to zero, default is false
#'
#' @return A sensitivity matrix
#'
#' @references Caswell, H. 2001. Matrix population models: construction,
#' analysis, and interpretation, Second edition. Sinauer, Sunderland,
#' Massachusetts, USA.
#'
#' @seealso \code{\link{elasticity}}
#'
#' @author Chris Stubben
#'
#' @examples
#' data(teasel)
#' sens <- sensitivity(teasel)
#' ## IMAGE plot with smaller boxes
#' image2(sens, mar=c(1,3.5,5,1), box.offset=.1)
#'  title("Sensitivity matrix using image2", line=2.5)
#' ## MATPLOT
#' matplot2(sens, log='y', type='b', yaxt='n', ltitle="Fate",
#'  ylab=expression(paste("Sensitivity of ",lambda)),
#'  main="Sensitivity matrix using matplot2")
#' pwrs <- -4:1
#' axis(2, 10^pwrs, parse(text=paste("10^", pwrs, sep = "")), las=1)
#'
#' @export

sensitivity <- function(A, zero = FALSE) {
  ev <- eigen(A)
  lmax <- which.max(Re(ev$values))
  W <- ev$vectors
  w <- abs(Re(W[, lmax]))
  V <- try(Conj(solve(W)), silent = TRUE)
  if (class(V) == "try-error") {
    message("Warning: matrix is singular")
    s <- A * NA
  } else {
    v <- abs(Re(V[lmax, ]))
    s <- v %o% w
    if (zero) {
      s[A == 0] <- 0
    }
    dimnames(s) <- dimnames(A)
  }
  s
}
