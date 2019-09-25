#' Generation time
#'
#' Calculates the generation time of a stage-classified matrix
#'
#' see section 5.3.5 in Caswell (2001).
#'
#' @param A projection matrix
#' @param \dots additional items are passed to \code{\link{splitA}} and are
#' used to split A into T and F matrices
#'
#' @return Generation time. If the transition matrix is singular, then NA is returned.
#'
#' @references Caswell, H. 2001. Matrix population models: construction,
#' analysis, and interpretation, Second edition. Sinauer, Sunderland,
#' Massachusetts, USA.
#'
#' @seealso see \code{\link{fundamental.matrix}} and \code{\link{net.reproductive.rate}}
#' for other age-specific traits
#'
#' @author Chris Stubben
#'
#' @examples
#' data(whale)
#' generation.time(whale)
#' ## fertilities in last column
#' data(teasel)
#' generation.time(teasel, r=1:6, c=6)
#' ## Plot 3 from Calathea
#' data(calathea)
#' sapply(calathea[9:12], generation.time)
#'
#' @export

generation.time <- function(A, ...) {
  if (!is.matrix(A)) {
    stop("A projection matrix is required")
  }
  A1 <- splitA(A, ...)
  Tmat <- A1[[1]]
  Fmat <- A1[[2]]

  # probably could add some other checks here
  s <- length(diag(Tmat))
  # check if matrix is singular
  N <- try(solve(diag(s) - Tmat), silent = TRUE)
  if (class(N) == "try-error") {
    generation.time <- NA
  }
  else {
    R <- Fmat %*% N
    Ro <- lambda(R)
    lambda <- lambda(A)
    generation.time <- log(Ro) / log(lambda)
  }
  generation.time
}
