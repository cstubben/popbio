#' Fundamental matrix and age-specific survival
#'
#' Age-specific survival calculations from stage-classified matrices.  Includes
#' the mean, variance and coefficient of variation (cv) of the time spent in
#' each stage class and the mean and variance of the time to death
#'
#' see section 5.3.1 in Caswell (2001).
#'
#' @param A projection matrix
#' @param \dots additional items are passed to \code{\link{splitA}} and are
#' used to split A into T and F matrices
#'
#' @return   A list with 5 items
#' \item{N}{ fundamental matrix or mean of the time spent in each stage class}
#' \item{var}{ variance of the time spent in each stage class}
#' \item{cv}{ coefficient of variation (sd/mean) }
#' \item{meaneta}{ mean of time to death}
#' \item{vareta}{ variance of time to death }
#'
#' @references Caswell, H. 2001. Matrix population models: construction,
#' analysis, and interpretation, Second edition. Sinauer, Sunderland,
#' Massachusetts, USA.
#'
#' @seealso see \code{\link{generation.time}} and \code{\link{net.reproductive.rate}}
#' for other age-specific traits
#'
#' @author Chris Stubben
#'
#' @examples
#' fundamental.matrix(whale)
#'
#' @export

fundamental.matrix <- function(A, ...) {
  if (!is.matrix(A)) {
    stop("A projection matrix is required")
  }
  A1 <- splitA(A, ...)
  Tmat <- A1[[1]]

  s <- length(diag(Tmat))
  # check if matrix is singular
  N <- try(solve(diag(s) - Tmat), silent = TRUE)
  if (class(N) == "try-error") {
    fundamental.matrix <- "Transition matrix is singular"
  }
  else {
    var <- (2 * diag(diag(N)) - diag(s)) %*% N - N * N
    dimnames(var) <- dimnames(A)
    total <- margin.table(N, 2)
    vareta <- margin.table(2 * (N %*% N) - N, 2) - total * total

    fundamental.matrix <- list(
      N = N,
      var = var,
      cv = var^.5 / N,
      meaneta = total,
      vareta = vareta
    )
  }
  fundamental.matrix
}
