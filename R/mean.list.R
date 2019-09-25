#' Mean matrix
#'
#' Calculates mean matrix from a list of matrices
#'
#' Returns the mean matrix from a list of matrices using a combination of
#' \code{\link{unlist}} and \code{\link{rowMeans}}.  See example for details.
#'
#' @param x A list of two or more matrices
#' @param \dots Additional arguments passed to \code{\link{rowMeans}}
#'
#' @return The mean matrix
#'
#' @note S3 method for the \code{\link{mean}} of a list of matrices
#'
#' @seealso \code{\link{var2}}
#'
#' @author Chris Stubben
#'
#' @examples
#' mean(hudsonia)
#' # or
#' x <- matrix(unlist(hudsonia), ncol=length(hudsonia) )
#' matrix2(rowMeans(x), colnames(hudsonia[[1]]))
#'
#' @export

mean.list <- function(x, ...) {
  if (!all(sapply(x, is.matrix))) {
    stop("'x' must be a list containing matrices")
  }
  dims <- sapply(x, dim)
  n <- dims[1, 1]
  p <- dims[2, 1]
  if (!all(n == dims[1, ]) || !all(p == dims[2, ])) {
    stop("the matrices must have the same dimensions")
  }
  mat <- matrix(unlist(x), n * p, length(x))
  mm <- matrix(rowMeans(mat, ...), n, p)
  dimnames(mm) <- dimnames(x[[1]])
  mm
}
