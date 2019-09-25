#' Split a projection matrix into separate T and F matrices
#'
#' Splits a projection matrix into transition and fertility matrices where
#' \code{A = T + F}
#'
#' see section 5.1 in Caswell (2001)
#'
#' @param A a projection matrix
#' @param r rows containing fertilities (default is first row) OR a logical
#' matrix where TRUE is the location of a fertility value OR a complete
#' fertility matrix
#' @param c columns containing fertilities, default is all columns except first
#'
#' @return A list with T and F matrices
#'
#' @note By default, the fertility matrix will include elements in the first row
#' (except first element). In some cases, it is not possible to split a
#' projection matrix using only row and column indexes. Therefore,  a logical
#' matrix (where TRUE is the location of a fertility value) or the complete
#' fertility matrix is also accepted.
#'
#' @references Caswell, H. 2001. Matrix population models: construction,
#' analysis, and interpretation, Second edition. Sinauer, Sunderland,
#' Massachusetts, USA.
#'
#' @seealso functions like  \code{\link{generation.time}} and
#' \code{\link{net.reproductive.rate}} use \code{splitA} to split the matrix
#'
#' @author Chris Stubben
#'
#' @examples
#' splitA(whale)
#' # teasel -fertilitiles in last column
#' splitA(teasel, r=1:6, c=6)
#' # hudsonia - fertilities in first two columns
#' A <- hudsonia[[1]]
#' splitA(A, r=1:2)
#' ## example using a logical matrix (if fertilities were in the upper diagonal)
#' splitA(A, row(A)<col(A))
#' # survival curves
#' x <- sapply(hudsonia, function(x) colSums(splitA(x, r=1:2)$T))
#' matplot2(t(x), legend="bottomright", ylab="Survival",
#'  main="Hudsonia survival curves")
#'
#' @export

splitA <- function(A, r = 1, c = -1) {
  tm <- A
  fm <- A
  ### check if r is a matrix
  if (is.matrix(r)) {
    # logical matrix (TRUE=fertility)
    if (is.logical(r)) {
      tm[r] <- 0
      fm[!r] <- 0
    } else {
      tm <- A - r
      fm <- r
    }
  } else {
    tm[r, c] <- 0
    fm[-r, ] <- 0
    fm[r, -(c)] <- 0
  }
  list(T = tm, F = fm)
}
