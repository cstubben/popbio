#' Square matrices
#'
#' Create a square matrix from a given set of values
#'
#' @param x a vector of matrix elements
#' @param stages a vector of row names (also  assigned to columns)
#' @param byrow fill matrix by rows , default TRUE
#'
#' @return a square matri
#'
#' @seealso \code{\link{matrix}}
#'
#' @author Chris Stubben
#'
#' @examples
#' # Centaurea corymbosa from Freville 2004
#' ceco <- c(0,0,5.905,0.368,0.639, 0.025, 0.001, 0.152, 0.051)
#' stages <- c("seedling", "vegetative", "flowering")
#' # shortcut for
#' matrix(ceco, nrow=3, byrow=TRUE, dimnames=list(stages,stages))
#' matrix2(ceco, stages)
#'
#' @export

matrix2 <- function(x, stages, byrow = TRUE) {
  if (!is.vector(x)) {
    x <- unlist(x)
  }
  rows <- sqrt(length(x))
  if (ceiling(rows) != floor(rows)) {
    stop("x will not convert to a square matrix")
  }
  if (missing(stages)) {
    stages <- 1:rows
  }
  if (length(stages) != rows) {
    stages <- stages[1:rows]
  } # or exit with error
  y <- matrix(x, nrow = rows, byrow = byrow, dimnames = list(stages, stages))
  y
}
