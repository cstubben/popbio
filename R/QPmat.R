#' Build a projection matrix from a time series of individuals (or densities) per stage
#'
#' Builds one projection matrix from a time series of number (or densities) of
#' individuals per stage (size classes or life stages) using Wood's quadratic
#' programming method. The matrix model also requires a constraint matrix C,
#' vector b, and vector listing nonzero elements of desired projection matrix.
#'
#' converted Matlab code from Example 6.3 in Caswell (2001)
#'
#' @param nout A time series of population vectors
#' @param C constraint matrix
#' @param b b vector
#' @param nonzero indices of the non-zero elements of the transition matrix (counting by column)
#'
#' @return A projection matrix.
#'
#' @note This function requires \code{solve.QP} in the \code{quadprog} package.
#'
#' @author Adapted to R by Patrick Nantel
#'
#' @examples
#' \dontrun{
#' ## list nonzero elements
#' nonzero <- c( 1, 2, 5, 6, 7, 9)
#' ## create C matrix
#' C <- rbind(diag(-1,6), c(1,1,0,0,0,0), c(0,0,1,1,0,0), c(0,0,0,0,0,1))
#' ## calculate b (transpose is not necessary - either way works)
#' b <- apply(C, 1, max)
#' QPmat(nematode, C,b,nonzero)
#' }
#'
#' @export

QPmat <- function(nout, C, b, nonzero) {
  # requires solve.QP in quadprog
  if (!requireNamespace("quadprog", quietly = TRUE)) {
    stop("quadprog needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  n <- dim(nout)
  # Generate the data vector
  z <- nout[, 2:n[2]]
  z <- matrix(z, n[1] * (n[2] - 1), 1)
  # Generate the matrix M
  M <- c()
  for (i in 1:(n[2] - 1)) {
    N <- kronecker(t(nout[, i]), diag(n[1]))
    m <- N[, nonzero]
    M <- rbind(M, m)
  }
  # Generate the matrix G and the vector f
  G <- t(M) %*% M
  f <- t(M) %*% z
  # Call R's quadratic programming routine
  res <- quadprog::solve.QP(G, f, -t(C), -b)
  phat <- res$solution
  # Generate the estimated projection matrix
  a <- numeric(n[1]^2)
  a[nonzero] <- phat
  a <- matrix(a, n[1], n[1])
  x <- rownames(nout) ##  add stage class names to vector
  dimnames(a) <- list(x, x)
  a
}
