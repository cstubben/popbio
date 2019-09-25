#' Eigenvalue and eigenvector analysis of a projection matrix
#'
#' Calculate population growth rate and other demographic parameters from a
#' projection matrix model using matrix algebra
#'
#' The calculation of eigenvalues and eigenvectors partly follows Matlab code in
#' section 4.8.1 (p. 107) in Caswell (2001). Since \code{popbio} version 2.0,
#' each part returned by \code{eigen.analysis} is now inlcuded as a separate
#' function.
#'
#' @param A A projection matrix
#' @param zero Set sensitivities for unobserved transitions to zero, default is FALSE
#'
#' @return A list with 6 items
#'    \item{lambda1}{dominant eigenvalue with largest real part }
#'    \item{stable.stage}{proportional stable stage distribution}
#'    \item{sensitivities }{matrix of eigenvalue sensitivities}
#'    \item{elasticities}{matrix of eigenvalue elasticities}
#'    \item{repro.value}{reproductive value scaled so v[1]=1}
#'    \item{damping.ratio}{damping ratio  }
#'
#' @note If matrix A is singular, then \code{eigen.analysis} will return
#' elasticities, sensitivities, and reproductive values with NAs.
#'
#' @seealso \code{\link{eigen}} and \code{\link{pop.projection}}
#'
#' @references Caswell, H. 2001. Matrix population models: construction,
#' analysis and interpretation, Second edition. Sinauer, Sunderland,
#' Massachusetts, USA.
#'
#' @author Original code by James Holland Jones, Stanford University, August 2005
#'
#' @examples
#' ## Imprimitive matrix
#' A <- matrix(c(0,0,2,.3,0,0,0,.6,0), nrow=3,byrow=TRUE)
#' A
#' ev <- eigen(A)
#' ev$values
#' Mod(ev$values)
#' lmax <- which.max(Re(ev$values))
#' lmax
#' Re(ev$values)[lmax]
#' ## damping ratio is NA
#' eigen.analysis(A)
#' ## cycles every 3 years
#' stage.vector.plot(pop.projection(A, c(1,1,1), 10)$stage.vectors)
#' ### Teasel
#' a <- eigen.analysis(teasel)
#' a
#' barplot(a$stable.stage, col="green", ylim=c(0,1),
#'   ylab="Stable stage proportion", xlab="Stage class", main="Teasel")
#' box()
#' op <- par(mfrow=c(2,2))
#' image2(teasel, cex=.8, mar=c(0.5,3,4,1) )
#' title("Teasel projection matrix", line=3)
#' image2(a$elasticities, cex=.8, mar=c(0.5,3,4,1) )
#' title("Elasticity matrix", line=3)
#' ## default is sensitivity for non-zero elements in matrix
#' image2(a$sensitivities, cex=.8, mar=c(0.5,3,4,1) )
#' title("Sensitivity matrix 1", line=3)
#' ## use zero=FALSE to get sensitivities of all elements
#' image2(eigen.analysis(teasel, zero=FALSE)$sensitivities, cex=.8, mar=c(0.5,3,4,1) )
#' title("Sensitivity matrix 2", line=3)
#' par(op)
#'
#' @export

eigen.analysis <- function(A, zero = FALSE) {
  ev <- eigen(A)
  # R sorts eigenvalues in decreasing order, according to Mod(values), usually
  # dominant eigenvalue is first (ev$values[1]) except for imprimitive matrices
  # with d eigenvalues of equal modulus.
  lmax <- which.max(Re(ev$values))
  lambda <- Re(ev$values[lmax])
  # Damping ratio. Use second eigenvalue OR second largest magnitude in case of
  # ties using rle - round needed for imprimitive matrices
  dr <- rle(round(Mod(ev$values), 5))$values
  dr <- dr[1] / dr[2]
  W <- ev$vectors
  w <- abs(Re(W[, lmax]))
  # check if matrix is singular and output NAs rather than stop (better for
  # loops and bootstrapping)
  V <- try(Conj(solve(W)), silent = TRUE)
  if (class(V) == "try-error") {
    eigen.analysis <- list(
      lambda1 = lambda, stable.stage = w / sum(w),
      sensitivities = A * NA, elasticities = A * NA, repro.value = w * NA,
      damping.ratio = dr
    )
  }
  else {
    v <- abs(Re(V[lmax, ]))
    s <- v %o% w
    if (zero) {
      s[A == 0] <- 0
    }
    e <- s * A / lambda
    x <- dimnames(A)
    dimnames(s) <- x
    names(w) <- x[[1]]
    names(v) <- x[[1]]
    eigen.analysis <- list(
      lambda1 = lambda, stable.stage = w / sum(w),
      sensitivities = s, elasticities = e, repro.value = v / v[1],
      damping.ratio = dr
    )
  }
  eigen.analysis
}
