#' Damping ratio
#'
#' Calculate the damping ratio of a projection matrix
#'
#' see section 4.7 in Caswell (2001)
#'
#' @param A A projection matrix
#'
#' @return Damping ratio
#'
#' @note The damping ratio is calculated by dividing the dominant eigenvalue by
#' the eigenvalue with the second largest magnitude.
#'
#' @seealso \code{\link{lambda}}
#'
#' @references Caswell, H. 2001. Matrix population models: construction,
#' analysis, and interpretation, Second edition. Sinauer, Sunderland,
#' Massachusetts, USA.
#'
#' @author Chris Stubben
#'
#' @examples
#' ## whale converges slowly to stable stage distribution
#' data(whale)
#' matplot2(pop.projection(whale, c(1,1,1,1), 60)$stage.vectors,
#' prop=TRUE, legend=NA,
#' main=paste("whale damping ratio = ", round(damping.ratio(whale),3) ) )
#' # Calathea - compare to Table 12 in Horvitz and Schemske (1995)
#' data(calathea)
#' x <- sapply(calathea[-17], damping.ratio)
#' x <- matrix(x, nrow=4, byrow=TRUE, dimnames= list(paste("plot", 1:4), 1982:1985))
#' x
#' matplot2(x, type='b', ylab="Damping ratio", main="Calathea")
#'
#' @export

damping.ratio <- function(A) {
  ev <- eigen(A)
  ## Use second largest magnitude in case of ties
  ## Also, add round for imprimitive matrices.   For example,
  ## without rounding, Mod(ev$values) of this matrix
  ## A<-matrix(c(0,0,2,.3,0,0,0,.6,0), nrow=3,byrow=TRUE) is
  ## 0.7113786608980130 0.7113786608980126 and damping.ratio would be 1.
  ## with rounding, only one modulus .711 and damping.ration is NA
  dr <- rle(round(Mod(ev$values), 5))$values
  dr <- dr[1] / dr[2]
  dr
}
