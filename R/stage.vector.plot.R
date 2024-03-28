#' Plot stage vector projections
#'
#' Plots short-term dynamics and convergence to stage stage distribution using
#' stage vector projections
#'
#' see section 2.2 in Caswell 2001
#'
#' @param stage.vectors a matrix listing stage class vectors in columns
#' @param proportions plot proportional changes or total numbers, defaults to proportions
#' @param legend.coords a \code{\link{legend}} keyword or vector of x,y coordinates,
#' defaults to top-right corner
#' @param ylim the y limits of the plot, defaults to min and max values in stage.vectors
#' @param xlab a label for the x axis
#' @param ylab a label for the y axis
#' @param col vector of line colors
#' @param \dots additional options are passed to \code{\link{plot}} function
#'
#' @return A plot of stage or age class projections
#'
#' @seealso see \code{\link{pop.projection}}
#'
#' @author Chris Stubben
#'
#' @examples
#' ## matrix from Example 2.1 in Caswell
#' A <- matrix2(c(
#' 0, 0.3,   0,
#' 1,   0, 0.5,
#' 5,   0,   0
#' ), 1:3)
#' n <- c(1,0,0)
#' p <- pop.projection(A,n,60)
#' ## Plots in Figure 2.3
#' stage.vector.plot(p$stage.vector[,1:15], col='black', las=1, prop=FALSE)
#' stage.vector.plot(p$stage.vector[,1:40], col=2:4, las=1)
#' ## log-scale with custom y-axis
#' stage.vector.plot(p$stage.vector, col=2:4, prop=FALSE,
#' ylim=c(.01, 10), log='y', legend="bottomright", yaxt='n')
#' pwrs <- -2:1
#' # major ticks
#' axis(2, at = 10^pwrs, labels=parse(text=paste("10^", pwrs, sep = "")),
#' las=1, tcl= -.6)
#' # minor ticks
#' axis(2, at = 1:9 * rep(10^pwrs[-1] / 10, each = 9),
#'     tcl = -0.3, labels = FALSE)
#'
#' @export

stage.vector.plot <- function(stage.vectors, proportions = TRUE, legend.coords = "topright",
  ylim = NULL, xlab = "Years", ylab = NULL, col = 1:8, ...) {
  p <- stage.vectors
  n <- dim(p)[1] # number of stage vectors
  if (is.null(n)) {
    stop("stage.vectors should be a matrix with two or more stages")
  }
  x <- colnames(p) # x-axis names
  if (is.null(x)) {
    x <- 0:(dim(p)[2] - 1)
  } # start at 0,1,2,... if colnames missing
  if (length(col) < n) {
    col <- rep(col, n)
  } ## line colors (repeat if necessary)
  if (proportions) ## plot proportions
  {
    if (is.null(ylab)) {
      ylab <- "Proportion in stage class"
    }
    p <- prop.table(p, 2) ## Change counts to proportions using prop.table
    if (is.null(ylim)) {
      ylim <- c(min(p, na.rm = TRUE), max(p, na.rm = TRUE))
    }
    plot(x, p[1, ], type = "n", ylim = ylim, xlab = xlab, ylab = ylab, ...)
  }
  else ## OR plot total number
  {
    if (is.null(ylab)) {
      ylab <- "Number in stage class"
    }
    if (is.null(ylim)) {
      ylim <- c(floor(min(p, na.rm = TRUE)), ceiling(max(p, na.rm = TRUE)))
    }
    plot(x, p[1, ], type = "n", ylim = ylim, xlab = xlab, ylab = ylab, ...)
  }
  ## order legend by decreasing order of mean number in stage vector
  y <- sort(apply(p, 1, mean, na.rm = TRUE), index.return = TRUE, decreasing = TRUE)
  for (i in y$ix) ## Loop through stage classes
  {
    lines(x, p[i, ], lty = i, col = col[i], lwd = 2)
  }
  leg.names <- paste(names(y$x), "") ## pad names with trailing space for extra room
  if (leg.names[1] == " ") {
    leg.names <- paste("row", y$ix, "")
  }
  legend(legend.coords[1], legend.coords[2], leg.names, lty = y$ix, col = col[y$ix], lwd = 2)
}
