#' Plot a matrix
#'
#' Plot the rows of a matrix.  Useful for displaying a matrix of stage vectors,
#' survival rates and sensitivities.
#'
#' @param x a matrix
#' @param proportions If TRUE, then  plot proportional changes
#' @param legend a \code{\link{legend}} keyword or vector of x,y coordinates,
#' defaults to top-right corner
#' @param xlab a label for the x axis
#' @param ylab a label for the y axis
#' @param type plot type, default line
#' @param las style of axis labels, default horizontal
#' @param pch point types
#' @param lwd line width
#' @param lty line type
#' @param col  color
#' @param lcex legend size expansion
#' @param lbty legend box type
#' @param lcol number of columns in legend
#' @param ltitle legend title
#' @param lsort sort  legend by decreasing order of mean number in row
#' @param \dots additional options are passed to \code{\link{plot}} function
#'
#' @return A matrix plot
#'
#' @note Only a few basic legend options are available.  For more control, set
#' legend=NA and run separately
#'
#' @seealso \code{\link{matplot}} and \code{\link{stage.vector.plot}}
#'
#' @author Chris Stubben
#'
#' @examples
#' # survival rates
#' x <- calathea[9:12]
#' x <- sapply(x, function(x) colSums(splitA(x, r=1:2)$T))
#' matplot2(t(x), legend="bottomright", ylab="Survival",
#'  main="Calathea survival curves")
#' # Growth rates - do not sort legend
#' x <- sapply(calathea[-17], lambda)
#' x <- matrix(x, nrow=4, byrow=TRUE, dimnames= list(paste("plot", 1:4), 1982:1985))
#' matplot2(x, type='b', lsort=FALSE, ylab="Growth rate", main="Calathea growth rates")
#' # Convergence to stable stage (excluding seeds)
#' x <- pop.projection(calathea[[7]], rep(1,8), 10)
#' matplot2(x$stage.vectors[-1,], prop=TRUE,
#'   main="Calathea stage vectors", lcex=.7)
#'
#' @export

matplot2 <- function(x, proportions = FALSE, legend = "topright", xlab = NULL, ylab = NULL, type = "l",
                     las = 1, pch = c(15:18, 1:3), lwd = 1, lty = 1:nrow(x), col = rainbow(nrow(x)),
                     lcex = 1, lbty = "o", lcol = 1, ltitle = NULL, lsort = TRUE, ...) {
  n <- nrow(x)
  if (is.null(n)) {
    stop("x should be a matrix with at least 2 rows")
  }
  if (proportions) {
    x <- prop.table(x, 2) ## Change counts to proportions using prop.table
    if (is.null(ylab)) {
      ylab <- "Proportion"
    }
  }
  if (length(col) < n) {
    col <- rep(col, length.out = n)
  } ## line colors (repeat if necessary)
  if (length(lty) < n) {
    lty <- rep(lty, length.out = n)
  } ## line types
  if (length(pch) < n) {
    pch <- rep(pch, length.out = n)
  } ## point types
  if (is.null(ylab)) {
    ylab <- "Total"
  } ## default y label
  if (is.null(colnames(x))) {
    colnames(x) <- 1:ncol(x)
  } ## No column names, use default 1:n
  ## convert colnames to number if possible
  suppressWarnings(xnames <- as.numeric(colnames(x)))
  # any characters in column names?
  if (any(is.na(xnames))) {
    if (is.null(xlab)) {
      xlab <- "Stage"
    }
    matplot(t(x), xlab = xlab, ylab = ylab, col = col, las = las, type = type, lty = lty, lwd = lwd, pch = pch, xaxt = "n", ...)
    ## probably should add pretty breakpoints if ncol(x)>10
    axis(1, at = 1:ncol(x), labels = colnames(x))
  }
  ## else numeric column names
  else {
    if (is.null(xlab)) {
      xlab <- "Year"
    }
    matplot(xnames, t(x), xlab = xlab, ylab = ylab, col = col, las = las, type = type, lty = lty, lwd = lwd, pch = pch, xaxt = "n", ...)
    ## if only a few years, do not label  1992.0, 1992.5, etc
    if (ncol(x) < 6) {
      axis(1, at = xnames, labels = as.character(xnames))
    }
    else {
      axis(1, at = pretty(xnames))
    }
  }
  ## order legend by decreasing order of mean number in row
  if (lsort) {
    y <- sort(apply(x, 1, mean, na.rm = TRUE), index.return = TRUE, decreasing = TRUE)
  }
  else {
    y <- list(x = apply(x, 1, mean, na.rm = TRUE), ix = 1:n)
  }
  leg.names <- paste(names(y$x), "") ## pad names with trailing space for extra room
  ## if rownames are missing
  if (leg.names[1] == " ") {
    leg.names <- paste("row", y$ix, "")
  }

  # fix to pass args.legend option like barplot.default
  ## legend box is smaller if pch is used (fits to point)
  if (type == "l") {
    pch <- NULL
  }
  legend(legend[1], legend[2], leg.names,
    lty = lty[y$ix], col = col[y$ix],
    lwd = lwd, pch = pch[y$ix], cex = lcex, bty = lbty, ncol = lcol, title = ltitle
  )
}
