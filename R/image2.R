#' Display a matrix image
#'
#' Creates a grid of colored rectangles to display a projection, elasticity,
#' sensitivity or other matrix.
#'
#' @param x A numeric matrix with row and column names
#' @param col A vector of colors for boxes
#' @param breaks A numeric vector of break points or number of intervals into
#' which \code{x} is to be \code{\link{cut}}. Default is the length of \code{col}
#' @param log Cut values in \code{x} using a log scale, default TRUE
#' @param border The border color for boxes, default is no borders
#' @param box.offset Percent reduction in box size (a number between 0 and 1),
#' default is 10\% reduction
#' @param round Number of decimal places to display values of \code{x} in each box
#' @param cex Magnification size of text and labels, if specified this will
#' replace values in both text.cex and label.cex
#' @param text.cex Magnification size of text in cells only
#' @param text.col Color of text in cells, use NA to skip text labels
#' @param mar Margins on four sides of plot
#' @param labels A vector giving sides of the plot (1=bottom, 2=left, 3=top,
#' 4=right) for row and column labels
#' @param label.offset Amount of space between label and boxes
#' @param label.cex Magnification size of labels
#' @param srt String rotation for labels on top and bottom of matrix
#'
#' @return A image plot of the matrix
#'
#' @note #' The minimum value in \code{x} is usually assigned to the first color
#' category and the rest of the values are then cut into equally spaced
#' intervals.  This was added to show transitions with very low probabilities in
#' a new color category, eg, 2e-06 would usually be grouped with 0 using
#' \code{\link{image}}. Note if all elements > 0, then the first color will not
#' be used.
#'
#' @seealso \code{\link{image}}
#'
#' @author Chris Stubben
#'
#' @examples
#' data(calathea)
#' A <- calathea[[11]]
#' op <- par(mfrow=c(2,2))
#' image2(A, text.cex=.8)
#' ## with  gray border and labels on bottom right
#' image2( A, text.cex=.8, border="gray70", labels=c(1,4), mar=c(3,1,1,3))
#' ## no text or box offset
#' image2( A, box.offset=0, text.col=NA)
#' # set zeros to NA to print everything but zero
#' A[A == 0] <- NA
#' image2( A, box.offset=0 , text.cex=.8)
#' ## if comparing two or more matrices, get the log10 range
#' ## of values (not including zero) and pass to breaks
#' x <- unlist(calathea[-17])
#' x <- log10(range(x[x!=0]))
#' par(mfrow=c(4,4))
#' for(i in 1:16){
#'   A <- calathea[[i]]
#'   A[A == 0] <- NA
#'   image2(A, cex=.7, box.offset=0, breaks=seq(x[1], x[2], len=24))
#'     title(names(calathea[i]), line=3)
#' }
#' par(op)
#'
#' @export

image2 <- function(x, col = c("white", rev(heat.colors(23))), breaks, log = TRUE,
    border = NA, box.offset = 0.1, round = 3, cex, text.cex = 1, text.col = "black",
    mar = c(1, 3, 3, 1), labels = 2:3, label.offset = 0.1, label.cex = 1, srt = 90) {
  ## convert vector like 1:5 to matrix with 1 row (default is one column)
  if (!is.matrix(x)) {
    x <- t(as.matrix(x)) # only transpose if vector
  }
  if (!is.numeric(x)) {
    stop("A numeric matrix is required")
  }
  if (!missing(cex)) {
    text.cex <- cex
    label.cex <- cex
  } ## cex replaces any values in text.cex or label.cex
  op <- par(mar = mar, xpd = TRUE)
  x <- x[nrow(x):1, , drop = FALSE] ## flip matrix so top row on botton
  x1 <- ncol(x) ## number of columns and rows
  y1 <- nrow(x)
  # hack to get three colors needed for cut
  if (length(col) == 1) {
    col <- rep(col, 3)
  }
  if (length(col) == 2) {
    col <- c(col, col[-1])
  }
  if (missing(breaks)) {
    breaks <- length(col) - 1
  } ## number of breaks
  else {
    if (length(breaks) != length(col)) {
      warning("Breaks is not the same length as colors.
Some blocks may be unfilled or some colors may not be used")
    }
  }
  ## check if any values < 0.  If so, minimum value will be in first color category.
  checkmin <- min(x, na.rm = TRUE)
  if (checkmin < 0) {
    x <- x - checkmin
  }
  ## first check for NAs
  missingNA <- is.na(x)
  x[x == 0] <- NA ## set zeros to NA for log10
  if (log) {
    z <- cut(log10(x), breaks)
  } ## cut into intervals using log10 transformation
  else {
    z <- cut(x, breaks)
  }
  z2 <- matrix(z, y1, x1) ## reshape into matrix
  x[is.na(x)] <- 0 ## set NA values back to zero
  x[missingNA] <- NA ## reset NAs

  if (checkmin < 0) {
    x <- x + checkmin
  }
  x <- round(x, round) ## round decimal places
  ## PLOT rectangles
  offset <- box.offset / 2
  plot(seq(1, x1 + 1, len = 2), seq(1, y1 + 1, len = 2), type = "n", axes = FALSE, ann = FALSE, xaxs = "i", yaxs = "i")
  for (i in 1:x1)
  {
    for (j in 1:y1)
    {
      if (is.na(z2[j, i])) {
        n1 <- 1
      } ## if element is zero, use first color (default white)
      else {
        n1 <- match(z2[j, i], levels(z)) + 1
      } # else match to cut levels in z2 and use that color (plus 1)
      rect(i + offset, j + offset, i + 1 - offset, j + 1 - offset, border = border, col = col[n1])
      if (!is.na(text.col)) {
        text(i + .5, j + .5, x[j, i], cex = text.cex, col = text.col)
      }
    }
  }
  ## rownames (left and right)
  if (2 %in% labels) text(1 - label.offset, 1:y1 + .5, rownames(x), pos = 2, offset = 0, cex = label.cex) ## on left
  if (4 %in% labels) text(x1 + 1 + label.offset, 1:y1 + .5, rownames(x), pos = 4, offset = 0, cex = label.cex)
  ## colnames (bottom and top)
  if (1 %in% labels) text(1:x1 + .5, 0.9 - label.offset, colnames(x), pos = 2, offset = 0, srt = (0 + srt), cex = label.cex)
  if (3 %in% labels) text(1:x1 + .5, y1 + 1.1 + label.offset, colnames(x), pos = 2, offset = 0, srt = (360 - srt), cex = label.cex)
  par(op)
}
