#' Create log-log plots of variance vs. sensitivity and CV vs. elasticity
#'
#' Create log-log plots of both variance vs. sensitivity and CV vs. elasticity
#' in matrix elements.  Plots are based on Figure 2 in Pfister(1998)
#'
#' Calculates mean, variance and coefficient of variation (CV) of matrix
#' elements from a list of two or more projection matrices.  The sensitivity and
#' elasticity matrices are then calculated from the mean matrix using
#' \code{\link{eigen.analysis}}
#'
#' @param A A list of two or more projection matrices
#'
#' @return Creates two log-log plots similar to Figure 2 in Pfister(1998) and
#' outputs a data.frame with 5 columns listing mean, variance, CV, sensitivity
#' and elasticity for matrix elements with a mean and variance > 0
#'
#' @references Pfister, CA.  1998. Patterns of variance in stage-structured
#' populations: Evolutionary predictions and ecological implications. PNAS 95:213-218.
#'
#' @author Chris Stubben
#'
#' @examples
#' ## 4 Hudsonia matrices
#' pfister.plot(hudsonia)
#' ## 3 Mimulus cardinalis matrices at Carlon
#' mim <- subset(monkeyflower, species == "cardinalis" &
#'     site == "Carlon" & year != "pooled", select = c(4:19))
#' ## convert data frame to list of matrices using split
#' mim1 <-split(mim, 2000:2002)
#' mim2 <-lapply(mim1, matrix, nrow=4, byrow=TRUE)
#' vr1 <- pfister.plot(mim2)
#' vr1
#' ## PLOT using labels
#' plot(vr1$cv, vr1$elas, xlab="CV", ylab="Elasticity", log="xy", type='n')
#' # Split matrix elements into transitions representing F (fertility),
#' # S (survival), G (growth), and R (retrogression).
#' # Fertility on top row, survival on diagonal, growth is above diagonal
#' # and retrogression below diagonal.
#' rownames(vr1)
#' y2 <- expression(S[11],G[21],G[31],G[41],
#'                  F[12],S[22],G[32],G[42],
#'                  F[13],R[23],S[33],G[43],
#'                  F[14],R[34],S[44])
#' text(vr1$cv, vr1$elas, y2)
#' ### add trend line
#'  abline(lm(log10(vr1$elas)~log10(vr1$cv)), col="red")
#' ## include Spearman's rank correlation
#' a <- cor.test(vr1$cv, vr1$elas, method="spearman")
#' a
#' text(10, .0015, substitute(rho == x, list(x=round(a$estimate,2))), col="blue")
#'
#' @export

pfister.plot <- function(A) {
  n <- length(A)
  # error-checking
  if (class(A)[1] != "list") {
    stop("A LIST of annual matrices is required")
  }
  ## matrix inputs should have same dimensions
  if (length(unique(lapply(A, dim))) > 1) {
    stop("Matrices have different dimensions")
  }
  if (n < 2) {
    stop("A list of TWO or more projection matrices is required input")
  }
  # COLUMN NAMES
  col <- names(A)
  # NUMBER of stages
  x <- dim(A[[1]])[1]
  ## ROW names
  row <- paste("a", 1:x, rep(1:x, each = x), sep = "")
  ## annual matrix elements
  vr <- data.frame(matrix(unlist(A), ncol = n, dimnames = list(row, col)))
  ## MEAN, var, and cv
  vr$mean <- apply(vr, 1, mean)
  vr$var <- apply(vr, 1, var)
  vr$cv <- vr$var^.5 / vr$mean * 100
  ## mean matrix
  meanA <- matrix(vr$mean, nrow = x)
  ## Sensitivities and elasticities of mean matrix
  eigA <- eigen.analysis(meanA)
  vr$sens <- as.vector(eigA$sensitivities)
  vr$elas <- as.vector(eigA$elasticities)
  # NON-ZERO elements
  vr1 <- subset(vr, mean > 0 & var > 0)
  #### log-log PLOTS
  op <- par(mfrow = c(1, 2))
  plot(vr1$var, vr1$sens, xlab = "Variance", ylab = "Sensitivity", log = "xy", pch = 16, col = "blue")
  plot(vr1$cv, vr1$elas, xlab = "CV", ylab = "Elasticity", log = "xy", pch = 16, col = "blue")

  par(op)
  # output plot values
  vr1[, (n + 1):(n + 5)]
}
