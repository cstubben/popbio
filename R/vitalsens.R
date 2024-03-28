#' Vital rate sensitivities and elasticities
#'
#' Calculates deterministic sensitivities and elasticities of lambda to
#' lower-level vital rates using partial derivatives
#'
#' Vital rate sensitivities and elasticities are discussed in example 9.3 and
#' 9.6 in Caswell (2001). Also see Chapter 9 and Box 9.1 for Matlab code in
#' Morris and Doak (2002).
#'
#' @param elements An object of mode \code{\link{expression}} with all matrix
#' elements represented by zeros or symbolic vital rates
#' @param vitalrates A list of vital rates with \code{\link{names}} matching
#' expressions in elements above
#'
#' @return A dataframe with vital rate estimates, sensitivities, and elasticities
#'
#' @note The element expressions should return the actual matrix element estimates
#'  after evaluating the variables using \code{\link{eval}} below.
#'
#'  \code{A<-sapply(elements, eval, vitalrates, NULL)}
#'
#' In addition, these expressions should be arranged by rows so the
#' following returns the projection matrix.
#'
#'  \code{matrix(A, nrow=sqrt(length(elements)), byrow=TRUE)}
#'
#' @references Caswell, H. 2001. Matrix population models: construction,
#' analysis, and interpretation, Second edition. Sinauer, Sunderland,
#' Massachusetts, USA.
#'
#' Morris, W. F., and D. F. Doak. 2002. Quantitative conservation biology:
#' Theory and practice of population viability analysis. Sinauer, Sunderland,
#' Massachusetts, USA.
#'
#' @author Chris Stubben. Based on code posted by Simon Blomberg to R-help mailing list
#'
#' @examples
#' ## emperor goose in Morris and Doak 2002.
#' goose.vr <- list( Ss0=0.1357,  Ss1=0.8926,  Sf2=0.6388,  Sf3= 0.8943)
#' goose.el <- expression(
#' 0,  0,  Sf2*Ss1,Sf3*Ss1,
#' Ss0,0,  0,      0,
#' 0,  Ss1,0,      0,
#' 0,  0,  Ss1,    Ss1)
#' ## first plot effects of changing vital rates -- Figure 9.1
#' n <- length(goose.vr)
#' vr <- seq(0,1,.1)
#' vrsen <- matrix(numeric(n*length(vr)), ncol=n, dimnames=list(vr, names(goose.vr)))
#' for (h in 1:n) {
#'    goose.vr2 <- list(  Ss0=0.1357,  Ss1=0.8926,  Sf2=0.6388,  Sf3= 0.8943)
#'    for (i in 1:length(vr))
#'    {
#'       goose.vr2[[h]] <- vr[i]
#'       A <- matrix(sapply(goose.el, eval,goose.vr2 , NULL), nrow=sqrt(length(goose.el)), byrow=TRUE)
#'       vrsen[i,h] <- max(Re(eigen(A)$values))
#'    }
#' }
#' matplot(rownames(vrsen), vrsen, type='l', lwd=2, las=1,
#' ylab="Goose population growth", xlab="Value of vital rate",
#' main="Effects of changing goose vital rates")
#' vrn <- expression(s[0], s["">=1], f[2], f["">=3])
#' legend(.8, .4, vrn, lty=1:4, lwd=2, col=1:4, cex=1.2)
#' ## then calculate sensitivities  -- Table 9.1
#' x <- vitalsens(goose.el, goose.vr)
#' x
#' sum(x$elasticity)
#' barplot(t(x[,2:3]), beside=TRUE, legend=TRUE, las=1, xlab="Vital rate",
#' main="Goose vital rate sensitivity and elasticity")
#' abline(h=0)
#' ## Table 7 endangered lesser kestral in Hiraldo et al 1996
#' kest.vr <-  list(b = 0.9321, co = 0.3847, ca = 0.925, so = 0.3409, sa = 0.7107)
#' kest.el <- expression( co*b*so, ca*b*so, sa, sa)
#' x <- vitalsens(kest.el, kest.vr)
#' x
#' sum(x$elasticity)
#' barplot(t(x[,2:3]), beside=TRUE, las=1, xlab="Vital rate",
#' main="Kestral vital rate sensitivity and elasticity")
#' legend(1,1, rownames(t(x[,2:3])), fill=grey.colors(2))
#' abline(h=0)
#'
#' @export

vitalsens <- function(elements, vitalrates) {
  ## check if elements is matrix expression?
  #  expression(matrix2(c(0,F,G,S)))
  # or  expression(0,F,G,S)
  #  grepl("matrix", elements)
  if (is.vector(vitalrates)) {
    vitalrates <- as.list(vitalrates)
  }
  if (!is.list(vitalrates)) {
    stop("Vital rates should be a vector or list")
  }
  if (class(elements)[1] != "expression") {
    stop("Matrix elements should be an expression")
  }
  ## check length of expression
  n <- sqrt(length(elements))
  if (n %% 1 != 0) {
    stop(paste(
      "Length of element expression is", length(elements),
      "- Expecting power of 2 like 4,9,16 to form a square matrix"
    ))
  }
  ## get values for matrix elements - enclos=NULL is used to restrict eval to names in vitalrates
  vrs <- try(sapply(elements, eval, vitalrates, NULL), silent = TRUE)
  if (inherits(vrs, "try-error")) {
    # keep useful part of error message
    vrs <- sub("Error in eval\\(expr, envir, enclos\\) :", "", vrs[1])
    stop(paste("Cannot evaluate element expression using given vital rates:", vrs))
  }
  ## store results in data.frame
  res <- data.frame(estimate = unlist(vitalrates), sensitivity = 0, elasticity = 0)
  ## create projection matrix
  A <- matrix(vrs, nrow = n, byrow = TRUE)
  eig <- eigen.analysis(A)
  ## finally, get derivatives
  deriv.funcs <- sapply(elements, deriv, namevec = names(vitalrates), function.arg = TRUE)
  devs <- lapply(deriv.funcs, function(x) do.call(x, vitalrates))
  for (i in 1:length(vitalrates))
  {
    derivs <- matrix(as.numeric(lapply(devs, function(x) attr(x, "gradient")[i])), nrow = n, byrow = TRUE)
    res[i, 2] <- sum(derivs * eig$sensitivities)
    res[i, 3] <- vitalrates[[i]] / eig$lambda1 * sum(derivs * eig$sensitivities)
  }
  res
}
