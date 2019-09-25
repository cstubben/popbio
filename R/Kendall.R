#' Find the best Kendall's estimates of mean and environmental variance for
#' beta-binomial vital rates
#'
#' Finds the best estimates of mean and environmental variance for beta-binomial
#' vital rates, using a brute force search for the best adjusted estimates from
#' a very large number of combinations of different possible mean and variance
#' values.
#'
#' converted Matlab code from  Box 8.2 in Morris and Doak (2002)
#'
#' @param rates}{a matrix or dataframe with four columns: Rate identifier, Year,
#' Total number of starting individuals, Number growing (or surviving).
#' @param grades}{number of different levels of means and variances to try, default is 1000
#' @param maxvar}{maximum variance to search over, default is 0.20. The maximum
#' possible is 0.25 and searching a narrower range will improve the accuracy of the answer.
#' @param minvar}{minimum variance to search, default is 0.00001
#' @param maxmean}{maximum limit on the mean values to search, default 1
#' @param minmean}{minimum limit on the mean values to search, default 0.01
#'
#' @return  A list with estimates and confidence intervals
#' \item{est}{a matrix with 5 columns: (1) estimated mean, (2) Kendall's MLE mean,
#' (3) estimated variance, (4) Kendall's MLE variance, (5) Kendall's unbiased MLE variance.}
#' \item{ci}{ a matrix with  95\% confidence limits for the Kendall's mean and
#' unbiased variance estimates with 4 columns: (1) low and (3) high mean limits,
#' (3) low and (4) high variance limits.}
#'
#' @note may deliver warning messages of: 'no finite arguments to min; returning Inf',
#' indicating use of very low values for variance, but this is not a malfunction.
#'
#' @references Kendall, B. E. 1998. Estimating the magnitude of environmental
#' stochasticity  in survivorship data. Ecological Applications 8(1): 184-193.
#'
#' Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
#' biology: Theory and practice of population viability analysis. Sinauer,
#' Sunderland, Massachusetts, USA.
#'
#' @seealso \code{\link{varEst}}
#'
#' @author Adapted to R from Morris \& Doak (2002: 267-270) by Patrick Nantel.
#'
#' @examples
#' ## desert tortoise input from Box 8.2 - compare results to Table 8.3
#' tor <- data.frame(rate=rep(c("g4","g5","g6"), each=3),
#'    year=rep(1:3,3),      ## representing 70s, early 80s, late 80s
#'    start=c(17,15,7,22,19,4,32,31,10),
#'    grow=c(8,1,0,5,5,0,2,1,0)
#' )
#' ## use fewer grades for faster loop
#' tor.est<-Kendall(tor, grades=200)
#' tor.est
#' wp.est <- Kendall(woodpecker, grades=200)
#' wp.est
#'
#' @export

Kendall <- function(rates, grades = 1000, maxvar = 0.2, minvar = 0.00001, maxmean = 1, minmean = 0.01) {
  ## number of different years or time intervals and rate ids in dataset
  times <- length(unique(rates[, 2]))
  classes <- unique(rates[, 1])
  n <- length(classes)
  results <- matrix(rep(0, 5 * n),
    nrow = n,
    dimnames = list(classes, c("mean", "MLE.mean", "var", "MLE.var", "cor.MLE.var"))
  )
  resultslik <- matrix(rep(0, 4 * n),
    nrow = n,
    dimnames = list(classes, c("low.mean", "hi.mean", "low.var", "hi.var"))
  )
  # makes all the sets of mean values to search over:
  means <- t(matrix(seq(minmean, maxmean, length = grades), grades, grades))
  # makes all the sets of variance values to search over:
  vars <- matrix(seq(minvar, maxvar, length = grades), grades, grades)
  # using the means and variances to compute the a and b
  # (aa and bb) parameters of a beta distribution
  aa <- means * (((means * (1 - means)) / vars) - 1)
  bb <- (1 - means) * (((means * (1 - means)) / vars) - 1)
  # eliminate impossible combinations of mean and variance values
  aa[aa <= 0] <- NaN
  bb[bb <= 0] <- NaN
  for (clas in 1:n) # loop through each rate or class
  {
    message("Computing estimates for rate ", classes[clas])
    minrow <- (clas - 1) * times + 1 # find the min and max rows of the data matrices to use
    maxrow <- clas * times
    dat <- rates[minrow:maxrow, ] # fetch the data to use
    estmn <- mean(dat[, 4] / dat[, 3], na.rm = TRUE) # raw estimate of mean - excluding double zeros
    estvar <- var(dat[, 4] / dat[, 3], na.rm = TRUE) # raw est. of variance  - excluding double zeros
    # As is often done, use the negative of log liklihoods,
    # or -log likelihoods, in the following computations.
    loglikli <- matrix(0, grades, grades) # initialize -loglikelihoods
    for (tt in 1:times) # calculate -loglikelihood for each year
    {
      # this uses the -log-likelihood formula from Kendall:
      newlogL <- -log(beta(aa + dat[tt, 4], dat[tt, 3] - dat[tt, 4] + bb) / beta(aa, bb))
      loglikli <- newlogL + loglikli # add up the -log-likelihoods for each year
    }
    # the lines below summarize the results
    minLL <- min(loglikli, na.rm = TRUE) # what is the best log-liklihood?
    minvars <- apply(loglikli, 2, min, na.rm = TRUE) # finding best var
    ii <- match(minvars, loglikli)
    minii <- min(minvars, na.rm = TRUE)
    jj <- which.min(minvars)
    MLEvar <- vars[ii[jj]]
    minvars <- apply(loglikli, 1, min, na.rm = TRUE) # finding best mean
    ii <- match(minvars, loglikli)
    minii <- min(minvars, na.rm = TRUE)
    jj <- which.min(minvars)
    MLEmean <- means[ii[jj]]
    clLL <- loglikli - minLL # differences from best -LL
    hivar <- max(vars[clLL < 3.0], na.rm = TRUE) # confidence limits for var
    lowvar <- min(vars[clLL < 3.0], na.rm = TRUE)
    himean <- max(means[clLL < 3.0], na.rm = TRUE) # confidence limits for mean
    lowmean <- min(means[clLL < 3.0], na.rm = TRUE)
    # perform correction for max. likelihood estimate of variance
    # (see Kendall 1998 for explanation)
    corrMLEvar <- MLEvar * times / (times - 1)
    corrlowvar <- lowvar * times / (times - 1)
    corrhivar <- hivar * times / (times - 1)
    # storing the results
    results[clas, ] <- c(estmn, MLEmean, estvar, MLEvar, corrMLEvar)
    resultslik[clas, ] <- c(lowmean, himean, corrlowvar, corrhivar)
  }
  Kendall <- list(est = results, ci = resultslik)
  Kendall
}
