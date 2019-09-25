#' Stretched beta-distributed random numbers
#'
#' Generate a stretched beta number with mean, standard deviation, minimum and
#' maximum values and CDF value for bounded fertility estimates
#'
#' converted Matlab code from Box 8.5 in Morris and Doak (2002)
#'
#' @param mn mean of a fertility rate
#' @param std standard deviation
#' @param minb minimum value
#' @param maxb maximum value
#' @param fx Cumulative Distribution Function value
#'
#' @return  Returns a stretched beta number with mean mn, standard deviation std,
#' minimum and maximum values (minb, maxb) and CDF value fx.
#'
#' @references Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
#' biology: Theory and practice of population viability analysis. Sinauer,
#' Sunderland, Massachusetts, USA.
#'
#' @seealso \code{\link{betaval}}
#'
#' @author Adapted to R by Patrick Nantel, 11 July 2005.
#'
#' @examples
#' stretchbetaval(3, 1.2, 1, 20, runif(1))
#' # Generates stretchbeta random
#' # fertilities for a population of 1000 mature individuals (Ni) with mean
#' # fertility (f) of 3.0 and inter-individual variance in fertility (varF) of 1.5.
#' Ni   <- 1000
#' f    <- 2.5
#' varF <- 1
#' fmin <- 1
#' fmax <- 5
#' rndfert<-numeric(Ni)
#' for(i in 1:Ni) rndfert[i] <- stretchbetaval(f, sqrt(varF), fmin, fmax, runif(1)) 
#' hist(rndfert,20, main="Stretched beta-distributed random fertilities",
#'  xlab="Fertility rate", , col="blue")
#'
#' @export

stretchbetaval <- function(mn, std, minb, maxb, fx) {
  if (std == 0) {
    bb <- mn
  } # with no variation, then the value = mean
  else {
    # convert the stretched beta parameters to corresponding
    # ones for a {0,1} beta
    mnbeta <- (mn - minb) / (maxb - minb)
    sdbeta <- std / (maxb - minb)
    # next, check for un-doable parameter combos
    if (sdbeta < (mnbeta * (1 - mnbeta))^0.5) {
      bvalue <- betaval(mnbeta, sdbeta, fx) # find beta value
      bb <- bvalue * (maxb - minb) + minb # convert to stretched value
    }
    else {
      maxsd <- ((mnbeta * (1 - mnbeta))^0.5) * (maxb - minb)
      bb <- paste("WARNING: The std is too high.  The maximum std possible is:", round(maxsd, 3))
    }
  }
  bb
}
