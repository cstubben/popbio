#' Count-based extinction probabilities and bootstrap confidence intervals
#'
#' This function takes parameters derived from population counts and calculates
#' the probability of extinction with bootstrap confidence intervals for a
#' density-independent model, using a diffusion approximation.
#'
#' converted Matlab code from  Box 3.4 in Morris and Doak (2002)
#'
#' @param mu estimated value of mean mu
#' @param sig2 estimated value of sample variance
#' @param nt number of transitions in the data set
#' @param Nc current population size
#' @param Ne quasi-extinction threshold
#' @param tq length of the census (in years), default is number of transitions
#' @param tmax latest time to calculate extinction probability, default 50
#' @param Nboot number of bootstrap samples for calculating confidence intervals
#' for extinction probabilities, default 500)
#' @param plot draw extinction time CDF plot with log-scale on y-axis
#'
#' @return The function plots the cumulative probabilities of quasi-extinction
#' through time with 95\% confidence intervals. It also returns a data frame
#' with the extinction time CDF for the best parameter estimates (Gbest), and
#' the lower and upper bootstrap confidence limits for extinction probabilites
#' (Glo, Gup).
#'
#' @seealso \code{\link{extCDF}}
#'
#' @references Dennis et al. 1991, Ecological Monographs 61: 115-143
#'
#' Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
#' biology: Theory and practice of population viability analysis. Sinauer,
#' Sunderland, Massachusetts, USA.
#'
#' @author Adapted to R by Patrick Nantel, 4 May 2005, from program 'extprob'
#' of Morris and Doak (2002: 79-86)
#'
#' @examples
#' ## plot like Figure 3.8 in Morris and Doak (2002).
#' logN <- log(grizzly$N[-1]/grizzly$N[-39])
#' countCDFxt(mu=mean(logN), sig2=var(logN), nt=38, tq=38, Nc=99, Ne=20)
#'
#' @export

countCDFxt <- function(mu, sig2, nt, Nc, Ne, tq = nt, tmax = 50, Nboot = 500, plot = TRUE) {
  SEmu <- sqrt(sig2 / tq) # calculate standard error of mu
  # initialize array to store lower and upper bootstrap confidence limits
  # for extinction probabilities
  Glo <- matrix(1, tmax, 1)
  Gup <- matrix(0, tmax, 1)
  # Calculates confidence interval for mu using equation 3.12 on p. 70
  CI_mu <- c(mu - qt(0.975, nt - 1) * SEmu, mu + qt(0.975, nt - 1) * SEmu)
  # Calculates confidence interval for sigma^2
  CI_sig2 <- c((nt - 1) * sig2 / qchisq(.975, nt - 1), (nt - 1) * sig2 / qchisq(.025, nt - 1))
  # Calculate the extinction time CDF for the best parameter estimates
  Gbest <- extCDF(mu, sig2, Nc, Ne, tmax)
  Gbest <- matrix(Gbest, tmax, 1)
  # Calculate bootstrap confidence limits for extinction probabilites
  for (i in 1:Nboot)
  {
    # Generate random variates of mu and sig2 within their confidence intervals
    ##  CHANGED murnd <- mu + SEmu * rnorm(1,mu,sqrt(sig2))
    murnd <- Inf
    while (murnd < CI_mu[1] | murnd > CI_mu[2]) {
      # random number from normal distribution:
      ##  CHANGED murnd <- mu + SEmu * rnorm(1,mu,sqrt(sig2))
      murnd <- mu + SEmu * rnorm(1)
    }
    # CHANGED sig2rnd <- sig2*rchisq(1,nt-1)/(nt-1)
    sig2rnd <- Inf
    while (sig2rnd < CI_sig2[1] | sig2rnd > CI_sig2[2]) {
      # random number from chi-square distribution
      sig2rnd <- sig2 * rchisq(1, nt - 1) / (nt - 1)
    }
    # Calculate extintion probabilities given murnd and sig2rnd
    G <- extCDF(murnd, sig2rnd, Nc, Ne, tmax)
    # Store extreme values in each bootstrap interation
    # (only if value is more extreme than previous bootstrap iterations)
    for (x in 1:tmax)
    {
      if (G[x] < Glo[x]) {
        Glo[x] <- G[x]
      }
      if (G[x] > Gup[x]) {
        Gup[x] <- G[x]
      }
    }
  } # end bootstrap
  # plot CDF
  if (plot) {
    plot(Gbest,
      log = "y", type = "l", pch = 16, col = "blue",
      ylim = c(min(Gbest[Gbest != 0]), max(Gup)),
      main = "Extinction CDF",
      xlab = "Years into the future",
      ylab = "Cumulative probability of quasi-extinction"
    )
    lines(Glo, col = "red", lty = 2)
    lines(Gup, col = "red", lty = 2)
  }
  data.frame(Gbest, Glo, Gup)
}
