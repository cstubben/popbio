#' Estimate the variance of beta-binomial vital rates
#'
#' Finds the best estimates of mean and environmental variance for beta-binomial
#' vital rates using the approximation method of Akcakaya (2002)
#'
#' @param rates a matrix or dataframe with four columns: Rate identifier, Year,
#' Total number of starting individuals, Number surviving (or growing)
#' @param weighted either 1 for weighted average demographic variance, or 0 for
#' unweighted average, default is 1
#'
#' @return A matrix with 3 columns: (1) total observed variance, (2) estimate of
#' variance due to demographic stochasticity, and (3) estimate of variance due
#' to environmental stochasticity.
#'
#' @references Akcakaya, H. R. 2002. Estimating the variance of survival rates
#' and fecundities. Animal Conservation 5: 333-336.
#'
#' Kendall, B. E. 1998. Estimating the magnitude of environmental stochasticity
#' in survivorship data. Ecological Applications 8(1): 184-193.
#'
#' @seealso \code{\link{Kendall}}
#'
#' @author Patrick Nantel, 20 June 2005. Last modified May 1st 2007.
#'
#' @examples
#' data(woodpecker)
#' varEst(woodpecker)
#'
#' @export

varEst <- function(rates, weighted = 1) {
  times <- length(unique(rates[, 2]))
  classes <- unique(rates[, 1])
  n <- length(classes)
  results <- matrix(rep(0, 3 * n),
    nrow = n,
    dimnames = list(classes, c("total.var", "demo.var", "env.var"))
  )
  for (rate in 1:n)
  {
    minrow <- (rate - 1) * times + 1
    maxrow <- rate * times
    N <- rates[minrow:maxrow, 3]
    m <- rates[minrow:maxrow, 4]
    p <- m / N
    if (weighted == 0) {
      demvar <- sum((p * (1 - p)) / N) / times # equ.2 of Akcakaya 2002: unweighted mean of demographic variance
      totalvar <- var(p, na.rm = TRUE) # unweighted total variance of the vital rate
    }
    else {
      demvar <- sum(p * (1 - p)) / sum(N) # equ.3 of Akcakaya 2002: weighted average demographic variance
      wmeanp <- sum(m) / sum(N) # weighted mean of the vital rate
      totalvar <- sum(N * (p - wmeanp)^2) / sum(N) # weigthed total variance (Kendall 1998, eq. 1)
    }
    envstochvar <- totalvar - demvar
    results[rate, ] <- c(totalvar, demvar, envstochvar)
  }
  results
}
