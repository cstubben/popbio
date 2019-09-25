#' Create a projection matrix for Aquilegia
#'
#' Creates a projection matrix for \emph{Aquilegia} from annual transition data,
#' assuming new seeds and seed bank seeds have an equal chance for successful
#' germination and equal survival rates.
#'
#' Adds individual fertilites to annual transitions using a prebreeding census.
#'
#' @param trans A data frame with transitions listing \code{\link{ordered}}
#' stages and fates and counts of mature fruits
#' @param recruits The number of observed recruits in year \code{t + 1}.
#' @param summary Output projection matrix and summaries.  Otherwise output
#' transition table with added individual fertilities.
#' @param seed.survival Estimated seed survival rate for both new seeds and seed
#' bank.  Default is 12.6 percent survival.
#' @param seed.bank.size Estimated size of the seed bank.  Seed bank and new
#' seeds contribute to a common germinant pool with equal chance for
#' germination. Default is 10,000 seeds in seed bank.
#' @param seeds.per.fruit The number of seeds produced per mature fruit.
#' Default is 120 seeds.
#' @param \dots additional arguments passed to \code{\link{projection.matrix}}
#'
#' @return If summary is TRUE, a list with
#'   \item{recruits}{total number of recruits}
#'   \item{seed.survival}{seed survival rate}
#'   \item{seed.bank}{total number of seeds in seed bank }
#'   \item{seeds.from.plants}{ total number of new seeds just released from fruits }
#'   \item{recruitment.rate}{ recruitment rate calculated as recruits/(seed.bank.size + seeds.from.plants)}
#'   \item{A}{projection matrix}
#'   \item{lambda}{population growth rate}
#'   \item{n}{initial population vector}
#'   \item{n1}{final population vector}
#' If summary is FALSE, a data frame with individual fertilities added
#'   to the transition data frame only.
#'
#' @seealso \code{\link{projection.matrix}}
#'
#' @author Chris Stubben
#'
#' @examples
#' x <- subset(aq.trans, year==1996)
#' ## number of recruits in 1997
#' rec <- nrow(subset(aq.trans, year==1997 & stage == "recruit"))
#' aq.matrix(x, recruits=rec)
#' aq.matrix(x, recruits=rec, seed.survival=.7, seed.bank=3000)
#'
#' @export

aq.matrix <- function(trans, recruits, summary = TRUE, seed.survival = 0.126,
  seed.bank.size = 10000, seeds.per.fruit = 120, ...) {
  x <- trans
  seeds.from.plants <- sum(x$fruits) * seeds.per.fruit
  ## assume seeds in seed bank and new seeds have equal chance for successful germination
  recruitment.rate <- recruits / (seed.bank.size + seeds.from.plants)
  ## add fertilities
  x <- cbind(x, recruit = x$fruits / sum(x$fruits) * seeds.from.plants * recruitment.rate)
  x <- cbind(x, seed = x$fruits * seeds.per.fruit * seed.survival)
  if (summary) {
    ## STAGE vector
    n <- summary(x$stage)
    n["seed"] <- seed.bank.size
    ## matrix
    A <- projection.matrix(x, add = c(1, 1, seed.survival, 2, 1, recruitment.rate), ...)
    lam <- max(Re(eigen(A)$values))
    n1 <- A %*% n
    ## format same as n
    n1 <- as.vector(n1)
    names(n1) <- names(n)
    z <- list(
      recruits = recruits,
      seed.survival = seed.survival,
      seed.bank = seed.bank.size,
      seeds.from.plants = seeds.from.plants,
      recruitment.rate = recruitment.rate,
      A = A,
      lambda = lam,
      n = n,
      n1 = round(n1, 0)
    )
    z
  }
  else {
    x
  }
}
