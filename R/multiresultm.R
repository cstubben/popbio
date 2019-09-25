#' Incorporate demographic stochasticity into population projections
#'
#' Generates multinomial random numbers for state transitions and lognormal or
#' binomial (for clutch size=1) random numbers for fertilities  and returns a
#' vector of the number of individuals per stage class at \emph{t+1}.
#'
#' Adapted from Matlab code in Box 8.11 in Morris and Doak (2002) and section
#' 15.1.3 in Caswell (2001)
#'
#' @param n the vector of numbers of individuals per class at t
#' @param T a transition T matrix
#' @param F a fertility F matrix
#' @param varF a matrix of inter-individual variance in fertilities, default is
#' NULL for simulating population where clutch size = 1, so that fertilities
#' give the probabilities of birth
#'
#' @return a vector of the number of individuals per class at t+1.
#'
#' @references Caswell, H. 2001. Matrix population models: construction,
#' analysis, and interpretation, Second edition. Sinauer, Sunderland,
#' Massachusetts, USA.
#' 
#' Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
#' biology: Theory and practice of population viability analysis. Sinauer,
#' Sunderland, Massachusetts, USA.
#'
#' @author Patrick Nantel
#'
#' @examples
#' data(whale)
#' x <- splitA(whale)
#' whaleT <- x$T
#' whaleF <- x$F
#' multiresultm(c(1,9,9,9),whaleT, whaleF)
#' multiresultm(c(1,9,9,9),whaleT, whaleF)
#' ## create graph similar to Fig 15.3 a
#' reps <- 10    # number of trajectories
#' tmax <- 200   # length of the trajectories
#' totalpop <- matrix(0,tmax,reps)  # initializes totalpop matrix to store trajectories
#' nzero <- c(1,1,1,1) # starting population size
#' for (j in 1:reps) {
#'    n <- nzero
#'    for (i in 1:tmax) {
#'       n <- multiresultm(n,whaleT,whaleF)
#'       totalpop[i,j] <- sum(n)
#'    }
#' }
#' matplot(totalpop, type = 'l', log="y",
#'         xlab = 'Time (years)', ylab = 'Total population')
#'
#' @export

multiresultm <- function(n, T, F, varF = NULL) {
  # First, determine numbers from survival and growth
  clas <- length(n) # number of classes
  death <- 1 - colSums(T) # vector of death rates
  T <- rbind(T, death) # append death rates to matrix T
  outcome <- matrix(0, nrow(T), clas) # initialize matrix "outcome"
  ## see Box 8.11 in Morris and Doak
  for (j in 1:clas) {
    Tj <- T[, j] # extract column j of in matrix T
    ni <- n[j] # extract entry j of vector n
    ind <- matrix(1, 1, ni)
    pp <- cumsum(Tj / sum(Tj)) # find cumulative probabilities
    rnd <- runif(ni, min = 0, max = 1) # make a uniform random for each individual
    for (ii in 1:length(Tj)) { # find each individual's random fate
      ind <- ind + (rnd > pp[ii])
    } # end for ii
    for (ii in 1:length(Tj)) { # add up the individuals in each fate
      outcome[ii, j] <- sum(ind == ii)
    } # end for ii
  } # end for j
  # Second, determine numbers of offspring
  if (length(varF) != 0) { # if there is a matrix of inter-individual
    # variance of fertilities
    # This routine generates lognormal numbers from mean fertilities and their variance.
    offspring <- matrix(0, clas, clas) # initialize matrix "offspring"
    for (j in 1:clas) {
      fj <- F[, j] # extract column j of matrix F
      if (max(fj) > 0) { # skip loop if there is no fertility
        ni <- n[j] # extract entry j of vector n
        for (i in 1:length(fj)) {
          if (F[i, j] > 0) { # skip if fertility is null

            # rndfert  <- lnorms(F[i,j],varF[i,j],rnorm(ni,0,1)) # make lognormal random fertilities
            rndfert <- lnorms(ni, F[i, j], varF[i, j]) # updated lnorms in version 2.0
            offspring[i, j] <- sum(rndfert) # computes number of offsprings from fertilities
          } # end if
        } # end for i
      } # end if
    } # end for j
  } # end if
  else {
    # This routine generates binomial random births from mean fertilities,
    # for population with clutch size = 1.
    offspring <- matrix(0, clas, clas) # initialize matrix "offspring"
    for (j in 1:clas) {
      fj <- F[, j] # extract column j of matrix F
      if (max(fj) > 0) { # skip loop if there is no fertility
        ni <- n[j] # extract entry j of vector n
        for (i in 1:length(fj)) {
          if (F[i, j] > 0) { # skip if fertility is null
            rndbirth <- rbinom(ni, 1, F[i, j]) # make binomial random births
            offspring[i, j] <- sum(rndbirth) # computes number of offspring from random births
          } # end if
        } # end for i
      } # end if
    } # end for j
  } # end else
  multiresultm <- matrix(rowSums(offspring) + rowSums(outcome[1:clas, ]), clas, 1, dimnames = list(rownames(F), "t+1"))
  multiresultm
}
