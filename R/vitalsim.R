#' Stochastic vital rate simulations
#'
#' Calculates the extinction time CDF and stochastic growth rate by sampling vital
#' rates from a beta, stretched beta, or lognormal distribution and includes
#' within-year, auto- and cross-correlations
#'
#' Vital rates used must be either fertility values or binomial probabilities,
#' i.e., probabilities for events with only two possible outcomes (such as
#' survival). Means and variances of the vital rates should preferably be
#' corrected to remove sampling errors and demographic stochasticity.  Note that
#' this version of the function does not simulate demographic stochasticity and
#' is density-independent.
#'
#' @param vrmeans means of vital rates
#' @param vrvars variance of vital rates
#' @param corrin within year correlation
#' @param corrout between year correlations
#' @param makemx a function that creates a square projection matrix from a
#' vector of \code{vrmeans}
#' @param n0 initial population vector
#' @param yrspan the number of years of correlations to build into the M12 matrix
#' @param Ne  quasi-extinction threshold
#' @param tmax latest time to calculate extinction probability, default 50
#' @param runs the number of trajectories, default is 500.  1000 is recommended
#' @param vrtypes identifies the distribution for each rate in vrmeans where
#' 1 =beta, 2 = stretched beta, 3 = lognormal, default is all ones
#' @param vrmins minimum value for each vital rate; use zeros for rates that are
#' not stretched betas, default is all zeros
#' @param vrmaxs maximum value for each vital rate; use zeros for rates that are
#' not stretched betas, default is all zeros
#' @param sumweight a vector of weights, with 0 to omit a class and 1 to include
#' it when computing the summed density to compare to the quasi-extinction
#' threshold, default is to include all classes
#'
#' @return Plots a histogram of log stochastic growth rates and the cumulative
#' probability of quasi-extinction and returns a list with 4 items:
#' \item{detLambda}{the deterministic population growth rate computed from
#' the mean matrix}
#' \item{stochlambda}{the mean stochastic growth rate with 95\% confidence intervals.}
#' \item{logLambdas}{ a vector of all log stochastic growth rates in first plot}
#' \item{CDFExt}{a vector of cumulative probabilities of quasi-extinction in second plot}
#'
#' @note The correlation matrices for \emph{Hudsonia} in Morris and Doak 2002
#' include some correlations > 1.  A corrected set of correlations was sent by
#' D. Doak on 8/4/2007.  Therefore the results from the simulation below are
#' different than the book.
#'
#' @seealso \code{\link{hudmxdef}}, \code{\link{hudvrs}} and \code{\link{hudcorrs}}
#'
#' @author Original MATLAB code from Box 8.10 in Morris and Doak (2002).
#' Adapted to R by Patrick Nantel, 12 July 2005
#'
#' @examples
#' ## load vital rates and correlation matrices
#' data(hudvrs)
#' data(hudcorrs)
#' ## set vrtypes
#' hudvrtypes <- c(rep(1,13), rep(3,5), rep(1,6))
#' ## run Full model- using 100 runs here for speed
#' full <- vitalsim(hudvrs$mean, hudvrs$var, hudcorrs$corrin,
#'  hudcorrs$corrout, hudmxdef, vrtypes=hudvrtypes,
#'  n0=c(4264,3,30,16,25,5), yrspan=20 , runs=100)
#' ## deterministic and stochastic lambda
#' full[1:2]
#' ## log stochastic lambda
#' log(full$stochLambda)
#' sd(full$logLambdas)
#' ## SKIP the next two simulations- however, sample output is included for plotting
#' #NO between year correlations so corrout = diag(0,13)  - all zeros
#' # no.between <- vitalsim(hudvrs$mean, hudvrs$var, hudcorrs$corrin,
#' # diag(0,13), hudmxdef, vrtypes=hudvrtypes,
#' # n0=c(4264,3,30,16,25,5), yrspan=20 )
#' no.between <- list(CDFExt=c(rep(0,40),0.01,0.04,0.12,0.15,
#' 0.20,0.31,0.49,0.58,0.72,0.78))
#' #NO correlations so corrout = diag(0,13) AND corrin=diag(13) - ones on diagonal
#' # no.corr<-vitalsim(hudvrs$mean, hudvrs$var, diag(13),
#' # diag(0,13), hudmxdef, vrtypes=hudvrtypes,
#' # n0=c(4264,3,30,16,25,5), yrspan=20 )
#' no.corr <- list(CDFExt=c(rep(0,39),0.03,0.03,0.06,0.12,0.20,
#' 0.30,0.42,0.52,0.65,0.76,0.83))
#' ## Figure 8.3 with corrected correlation matrices for full model
#' matplot(cbind(a=full$CDFExt, no.between$CDFExt, no.corr$CDFExt), type='l',
#'  ylim=c(0,1), lty=1:3, col=2:4, lwd=2, las=1,
#'  xlab="Years into the future", ylab="Cumulative probability of quasi-extinction")
#' legend(2,1, c("Full model", "No between-year correlations", "No correlations"),
#'  lty=1:3, col=2:4, lwd=2)
#'
#' @export

vitalsim <- function(vrmeans, vrvars, corrin, corrout, makemx, n0, yrspan, Ne = 500,
  tmax = 50, runs = 500, vrtypes = NULL, vrmins = NULL, vrmaxs = NULL, sumweight = NULL) {
  x1 <- length(vrmeans)
  x2 <- length(n0)
  x3 <- dim(corrin)[1]
  ## calcualte np and np2
  np <- x3
  np2 <- x1 - x3
  np3 <- np * yrspan ## CORRECTION in email from Doak 8/4/07
  ## create some reasonable defaults if not specified
  if (missing(vrmins)) {
    vrmins <- rep(0, x1)
  }
  if (missing(vrmaxs)) {
    vrmaxs <- rep(0, x1)
  }
  if (missing(vrtypes)) {
    vrtypes <- rep(1, x1)
  } # 1 = beta, 2 = stretched beta, 3 = lognormal
  if (missing(sumweight)) {
    sumweight <- rep(1, x2)
  }
  ## some error checking -- could add more
  if (length(vrtypes) != x1) {
    stop("vrtypes is not the same length as vrmeans!", call. = FALSE)
  }
  if (length(vrmins) != x1) {
    stop("vrmins is not the same length as vrmeans!", call. = FALSE)
  }
  if (length(vrmaxs) != x1) {
    stop("vrmaxs is not the same length as vrmeans!", call. = FALSE)
  }
  if (length(sumweight) != x2) {
    stop("sumweight is not the same length as n0!", call. = FALSE)
  }
  vrs <- vrmeans
  meanmx <- makemx(vrs) # create mean matrix using function
  #
  Nstart <- sum(n0) # starting population number
  lam0 <- lambda(meanmx) # find the deterministic population growth rate (mean matrix)
  #-------------------------------------------------------------------------------
  # this section makes sets of beta or strecthed beta values to choose
  # from during the simulations. It makes 99 values for 1
  # increments of Fx for each parameter
  message("Generating beta distributed values for vital rates")
  parabetas <- matrix(0, 99, np + np2)
  for (ii in 1:(np + np2))
  {
    if (vrtypes[ii] != 3) {
      for (fx99 in 1:99)
      {
        if (vrtypes[ii] == 1) {
          parabetas[fx99, ii] <- betaval(vrmeans[ii], sqrt(vrvars[ii]), fx99 / 100)
        }
        if (vrtypes[ii] == 2) {
          parabetas[fx99, ii] <- stretchbetaval(
            vrmeans[ii], sqrt(vrvars[ii]),
            vrmins[ii], vrmaxs[ii], fx99 / 100
          )
        }
      }
    }
  }
  #--------creating and using the big correlation matrix, M--------
  # this set of loops makes the big correlation matrix (M)
  # with multi-year correlations: the if statements are used to
  # estimate the correct correlations with increasing time lags,
  # always assuming that all long-time-lag correlations are only
  # caused by within-year and one-time-step correlations
  #
  # need function for simple matrix powers
  "%^%" <- function(mat, pow) {
    result <- diag(nrow(mat))
    while (pow > 0) {
      result <- result %*% mat
      pow <- pow - 1
    }
    result
  }
  message("Calculating the multi-year correlation matrix")
  M <- matrix(, yrspan * np, yrspan * np) # initialize the big correlation matrix (M)
  for (ii in 1:yrspan)
  {
    for (jj in 1:yrspan)
    {
      if (ii == jj) {
        litmx <- corrin
      }
      else {
        litmx <- corrout
      }
      expo <- 1
      if (ii > jj) {
        expo <- ii - jj
        litmx <- litmx %^% expo
      }
      if (ii < jj) {
        expo <- jj - ii
        litmx <- (t(litmx)) %^% expo
      }
      for (ip in 1:np)
      {
        for (jp in 1:np)
        {
          M[(ip + np * (ii - 1)), (jp + np * (jj - 1))] <- litmx[ip, jp]
        }
      }
    }
  }
  # get the eigenvalues for calculating the M12 matrix
  ev <- eigen(M)
  d <- ev$val
  W <- ev$vec # eigenvalues (diagonal matrix) and vectors
  o <- rev(order(Mod(d))) # re-order by largest real part to match Matlab??
  d <- d[o]
  W <- W[, o]
  ## NOTE: this section will not be evaluated using matlab code in Box 8.10
  ##  since min(d) will always be > 0 if computed using complex input..
  checkeig <- min(d) # check for negative eigenvalues
  if (checkeig < 0) {
    message("Correcting negative eigenvalues")
    maxneg <- abs(min(d[d < 0])) # the largest negative eigenvalue
    d[d <= maxneg] <- 0 # sets negatives and small positive values = 0
    d <- diag(d)
    newfullmx <- W %*% d %*% t(W) # make a corrected matrix
    for (ii in 1:np3) # CORRECTION in email 8/4/07
    { ## change from covariances to correlations
      for (jj in 1:np3)
      {
        if (newfullmx[ii, ii] == 0 | newfullmx[jj, jj] == 0) {
          newfullmx[ii, jj] <- 0
        }
        else {
          newfullmx[ii, jj] <- newfullmx[ii, jj] / ((newfullmx[ii, ii] * newfullmx[jj, jj])^0.5)
        }
      }
    }
    ev <- eigen(newfullmx)
    d <- ev$val
    W <- ev$vec
    o <- rev(order(Mod(d)))
    d <- d[o]
    W <- W[, o]
  }
  d <- diag(d)
  M12 <- W %*% (abs(d)^0.5) %*% t(W) # the M^(1/2) matrix
  sz <- nrow(M12) # the total number of lines in M12
  # get the lines from the middle of M12 to use to generate correlations
  startcase <- round(yrspan / 2) * np + 1 #
  zvalold <- Re(M12[startcase:(startcase + np - 1), ])
  zvalnew <- Re(M12[ (startcase + np):(startcase + 2 * np - 1), ])
  newns <- matrix(rnorm(sz), ncol = 1) #
  oldxy <- zvalold %*% newns #
  #-----end of: creating and using the big correlation matrix------
  #
  # Runs to get growth rate and extinction risk
  message("Running projections to get growth rate and extinction risk")
  normresults <- c()
  PrExt <- matrix(0, tmax, 1) # the extinction time tracker
  logLam <- matrix(0, runs, 1) # the tracker of log-lambda values
  stochLam <- matrix(0, runs, 1) # tracker of stochastic lambda values
  for (xx in 1:runs)
  {
    if (xx == 1 || xx %% 10 == 0) {
      message("  Starting run ", xx)
    }
    nt <- n0 # start at initial population vector
    extinct <- 0
    for (tt in 1:tmax)
    {
      newns <- matrix(newns[(np + 1):sz, ], (sz - np), 1) # make random normals
      newns <- rbind(newns, matrix(rnorm(np), np, 1))
      newxy <- zvalnew %*% newns # make new set of correlated normals
      # these lines save normals to check correlations :
      normresults <- rbind(normresults, cbind(t(oldxy), t(newxy)))
      oldxy <- newxy
      # adds in randoms for uncorrelated vital rates.
      yrxy <- rbind(newxy, matrix(rnorm(np2), np2, 1))
      # find vital rate values
      vrs <- matrix(0, np + np2) # initialize vrs
      yrxy1 <- matrix(yrxy[vrtypes != 3]) # if not a lognormal rate
      yrxy2 <- matrix(yrxy[vrtypes == 3]) # if lognormal rate
      index <- c() # initilize index
      index <- round(100 * pnorm(yrxy1, 0, 1, TRUE, FALSE))
      index[index == 0] <- 1 # round at extremes
      index[index == 100] <- 99
      vrs[vrtypes != 3] <- diag(parabetas[index, vrtypes != 3]) # find stored value
      # calculate a lognormal value
      # vrs[vrtypes == 3]   <- lnorms(vrmeans[vrtypes == 3],vrvars[vrtypes == 3], yrxy2)
      vrs[vrtypes == 3] <- lnorms(yrxy2, vrmeans[vrtypes == 3], vrvars[vrtypes == 3]) # updated lnorms
      mx <- makemx(vrs) # use matrix definition function to make yearly matrix
      nt <- mx %*% nt # multiply matrix by the population vector
      if (extinct == 0) # check for extinction
      {
        Ntot <- sumweight %*% nt # compute weighted sum of current densities
        if (Ntot <= Ne) {
          PrExt[tt] <- PrExt[tt] + 1
          extinct <- 1
        }
      }
    }
    logLam[xx] <- (1 / tmax) * log(sum(nt) / Nstart) # calculate loglambda
    stochLam[xx] <- (sum(nt) / Nstart)^(1 / tmax) # and stoch. lambda  Not used??
  } # END runs loop
  CDFExt <- cumsum(PrExt / runs) # make the extinction CDF function
  loglsim <- mean(logLam) # mean of loglambda
  dse <- 1.96 * apply(logLam, 2, sd) # standard error of loglambda
  CL1 <- c(loglsim - dse, loglsim + dse) # approx. 95% confidence interval
  lamsim <- exp(mean(logLam)) # simulated stochastic growth rate
  CL2 <- exp(CL1)
  ## PLOT data
  op <- par(mfrow = c(1, 2))
  hist(logLam,
    xlab = "Log stochastic growth rate",
    col = "blue", main = expression(paste("Histogram of log ", lambda[s])), las = 1
  )
  abline(v = CL1, lty = 2)
  plot(CDFExt,
    type = "p", pch = 16, col = "blue", ylim = c(0, 1), las = 1,
    main = expression("Extinction CDF"),
    xlab = "Years into the future",
    ylab = "Cumulative probability of quasi-extinction"
  )
  par(op)
  vitalsim <- list(
    detLambda = lam0,
    stochLambda = c(lambda = lamsim, lc = CL2[1], uc = CL2[2]),
    logLambdas = c(logLam),
    CDFExt = CDFExt
  )
  vitalsim
}
