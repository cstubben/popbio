#' Bootstrap observed census transitions
#'
#' Calculate bootstrap distributions of population growth rates (lambda), stage
#' vectors, and projection matrix elements by randomly sampling with replacement
#' from a stage-fate data frame of observed transitions
#'
#' @param transitions a stage-fate data frame with stage or age class in the
#' current census,  fate in the subsequent census, and one or more fertility
#' columns
#' @param iterations Number of bootstrap iterations
#' @param by.stage.counts Resample transitions with equal probability (default)
#' or by subsets of initial stage counts
#' @param \dots additional options passed to \code{\link{projection.matrix}}
#'
#' @return A list with 3 items
#'   \item{lambda}{A vector containing bootstrap values for lambda}
#'   \item{matrix}{A matrix containing bootstrap transtion matrices with one
#' projection matrix per row.}
#'   \item{vector}{A matrix containing bootstrap stage vectors with one stage
#' vector per row.}
#'
#' @references see Morris and Doak 2005 in \url{http://esapubs.org/Archive/mono/M075/004/appendix-A.htm}
#' for resampling by stage class counts
#'
#' @seealso \code{\link{projection.matrix}}
#'
#' @author Chris Stubben
#'
#' @examples
#' ## create stage-fate dataframe using merge and subset
#' trans01 <- subset(
#'              merge(test.census, test.census, by="plant", sort=FALSE),
#'                      year.x==2001 & year.y==2002)
#' ## format column and row names
#' trans01 <- trans01[,c(1:4,6)]
#' colnames(trans01)[2:5] <- c("year", "stage", "fruits", "fate")
#' rownames(trans01) <- 1:nrow(trans01)
#' # order stage columns corresponding to matrix
#' trans01$stage <- ordered(trans01$stage,
#'                                   levels = c("seedling", "vegetative", "reproductive"))
#' ## add individual fertilities using prebreeding census with no seed bank
#' ##  based on the proportional reproductive outputs of flowering plants
#' ## and the total number of seedlings at the end of the projection interval
#' seedlings <- nrow(subset(test.census, year==2002 & stage=="seedling"))
#' trans01$seedling <- trans01$fruits/sum(trans01$fruits) * seedlings
#' trans01
#' ## Step by step instructions for bootstrapping dataframe
#' n <- nrow(trans01)
#' n
#' set.seed(77)
#' x <- sample(n, replace=TRUE)
#' x
#' bt <- trans01[x,]
#' bt
#' projection.matrix(bt)
#' ## or respample by stage class counts
#'  lapply(split(trans01, trans01$stage, drop=TRUE),
#'       function(x) x[sample(nrow(x), replace=TRUE),])
#' ## using boot.transitions
#' boot.transitions(trans01, 5)
#' boot.transitions(trans01, 5, by.stage=TRUE)
#' ## Aquilegia example
#' x <- subset(aq.trans, year==1996)
#' # calculate lamda, seed survival and recruitment rate using aq.matrix
#' rec <- nrow(subset(aq.trans, year==1997 & stage == "recruit"))
#' aq.96 <-  aq.matrix(x, rec)
#' # add  individual fertilities to data frame only
#' aq.96.trans <- aq.matrix(x, rec, summary=FALSE)
#' # pass estimated transitions in aq.96 to projection matrix
#' aq.96.boot <- boot.transitions(aq.96.trans, 200,
#'             add=c(1,1, aq.96$seed.survival, 2,1, aq.96$recruitment.rate) )
#' # calculate percentile intervals using quantile()
#' ci <- quantile(aq.96.boot$lambda, c(0.025,0.975) )
#' aq.96$lambda
#' ci
#' # plot histogram
#' hist(aq.96.boot$lambda, col="green", xlab="Lambda",
#'         main=paste('Bootstrap estimates of population\ngrowth rate from 1996-1997'))
#' abline(v=ci, lty=3)
#'
#' @export

boot.transitions <- function(transitions, iterations, by.stage.counts = FALSE, ...) {
  ## check orderd fate, stage, and one or more fertility columns?
  t <- iterations
  mat <- vector("list", t) ## initialize a list to store matrices
  vec <- vector("list", t) ## and stage vectors
  lam <- numeric(t) ## and a vector for lambdas
  for (i in 1:t) {
    if (by.stage.counts) {
      ## create new data frame with resampled transitions by counts in original class vector
      boot <- do.call(
        rbind,
        lapply(
          split(transitions, transitions$stage, drop = TRUE),
          function(x) x[sample(nrow(x), replace = TRUE), ]
        )
      )
    } else {
      boot <- transitions[sample(nrow(transitions), replace = TRUE), ]
    }
    A <- projection.matrix(boot, ...)
    # Nov 2011 - fixed bug noted by Tristan Lemke - if some stages are NA, they
    # will be included in the summary below and return an error when creating
    # final list using rownames(A).
    # vec[[i]] <- summary(boot$stage)
    vec[[i]] <- table(boot$stage)
    mat[[i]] <- as.vector(A)
    lam[i] <- lambda(A)
  }
  n <- dim(A)[1]
  boot.stage <- list(
    lambda = lam,
    matrix = matrix(unlist(mat),
      byrow = TRUE, nrow = t,
      dimnames = list(1:t, paste("a", 1:n, rep(1:n, each = n), sep = ""))
    ),
    vector = matrix(unlist(vec), byrow = TRUE, nrow = t, dimnames = list(1:t, names(vec[[1]])))
  )
  boot.stage
}
