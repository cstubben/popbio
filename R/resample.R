#' Resample a projection matrix
#'
#' Resample a projection matrix using  a multinomial distribution for
#' transitions and a log normal distribution for fertilities
#'
#' The projection matrix A is first split into separate transition and fertility
#' matrices.  Dead fates are added to the transtion matrix and the columns are
#' then sampled from a \code{\link{Multinomial}} distribution based on the size
#' in each corresponding stage class in \code{n}. The fertility rates are
#' sampled from a Log Normal distribution using the \code{\link{lnorms}}
#' function. The variance can be a single value which is applied to all rates,
#' or vector of different values to apply to each rate.  In this case, the
#' values are recycled to match the number of non-zero fertilities.
#'
#' @param A a projection matrix
#' @param n either a stage vector with the number of transitions to sample
#' in each column or a single value  that is applied to all columns
#' @param fvar either a vector of different fertility variances or a single
#' variance of fertility (default 1.5) that is applied to all rates
#' @param \dots additional items are passed to \code{\link{splitA}}
#' and are used to split A into T and F matrices
#'
#' @return A resampled projection matrix
#'
#' @note see section 12.1.5.2 on parametric bootsrap in Caswell (2001)
#'
#' @seealso \code{\link{boot.transitions}}
#'
#' @author Chris Stubben
#'
#' @examples
#' A <- hudsonia[[1]]
#' lambda(A)
#' ## NOTE fertilities are in first two rows, so use r=1:2 for splitting this matrix
#' ## resample transitions 100 times each
#' resample(A, 100, r=1:2)
#' ## set higher fvar in stage 4 and 6
#' ## because there are two fertilities per stage (8 total), need to repeat values
#' resample(A,1000, fvar=c(1.5, 1.5, 3, 3), r=1:2)
#' ## OR resample based on number of plants surveyed
#' # data from table 6.4 and  box 7.3)
#' n <- c(4264,3, 30, 16, 24,5)
#' ## create a list with 1000 resampled matrices
#' x <- lapply(1:1000, function(x) resample(A,n, r=1:2))
#' mean(x)
#' ## use var2 to check variances, especially if  using differnt fvar values
#' var2(x)
#' ## growth rates
#' y <- sapply(x, lambda)
#' quantile( y, c(0.025, .975) )
#' hist(y, br=30, col="palegreen", xlab="Lambda", main="1985 Hudsonia growth rates")
#' abline(v=quantile(y, c(0.025, .975)), lty=3)
#' ## double the sample size (and quadruple seedlings) and you may be able to detect a decline
#' n <- n * 2
#' n[2] <- n[2] * 2
#' x <- lapply(1:1000, function(x) resample(A, n * 2, r=1:2))
#' quantile( sapply(x, lambda), c(0.025, .975) )
#'
#' @export

resample <- function(A, n, fvar = 1.5, ...) {
  # if(nrow(A) != length(n)){ rep(n, length=nrow(A)) }
  # stages <- rownames(A)
  a <- splitA(A, ...)
  # add dead fates to transitions so columns sum to 1
  a1 <- rbind(a$T, dead = 1 - colSums(a$T))
  #  add stage vector to transtions (2 extra rows )
  ## this makes it  easier to apply the number in stage 1  to the
  ## transitions in column 1 and so on
  a1 <- rbind(a1, n)
  r <- nrow(A) + 2
  # sample transitions using sample sizes in stage vector (last row)
  # from  a multinomial distribtion
  t1 <- apply(a1, 2, function(x) rmultinom(1, x[r], prob = x[-r]) / x[r])
  # remove dead fates in last row
  t1 <- t1[-nrow(t1), ]
  ## get fertilities (remove zeros) and then
  f1 <- a$F[a$F > 0]
  ## fvar can be a vector, if so, then it should match the number of fertilities
  # sample fertilities from a log normal distribution
  # OCT 2011 - fixed bug noted by Tali Vardi - some elements may have both T and F values and were not added
  # t1[which(a$F > 0)] <- lnorms(1, f1, fvar)
  t1[which(a$F > 0)] <- t1[which(a$F > 0)] + lnorms(1, f1, fvar)
  # dimnames(t1) <- list(stages, stages)
  t1
}
