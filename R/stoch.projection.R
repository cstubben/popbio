stoch.projection<-function(matrices, n0, tmax=50, nreps=5000, prob=NULL, nmax=NULL, sumweight=rep(1,length(n0)), verbose=FALSE)
{
   if(!is.list(matrices)){ stop("Please use a list of matrices as input") }
  ## initialize empty matrix to store results
   est <- matrix(numeric(nreps * length(n0)), nrow = nreps)
   colnames(est) <- names(n0)

   for(i in 1:nreps)
   {
      # random sample of matrices
      A <- sample(matrices, tmax, replace=TRUE, prob=prob)
      n <- n0
      if(verbose){
         if(i==1 || i %% 100 == 0){print(paste("Starting projections for nrep", i), quote=FALSE)}
      }
      for( j in 1:tmax){
         n <- A[[j]] %*% n
         ## simple density dependence
         if(!is.null(nmax)){
            if(sum(n*sumweight) > nmax)  n<- n * (nmax/sum(n * sumweight))
         }
     }
     est[i,]<-n
  }
  est
}


stoch.projection2 <-function(matrices, n0, tmax=50, nreps=5000, prob=NULL, nmax=NULL, sumweight=rep(1,length(n0)), verbose=FALSE)
{
   if(!is.list(matrices)){ stop("Please use a list of matrices as input") }
   ## initialize empty matrix to store results
   #est <- matrix(numeric(nreps * length(n0)), nrow = nreps)
   # colnames(est) <- names(n0)
   t1 <- vector("list", nreps)
   for(i in 1:nreps)
   {
      t1[[i]] <-  matrix(0,length(n0), ncol=tmax)
      rownames(t1[[1]]) <- names(n0)
      # random sample of matrices
      A <- sample(matrices, tmax, replace=TRUE, prob=prob)
      n <- n0
      if(verbose){
         if(i==1 || i %% 100 == 0){print(paste("Starting projections for nrep", i), quote=FALSE)}
      }
      for( j in 1:tmax){
         n <- A[[j]] %*% n
         t1[[i]][ , j ] <- n
         if(!is.null(nmax)){
            if(sum(n*sumweight) > nmax)  n<- n * (nmax/sum(n * sumweight))
         }
     }
    # est[i,]<-n
  }
  #est
  t1
}


# skip seeds

t( sapply(x, function(y) y[, ncol(y)]))


matplot2(x[[1]][-1,])
