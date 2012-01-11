stoch.projection<-function(matrices, n0, tmax=50, nreps=5000, prob=NULL, nmax=NULL, sumweight=NULL, verbose=TRUE)
{
   if(is.list(matrices)){matrices<-matrix(unlist(matrices), ncol=length(matrices))}
   ## initialize empty matrix to store results at each time step...
   x<-length(n0)
   if(is.null(sumweight)){sumweight=rep(1,x)}
   est<-matrix(numeric(nreps*x), nrow=nreps)
   colnames(est)<-names(n0)
   y<-dim(matrices)[2]
   for(i in 1:nreps)
   {
      n<-n0
      if(verbose)
      {
         if(i==1 || i %% 100 == 0){print(paste("Starting projections for nrep", i), quote=FALSE)}
      }
      for( j in 1:tmax)
      {
         col<-sample(1:y, 1, prob= prob)
         A<-matrix(matrices[,col], nrow=x )
         n<-A %*% n
         ## simple density dependence
         if(!is.null(nmax))
         {
            if(sum(n*sumweight) > nmax){n<- n * (nmax/sum(n * sumweight))}
         }
     }
     est[i,]<-n
  }
  est
}

