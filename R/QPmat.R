QPmat <- function(nout, C, b, nonzero)
{
 ## Load "quadprog" package if NOT  in package list
  if(!"quadprog" %in% (.packages())){library(quadprog)}
  n<-dim(nout)
  # Generate the data vector
  z <- nout[ ,2:n[2]]
  z <- matrix(z, n[1]*(n[2]-1), 1)
  # Generate the matrix M
  M <- c()
  for (i in 1:(n[2]-1)){
  	N <- kronecker(t(nout[,i]), diag(n[1]))
  	m <- N[,nonzero]
  	M <- rbind(M,m)
  }
  # Generate the matrix G and the vector f
  G <-  t(M) %*% M
  f <-  t(M) %*% z   
  # Call R's quadratic programming routine
  res <- solve.QP(G,f,-t(C),-b)         
  phat <- res$solution 
  # Generate the estimated projection matrix
  a <-numeric(n[1]^2)
  a[nonzero] <- phat
  a <- matrix(a,n[1],n[1])
  x<-rownames(nout)       ##  add stage class names to vector
  dimnames(a)<-list(x,x)
  a

}  

