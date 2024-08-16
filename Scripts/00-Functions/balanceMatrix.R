# Function to balance a matrix
# RAS matrix balancing algorithm: to find a matrix X that satisfies marginal constraints
# FP 2023

#X[ ,+] = u  (marginals constraints on rows)
#X[+, ] = v  (marginals constraints on cols)
#and X is obtained by scaling rows and columns of A 
f.balanceMatrix <- function(trade,te,ti){
  
  te<-te
  ti<-t(ti)
  X <-  as.matrix(trade)
  X[X==0]<-0.1
  diag(X)<-0
  tol = 1
  mdif= max(abs(te-rowSums(X)),abs(ti-colSums(X)))
  flag= FALSE
  count=0
  minIter=1000 # run at least 1000 times
  
  
  while(mdif>tol & count<minIter){
    count=count+1
    R = ifelse(rowSums(X) == 0, 0, te / rowSums(X))
    X = X*R
    
    C = ifelse(colSums(X) == 0, 0, ti / colSums(X))
    X = sweep(X, 2, C, "*")
    
    prev_mdif = mdif
    mdif= max(abs(te-rowSums(X)),abs(ti-colSums(X)))
    mdif
    
    if (prev_mdif * 0.95 < mdif && mdif < prev_mdif * 1.05 && count>minIter) {
      flag = TRUE
      break
    }
  }
  if (flag) {
    return(as.data.frame(X))
  } else {
    return(as.data.frame(X))
  }
}
