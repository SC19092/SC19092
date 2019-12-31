#' Change point detection in linear model
#' @import glmnet
#' @import MFKnockoffs
#' @importFrom stats predict
#' @param X the data matrix
#' @param Y the response vector
#' @param target the target FDR level
#' @param c tuning parameter on the interval's length
#' @param changepoint oracle location of changepoints for reference
#'
#' @return a list with FDR reported
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' n <- 1000
#' p <- 60
#' np <- 4
#' a <- 0.5
#' AS <- 2
#' amplitude <- c(AS,-AS)
#' changepoint <- c(200,400,600,800) 
#' beta1 <- rep(0,p)
#' be1 <- sample(p,3)
#' beta1[be1] <- sample(amplitude, 3, replace = T)
#' beta2 <- rep(0,p)
#' be2 <- sample(p,3)
#' beta2[be2] <- sample(amplitude, 3, replace = T)
#' beta3 <- rep(0,p)
#' be3 <- sample(p,3)
#' beta3[be3] <- sample(amplitude, 3, replace = T)
#' beta4 <- rep(0,p)
#' be4 <- sample(p,3)
#' beta4[be4] <- sample(amplitude, 3, replace = T)
#' beta5 <- rep(0,p)
#' be5 <- sample(p,3)
#' beta5[be5] <- sample(amplitude, 3, replace = T)
#' beta <- c(beta1,beta2,beta3,beta4,beta5)
#' mu <- c(rep(0,p))
#' target <- 0.2
#' X <- matrix(rnorm(n*p), nrow = n)
#' X1 <- X
#' X2 <- X
#' X3 <- X
#' X4 <- X
#' X5 <- X
#' X2[(1:changepoint[1]),] <- 0
#' X3[1:changepoint[2],] <- 0
#' X4[1:changepoint[3],] <- 0
#' X5[(1:changepoint[4]),] <- 0
#' Xo <- cbind(X1,X2,X3,X4,X5)
#' Y <- Xo%*%beta+rnorm(n)
#' changepoint <- c(200,400,600,800) 
#' target <- 0.2
#' c <- 1
#' cplm(X, Y, target, c, changepoint)
#' }

cplm <- function(X, Y, target, c, changepoint){
  #Generate Data
  n <- nrow(X)
  p <- ncol(X)
  c <- 1
  m <- ceiling(c*sqrt(n))
  q <- floor(n/m)
  # Generate Large design matrix
  K_temp <- matrix(0, nrow = q, ncol=q, byrow=TRUE)
  X_temp <- X
  Y_temp <- c(Y)
  for(i in 1:q)
    K_temp[i,1:i] <- rep(1,i)
  x <- NULL
  y <- NULL
  x[[1]] <- as.matrix(X_temp[1:((n-(q-1)*m)),])
  y[[1]] <- Y_temp[1:((n-(q-1)*m))]
  for(i in 2:q){
    x[[i]] <- as.matrix(X_temp[(n-(q-i+1)*m+1):((n-(q-i)*m)),])
    y[[i]] <- Y_temp[(n-(q-i+1)*m+1):((n-(q-i)*m))]
  }
  X_temp1 <- lapply(1:length(x), function(j, mat, list) kronecker(K_temp[j,,drop=FALSE], x[[j]]), mat=K_temp, list=x) 
  Xn <- do.call("rbind",X_temp1)
  # First step pilot estimator
  cv1 <- cv.glmnet(Xn,Y_temp)
  nonzero1 <- predict(cv1,s="lambda.1se",type="nonzero")[,1]
  cv1 <- cv.glmnet(Xn[,nonzero1],Y_temp)
  betanot0 <- as.vector(predict(cv1,s="lambda.min",type="coefficients"))[-1]
  w1 <- rep(0,ncol(Xn))
  w1[nonzero1] <- betanot0
  sub.w <- w1[-c(((q-1)*p+1):(q*p))]
  sub.X <- Xn[-c(1:((n-(q-1)*m))),-c(1:p)]
  sub.y <- Y_temp[-c(1:((n-(q-1)*m)))]
  Z <- sub.y-sub.X%*%sub.w
  K2_temp <- matrix(0, nrow = q-1, ncol=q-1, byrow=TRUE)
  for(i in 1:(q-1))
    K2_temp[i,i] <- 1
  x2 <- NULL
  for(i in 2:q){
    x2[[i-1]] <- as.matrix(X_temp[(n-(q-i+1)*m+1):((n-(q-i)*m)),])
  }
  X2_temp1 <-lapply(1:length(x2), function(j, mat, list) kronecker(K2_temp[j,,drop=FALSE], x2[[j]]), mat=K2_temp, list=x2) 
  Xn2 <- do.call("rbind",X2_temp1)
  # construct contrast and regression
  x2ko <- NULL
  for (i in 1:(q-2)) {
    x2ko[[i]] <- x2[[i+1]]
  }
  x2ko[[(q-1)]] <- x2[[1]]
  X2ko_temp1 <-lapply(1:length(x2ko), function(j, mat, list) kronecker(K2_temp[j,,drop=FALSE], x2ko[[j]]), mat=K2_temp, list=x2ko) 
  Xn2ko <- do.call("rbind",X2ko_temp1)
  XXX <- cbind(Xn2,Xn2ko) # Augumented design
  cv2 <- cv.glmnet(XXX , Z)
  beta3 <- as.vector(predict(cv2,s="lambda.min",type="coefficients"))[-1]
  # Compute W-Statistics
  fifth_matrix <- matrix(beta3,p,length(beta3)/p)
  q1 <- dim(fifth_matrix)[2]/2
  W <- NULL
  for (i in 1:q1) {
    W[i] <- sum(abs(fifth_matrix[,i]))-sum(abs(fifth_matrix[,i+q1]))}
  
  # Set the Threshold
  t <- MFKnockoffs::MFKnockoffs.threshold(W,q=target,method = "knockoff")
  t_plus <- MFKnockoffs::MFKnockoffs.threshold(W,q=target,method = "knockoff+")
  
  S_plus <- which(W>=t_plus)
  S_plus <- S_plus+1
  
  S <- which(W>=t)
  S <- S+1
  
  # The segments of true changepoints
  tc <- NULL
  l1 <- m+n%%m
  tr <- changepoint
  for (i in 1:length(tr)) {
    if((tr[i]-l1)%%m==0){tc[i] <- (tr[i]-l1)%/%m+1}
    else {tc[i] <- (tr[i]-l1)%/%m+2}
  }
  tcc <- c(tc[1],tc[2],tc[3],tc[4])
  tc <- c(tc[1],tc[1]+1,tc[2],tc[2]+1,tc[3],tc[3]+1,tc[4],tc[4]+1)
  
  powerrr <- 0
  for (j in 1:length(tcc)) {
    gro <- c(tcc[j],tcc[j]+1)
    pandu <- intersect(S,gro)
    if(length(pandu)!=0)
    {powerrr <- powerrr+1}
  }
  
  powerrr_plus <- 0
  for (j in 1:length(tcc)) {
    gro_plus <- c(tcc[j],tcc[j]+1)
    pandu_plus <- intersect(S_plus,gro_plus)
    if(length(pandu_plus)!=0)
    {powerrr_plus <- powerrr_plus+1}
    
  }
  
  if(length(S)>0){
    fdr <- length(setdiff(S,tc))/(length(S))
    mfdr <- length(setdiff(S,tc))/(length(S)+1/target)
    tdf <- powerrr/length(tcc)
  }else
  {fdr <- 0
  mfdr <- 0
  tdf <- 0}
  
  if(length(S_plus)>0){
    fdr_plus <- length(setdiff(S_plus,tc))/(length(S_plus))
    mfdr_plus <- length(setdiff(S_plus,tc))/(length(S_plus)+1/target)
    
    tdf_plus <- powerrr_plus/length(tcc)
  }else{
    fdr_plus <- 0
    mfdr_plus <- 0
    tdf_plus <- 0}
  
  obj <- list(fdr=fdr,mfdr=mfdr,tdf=tdf,fdr_plus=fdr_plus,mfdr_plus=mfdr_plus,
              tdf_plus=tdf_plus,S=S,S_plus=S_plus)
  return(obj)
}

