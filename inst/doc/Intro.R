## ----eval=FALSE---------------------------------------------------------------
#  function(X, Y, target, c, changepoint){
#    #Generate Data
#    n <- nrow(X)
#    p <- ncol(X)
#    c <- 1
#    m <- ceiling(c*sqrt(n))
#    q <- floor(n/m)
#    # Generate Large design matrix
#    K_temp <- matrix(0, nrow = q, ncol=q, byrow=TRUE)
#    X_temp <- X
#    Y_temp <- c(Y)
#    for(i in 1:q)
#      K_temp[i,1:i] <- rep(1,i)
#    x <- NULL
#    y <- NULL
#    x[[1]] <- as.matrix(X_temp[1:((n-(q-1)*m)),])
#    y[[1]] <- Y_temp[1:((n-(q-1)*m))]
#    for(i in 2:q){
#      x[[i]] <- as.matrix(X_temp[(n-(q-i+1)*m+1):((n-(q-i)*m)),])
#      y[[i]] <- Y_temp[(n-(q-i+1)*m+1):((n-(q-i)*m))]
#    }
#    X_temp1 <- lapply(1:length(x), function(j, mat, list) kronecker(K_temp[j,,drop=FALSE], x[[j]]), mat=K_temp, list=x)
#    Xn <- do.call("rbind",X_temp1)
#    # First step pilot estimator
#    cv1 <- cv.glmnet(Xn,Y_temp)
#    nonzero1 <- predict(cv1,s="lambda.1se",type="nonzero")[,1]
#    cv1 <- cv.glmnet(Xn[,nonzero1],Y_temp)
#    betanot0 <- as.vector(predict(cv1,s="lambda.min",type="coefficients"))[-1]
#    w1 <- rep(0,ncol(Xn))
#    w1[nonzero1] <- betanot0
#    sub.w <- w1[-c(((q-1)*p+1):(q*p))]
#    sub.X <- Xn[-c(1:((n-(q-1)*m))),-c(1:p)]
#    sub.y <- Y_temp[-c(1:((n-(q-1)*m)))]
#    Z <- sub.y-sub.X%*%sub.w
#    K2_temp <- matrix(0, nrow = q-1, ncol=q-1, byrow=TRUE)
#    for(i in 1:(q-1))
#      K2_temp[i,i] <- 1
#    x2 <- NULL
#    for(i in 2:q){
#      x2[[i-1]] <- as.matrix(X_temp[(n-(q-i+1)*m+1):((n-(q-i)*m)),])
#    }
#    X2_temp1 <-lapply(1:length(x2), function(j, mat, list) kronecker(K2_temp[j,,drop=FALSE], x2[[j]]), mat=K2_temp, list=x2)
#    Xn2 <- do.call("rbind",X2_temp1)
#    # construct contrast and regression
#    x2ko <- NULL
#    for (i in 1:(q-2)) {
#      x2ko[[i]] <- x2[[i+1]]
#    }
#    x2ko[[(q-1)]] <- x2[[1]]
#    X2ko_temp1 <-lapply(1:length(x2ko), function(j, mat, list) kronecker(K2_temp[j,,drop=FALSE], x2ko[[j]]), mat=K2_temp, list=x2ko)
#    Xn2ko <- do.call("rbind",X2ko_temp1)
#    XXX <- cbind(Xn2,Xn2ko) # Augumented design
#    cv2 <- cv.glmnet(XXX , Z)
#    beta3 <- as.vector(predict(cv2,s="lambda.min",type="coefficients"))[-1]
#    # Compute W-Statistics
#    fifth_matrix <- matrix(beta3,p,length(beta3)/p)
#    q1 <- dim(fifth_matrix)[2]/2
#    W <- NULL
#    for (i in 1:q1) {
#      W[i] <- sum(abs(fifth_matrix[,i]))-sum(abs(fifth_matrix[,i+q1]))}
#    # Set the Threshold
#    t <- MFKnockoffs::MFKnockoffs.threshold(W,q=target,method = "knockoff")
#    t_plus <- MFKnockoffs::MFKnockoffs.threshold(W,q=target,method = "knockoff+")
#    S_plus <- which(W>=t_plus)
#    S_plus <- S_plus+1
#    S <- which(W>=t)
#    S <- S+1
#    # The segments of true changepoints
#    tc <- NULL
#    l1 <- m+n%%m
#    tr <- changepoint
#    for (i in 1:length(tr)) {
#      if((tr[i]-l1)%%m==0){tc[i] <- (tr[i]-l1)%/%m+1}
#      else {tc[i] <- (tr[i]-l1)%/%m+2}
#    }
#    tcc <- c(tc[1],tc[2],tc[3],tc[4])
#    tc <- c(tc[1],tc[1]+1,tc[2],tc[2]+1,tc[3],tc[3]+1,tc[4],tc[4]+1)
#    powerrr <- 0
#    for (j in 1:length(tcc)) {
#      gro <- c(tcc[j],tcc[j]+1)
#      pandu <- intersect(S,gro)
#      if(length(pandu)!=0)
#      {powerrr <- powerrr+1}
#    }
#    powerrr_plus <- 0
#    for (j in 1:length(tcc)) {
#      gro_plus <- c(tcc[j],tcc[j]+1)
#      pandu_plus <- intersect(S_plus,gro_plus)
#      if(length(pandu_plus)!=0)
#      {powerrr_plus <- powerrr_plus+1}
#    }
#    if(length(S)>0){
#      fdr <- length(setdiff(S,tc))/(length(S))
#      mfdr <- length(setdiff(S,tc))/(length(S)+1/target)
#      tdf <- powerrr/length(tcc)
#    }else
#    {fdr <- 0
#    mfdr <- 0
#    tdf <- 0}
#    if(length(S_plus)>0){
#      fdr_plus <- length(setdiff(S_plus,tc))/(length(S_plus))
#      mfdr_plus <- length(setdiff(S_plus,tc))/(length(S_plus)+1/target)
#      tdf_plus <- powerrr_plus/length(tcc)
#    }else{
#      fdr_plus <- 0
#      mfdr_plus <- 0
#      tdf_plus <- 0}
#    obj <- list(fdr=fdr,mfdr=mfdr,tdf=tdf,fdr_plus=fdr_plus,mfdr_plus=mfdr_plus,
#                tdf_plus=tdf_plus,S=S,S_plus=S_plus)
#    return(obj)
#  }

## -----------------------------------------------------------------------------
library(SC19092)

## -----------------------------------------------------------------------------
set.seed(111)
n <- 1000
p <- 60
np <- 4
a <- 0.5
AS <- 2
amplitude <- c(AS,-AS)
changepoint <- c(200,400,600,800) # Location of changepoints
beta1 <- rep(0,p)
be1 <- sample(p,3)
beta1[be1] <- sample(amplitude, 3, replace = T)
beta2 <- rep(0,p)
be2 <- sample(p,3)
beta2[be2] <- sample(amplitude, 3, replace = T)
beta3 <- rep(0,p)
be3 <- sample(p,3)
beta3[be3] <- sample(amplitude, 3, replace = T)
beta4 <- rep(0,p)
be4 <- sample(p,3)
beta4[be4] <- sample(amplitude, 3, replace = T)
beta5 <- rep(0,p)
be5 <- sample(p,3)
beta5[be5] <- sample(amplitude, 3, replace = T)
beta <- c(beta1,beta2,beta3,beta4,beta5)
mu <- c(rep(0,p))
target <- 0.2
X <- matrix(rnorm(n*p), nrow = n)
X1 <- X
X2 <- X
X3 <- X
X4 <- X
X5 <- X
X2[(1:changepoint[1]),] <- 0
X3[1:changepoint[2],] <- 0
X4[1:changepoint[3],] <- 0
X5[(1:changepoint[4]),] <- 0
Xo=cbind(X1,X2,X3,X4,X5)
Y=Xo%*%beta+rnorm(n)

obj <- cplm(X, Y, target, 1, changepoint)
res <- matrix(unlist(obj)[1:6], nrow = 2, byrow = T)
colnames(res) <- c("FDR", "Modified FDR", "power")
rownames(res) <- c("Knockoff", "Knockoff+")
knitr::kable(res)

## ----eval=FALSE---------------------------------------------------------------
#  function(data){
#    x <- data[,1]
#    y <- data[,2]
#    n <- length(x)
#    jointloocv <- numeric(n)
#    hx <- h.ucv(x,deriv.order=0)$h # bandwidth selected by Unbiased CV
#    hy <- h.ucv(y,deriv.order=0)$h
#    for(i in 1:n){
#      jointloocv[i] <- mean(dnorm((x[-i]-x[i])/hx)*dnorm((y[-i]-y[i])/hy))/(hx*hy)
#    }
#    xloocv <- numeric(n)
#    for(i in 1:n){
#      xloocv[i] <- mean(dnorm((x[-i]-x[i])/hx)/hx)
#    }
#    yloocv <- numeric(n)
#    for(i in 1:n){
#      yloocv[i] <- mean(dnorm((y[-i]-y[i])/hy)/hy)
#    }
#    I <- mean(jointloocv)+mean(xloocv)*mean(yloocv)-2*mean(xloocv*yloocv)
#    X <- matrix(0,nrow=n,ncol=n)
#    Y <- matrix(0,nrow=n,ncol=n)
#    for(i in 1:n){
#      for(j in 1:n){
#        if (j!=i){
#          X[i,j] <- dnorm((x[i]-x[j])/hx)^2
#          Y[i,j] <- dnorm((y[i]-y[j])/hy)^2
#        }
#      }
#    }
#    sigma <- sqrt(2*sum(X*Y)/(n^2*hx*hy))
#    teststat <- n*sqrt(hx*hy)*I/sigma
#    cat("The value of test statistic is", teststat,"\n")
#    pvalue <- 1-pnorm(teststat) # One sided p-value
#    cat("p-value =",pvalue,"\n")
#    if(pvalue<0.05){
#      cat("Since p value is smaller than 0.05, we reject the independence assumption under significant level 0.05.")
#    }
#    else{
#      cat("Since p value is greater than 0.05, we accept the independence assumption under significant level 0.05.")
#    }
#  }

## -----------------------------------------------------------------------------
KDE_indep_test(faithful)
plot(faithful)

## -----------------------------------------------------------------------------
lm.fit <- lm(mpg ~ hp+wt,data=mtcars)
summary(lm.fit)

## -----------------------------------------------------------------------------
plot(lm.fit)

## -----------------------------------------------------------------------------
knitr::kable(cor(mtcars[,1:4]))

## -----------------------------------------------------------------------------
set.seed(521)
# Inverse of CDF of Rayleigh
den_rayleigh <- function(x,sigma) return(x*exp(-x^2/(2*sigma^2))/sigma^2)

inv_Rayleigh <- function(x,sigma) return(sqrt(-2*sigma^2*log(1-x)))

Rayleigh <- function(n,sigma) {
  #Generate n random numbers from Rayleigh distribution with mode sigma
  x <- runif(n)
  return(inv_Rayleigh(x,sigma))
}

R1 <- Rayleigh(10000,1) # mode = 1
R2 <- Rayleigh(10000,2) # mode = 2
R3 <- Rayleigh(10000,3) # mode = 3

hist(R1,breaks=60,freq=F,main="Histogram of Rayleigh(1)")
xlim1 <- seq(0,max(R1),0.01)
lines(xlim1, den_rayleigh(xlim1,1),col="red") # Theoretical density
abline(v = 1, col="blue") # Mark the mode 

hist(R2,breaks=60,freq=F,main="Histogram of Rayleigh(2)")
xlim2 <- seq(0,max(R2),0.01)
lines(xlim2, den_rayleigh(xlim2,2),col="red")
abline(v = 2, col="blue")

hist(R3,breaks=60,freq=F,main="Histogram of Rayleigh(3)")
xlim3 <- seq(0,max(R3),0.01)
lines(xlim3, den_rayleigh(xlim3,3),col="red")
abline(v = 3, col="blue")

## -----------------------------------------------------------------------------
mix_den <- function(x,p1){ # The density of mixture distribution
  return(p1*dnorm(x)+(1-p1)*dnorm(x,3,1))
}
M1 <- mix_gauss(10000,0.75)
hist(M1,breaks=60,freq=F,main=paste("Histogram of mixed gaussian with p1 = 0.75"))
xlim1 <- seq(min(M1),max(M1),0.01)
lines(xlim1, mix_den(xlim1,0.75),col="red")

M2 <- mix_gauss(10000,0.5)
hist(M2,breaks=60,freq=F,main=paste("Histogram of mixed gaussian with p1 = 0.5"))
xlim2 <- seq(min(M2),max(M2),0.01)
lines(xlim2, mix_den(xlim2,0.5),col="red")

M3 <- mix_gauss(10000,0.6)
hist(M3,breaks=60,freq=F,main=paste("Histogram of mixed gaussian with p1 = 0.6"))
xlim3 <- seq(min(M3),max(M3),0.01)
lines(xlim3, mix_den(xlim3,0.6),col="red")

## -----------------------------------------------------------------------------
Wishart(1,10,diag(rep(1,3)))
Wishart(1,10,matrix(c(1,0.9,0.9,1),ncol=2,nrow=2))

## -----------------------------------------------------------------------------
set.seed(521)
n <- c(100,1000,10000,100000)
estimates <- numeric(4)
for (i in 1:4){
  U <- runif(n[i],0,pi/3)
  estimates[i] <- pi/3*mean(sin(U))
}
estimates #Compare the estimate with the exact value.

## -----------------------------------------------------------------------------
n <- 10000
f <- function(x) return(exp(-x)/(1+x^2))
simu1(n,T) #The estimator with antithetic variable
m <- 1000
naive <- numeric(m)
anti <- numeric(m)
for (i in 1:m){
  naive[i] <- simu1(n,F)
  anti[i] <- simu1(n,T)
}
(var(naive)-var(anti))/var(naive)

## -----------------------------------------------------------------------------
n <- 10000
k <- 5
m <- n/k
N <- 1000 # Number of replications 
inv_F <- function(x) return(-log(1-(1-exp(-1))*x)) 
a <- c(0,inv_F(c(0.2,0.4,0.6,0.8,1)))
h <- function(i,x) return(-log(exp(-a[i])-(1-exp(-1))/5*x)) # Inverse of cdf for density on i-th subintervals
g <- function(x) return(5*exp(-x)/(1-exp(-1))) # The density on subintervals
# Pure importance sampling
pure <- numeric(N)
for (i in 1:N){
  x <- inv_F(runif(n))
  pure[i] <- mean(f(x)*exp(x)*(1-exp(-1)))
}
# Stratified importance sampling
stra <- numeric(N)
for (j in 1:N){
  I <- numeric(k)
  for (i in 1:k){
    x <- h(i,runif(m))
    I[i] <- mean(f(x)/g(x))
  }
  stra[j] <- sum(I)
}
mean(stra) # bagged stratified importance sampling estimates
c(sd(pure),sd(stra)) # Compare the standard error

## -----------------------------------------------------------------------------
set.seed(521)
m <- 10000
coverage <- numeric(m)
n <- 20
truemean <- 2
alpha <- 0.05
for (i in 1:m){
  data <- rchisq(n,df=2)
  S <- sd(data)
  a <- mean(data)-S*qt(1-alpha/2,n-1)/sqrt(n)
  b <- mean(data)+S*qt(1-alpha/2,n-1)/sqrt(n)
  coverage[i] <- a<=truemean && truemean<=b
}
mean(coverage)

## -----------------------------------------------------------------------------
coverage <- numeric(m)
trueva <- 4
for (i in 1:m){
  data <- rchisq(n,df=2)
  UCL <- (n-1) * var(data) / qchisq(alpha, df=n-1)
  coverage[i] <- trueva<=UCL
}
mean(coverage)

## -----------------------------------------------------------------------------
n <- 1000
m <- 10000
skews <- numeric(m)
for (i in 1:m){
  x <- rnorm(n)
  skews[i] <- skew(x)
}
p <- c(0.025,0.05,0.95,0.975)
quants <- quantile(skews,p)
aquants <- qnorm(p,sd=sqrt(6/n))
se <- sqrt(p*(1-p)/(n*(dnorm(aquants,0,sqrt(6*(n-2)/(n+1)/(n+3)))^2))) # Standard Error
quants # Monte Carlo quantiles
aquants # Asymptotic quantiles with normal approximation
round(se,digits=4) # Estimated variance using large sample results of quantiles

## -----------------------------------------------------------------------------
alpha <- 2:6
n <- 1000 # Sample size
m <- 1000 # No. of replications
results <- matrix(0,nrow=m,ncol=length(alpha))
colnames(results) <- paste("alpha=",alpha)
for(k in 1:length(alpha)){
  for(i in 1:m){
    x <- rbeta(n,alpha[k],alpha[k])
    skew0 <- skew(x)
    test <- abs(sqrt(n/6)*skew0)
    results[i,k] <- test>qnorm(0.975)
  }
}
colMeans(results)

## -----------------------------------------------------------------------------
nu <- seq(from=5,to=15,by=2)
results <- matrix(0,nrow=m,ncol=length(nu))
for(k in 1:length(nu)){
  for(i in 1:m){
    x <- rt(n,nu[k])
    skew0 <- skew(x)
    test <- abs(sqrt(n/6)*skew0)
    results[i,k] <- test>qnorm(0.975)
  }
}
colMeans(results)

## -----------------------------------------------------------------------------
m <- 1000
n <- seq(from=10,to=30,by=5)
chisq <- matrix(0,nrow=m,ncol=length(n))
unif <- matrix(0,nrow=m,ncol=length(n))
expo <- matrix(0,nrow=m,ncol=length(n))
for(k in 1:length(n)){
  for(i in 1:m){
    x1 <- rchisq(n[k],df=1)
    x2 <- runif(n[k],min=0,max=2)
    x3 <- rexp(n[k],1)
    chisq[i,k] <- t.test(x1,mu=1)$p.value
    unif[i,k] <- t.test(x2,mu=1)$p.value
    expo[i,k] <- t.test(x3,mu=1)$p.value
  }
}
chisq <- chisq < 0.05
unif <- unif < 0.05
expo <- expo < 0.05
results <- rbind(colMeans(chisq),colMeans(unif),colMeans(expo))
colnames(results) <- paste("n=",n)
rownames(results) <- c("Chisq(1)","U(0,2)","Exp(1)")
knitr::kable(results)

## -----------------------------------------------------------------------------
U <- (0.651-0.676)/sqrt(0.6635*(1-0.6635))*sqrt(10000^2/20000)
U <- abs(U)
c(U,qnorm(0.975))

## -----------------------------------------------------------------------------
options(warn=-1)
library(boot)
library(bootstrap)
set.seed(521)
data(scor)
pairs(scor,main="paired scatter plots")
knitr::kable(cor(scor))

## -----------------------------------------------------------------------------
# Use boot function in package "boot"
b.cor <- function(x,i) cor(x[i,1],x[i,2])
ses1 <- numeric(4)
r12.boot <- boot(data=scor[,1:2], statistic= b.cor, R=2000)
ses1[1] <- sd(r12.boot$t)
r34.boot <- boot(data=scor[,3:4], statistic= b.cor, R=2000)
ses1[2] <- sd(r34.boot$t)
r35.boot <- boot(data=scor[,c(3,5)], statistic= b.cor, R=2000)
ses1[3] <- sd(r35.boot$t)
r45.boot <- boot(data=scor[,4:5], statistic= b.cor, R=2000)
ses1[4] <- sd(r45.boot$t)
# Use my function
ses2 <- numeric(4)
ses2[1] <- corboot(scor[,1:2],2000)
ses2[2] <- corboot(scor[,3:4],2000)
ses2[3] <- corboot(scor[,c(3,5)],2000)
ses2[4] <- corboot(scor[,4:5],2000)
ses <- rbind(ses1,ses2)
colnames(ses) <- c("mec, vec","alg, ana","alg, sta","ana, sta")
knitr::kable(ses)

## ----eval=FALSE---------------------------------------------------------------
#  b.sk <- function(x,i) return(skew(x[i]))
#  m <- 100 # Number of replications
#  n <- c(10,20,50,100,500,1000) # Sample size
#  N <- length(n)
#  results.norm <- matrix(0,nrow=N,ncol=3)
#  results.basic <- matrix(0,nrow=N,ncol=3)
#  results.perc <- matrix(0,nrow=N,ncol=3)
#  results.bca <- matrix(0,nrow=N,ncol=3)
#  for(i in 1:N){
#    result.norm <- matrix(0,nrow=m,ncol=3)
#    result.basic <- matrix(0,nrow=m,ncol=3)
#    result.perc <- matrix(0,nrow=m,ncol=3)
#    result.bca <- matrix(0,nrow=m,ncol=3)
#    for(k in 1:m){
#      x <- rnorm(n[i])
#      boo <- boot(data=x,statistic=b.sk,R=1000)
#      CI <- boot.ci(boo,type=c("norm","basic","perc","bca"))
#      CI.norm <- CI$normal[2:3]
#      CI.basic <- CI$basic[4:5]
#      CI.perc <- CI$percent[4:5]
#      CI.bca <- CI$bca[4:5]
#      result.norm[k,1] <- I(CI.norm[1]>0)
#      result.norm[k,2] <- I(CI.norm[1]<=0)*I(CI.norm[2]>=0)
#      result.norm[k,3] <- I(CI.norm[2]<0)
#      result.basic[k,1] <- I(CI.basic[1]>0)
#      result.basic[k,2] <- I(CI.basic[1]<=0)*I(CI.basic[2]>=0)
#      result.basic[k,3] <- I(CI.basic[2]<0)
#      result.perc[k,1] <- I(CI.perc[1]>0)
#      result.perc[k,2] <- I(CI.perc[1]<=0)*I(CI.perc[2]>=0)
#      result.perc[k,3] <- I(CI.perc[2]<0)
#      result.bca[k,1] <- I(CI.bca[1]>0)
#      result.bca[k,2] <- I(CI.bca[1]<=0)*I(CI.bca[2]>=0)
#      result.bca[k,3] <- I(CI.bca[2]<0)
#    }
#    results.norm[i,] <- apply(result.norm,2,mean)
#    results.basic[i,] <- apply(result.basic,2,mean)
#    results.perc[i,] <- apply(result.perc,2,mean)
#    results.bca[i,] <- apply(result.bca,2,mean)
#  }
#  colnames(results.norm) <- c("Missing on the left","Covered","Missing on the right")
#  colnames(results.basic) <- c("Missing on the left","Covered","Missing on the right")
#  colnames(results.perc) <- c("Missing on the left","Covered","Missing on the right")
#  colnames(results.bca) <- c("Missing on the left","Covered","Missing on the right")
#  rownames(results.norm) <- paste("n=",c(10,20,50,100,500,1000))
#  rownames(results.basic) <- paste("n=",c(10,20,50,100,500,1000))
#  rownames(results.perc) <- paste("n=",c(10,20,50,100,500,1000))
#  rownames(results.bca) <- paste("n=",c(10,20,50,100,500,1000))

## ---- eval=FALSE--------------------------------------------------------------
#  knitr::kable(results.norm) # Normal interval
#  knitr::kable(results.basic) # Basic interval
#  knitr::kable(results.perc) # Percentile interval
#  knitr::kable(results.bca) # BCA interval

## ---- eval=FALSE--------------------------------------------------------------
#  true_sk <- sqrt(8/5)
#  results.norm <- matrix(0,nrow=N,ncol=3)
#  results.basic <- matrix(0,nrow=N,ncol=3)
#  results.perc <- matrix(0,nrow=N,ncol=3)
#  results.bca <- matrix(0,nrow=N,ncol=3)
#  for(i in 1:N){
#    result.norm <- matrix(0,nrow=m,ncol=3)
#    result.basic <- matrix(0,nrow=m,ncol=3)
#    result.perc <- matrix(0,nrow=m,ncol=3)
#    result.bca <- matrix(0,nrow=m,ncol=3)
#    for(k in 1:m){
#      x <- rchisq(n[i],df=5)
#      boo <- boot(data=x,statistic=b.sk,R=1000)
#      CI <- boot.ci(boo,type=c("norm","basic","perc","bca"))
#      CI.norm <- CI$normal[2:3]
#      CI.basic <- CI$basic[4:5]
#      CI.perc <- CI$percent[4:5]
#      CI.bca <- CI$bca[4:5]
#      result.norm[k,1] <- I(CI.norm[1]>true_sk)
#      result.norm[k,2] <- I(CI.norm[1]<=true_sk)*I(CI.norm[2]>=true_sk)
#      result.norm[k,3] <- I(CI.norm[2]<true_sk)
#      result.basic[k,1] <- I(CI.basic[1]>true_sk)
#      result.basic[k,2] <- I(CI.basic[1]<=true_sk)*I(CI.basic[2]>=true_sk)
#      result.basic[k,3] <- I(CI.basic[2]<true_sk)
#      result.perc[k,1] <- I(CI.perc[1]>true_sk)
#      result.perc[k,2] <- I(CI.perc[1]<=true_sk)*I(CI.perc[2]>=true_sk)
#      result.perc[k,3] <- I(CI.perc[2]<true_sk)
#      result.bca[k,1] <- I(CI.bca[1]>true_sk)
#      result.bca[k,2] <- I(CI.bca[1]<=true_sk)*I(CI.bca[2]>=true_sk)
#      result.bca[k,3] <- I(CI.bca[2]<true_sk)
#    }
#    results.norm[i,] <- apply(result.norm,2,mean)
#    results.basic[i,] <- apply(result.basic,2,mean)
#    results.perc[i,] <- apply(result.perc,2,mean)
#    results.bca[i,] <- apply(result.bca,2,mean)
#  }
#  colnames(results.norm) <- c("Missing on the left","Covered","Missing on the right")
#  colnames(results.basic) <- c("Missing on the left","Covered","Missing on the right")
#  colnames(results.perc) <- c("Missing on the left","Covered","Missing on the right")
#  colnames(results.bca) <- c("Missing on the left","Covered","Missing on the right")
#  rownames(results.norm) <- paste("n=",c(10,20,50,100,500,1000))
#  rownames(results.basic) <- paste("n=",c(10,20,50,100,500,1000))
#  rownames(results.perc) <- paste("n=",c(10,20,50,100,500,1000))
#  rownames(results.bca) <- paste("n=",c(10,20,50,100,500,1000))

## ---- eval=FALSE--------------------------------------------------------------
#  knitr::kable(results.norm) # Normal interval
#  knitr::kable(results.basic) # Basic interval
#  knitr::kable(results.perc) # Percentile interval
#  knitr::kable(results.bca) # BCA interval

## -----------------------------------------------------------------------------
library(bootstrap)
set.seed(521)
data(scor)
n <- nrow(scor)
S <- cov(scor)
e <- eigen(S)
theta_hat <- e$values[1]/sum(e$values)
alpha <- 0.05
#Jackknife
theta_jack <- numeric(n)
for (i in 1:n){
  S <- cov(scor[-i,])
  e <- eigen(S)
  theta_jack[i] <- e$values[1]/sum(e$values)
}
bias_jack <- (n-1)*(mean(theta_jack)-theta_hat)
se_jack <- sqrt(mean((theta_jack-mean(theta_jack))^2)*(n-1))
A <- matrix(c(bias_jack,se_jack),ncol=2)
colnames(A) <- c("Bias.Jackknife","SE.Jackknife")
knitr::kable(A)

## -----------------------------------------------------------------------------
library(DAAG)
attach(ironslag)
n <- length(magnetic)
e1 <- e2 <- e3 <- e4 <- numeric(n)
for(i in 1:n){
  x <- chemical[-i]
  y <- magnetic[-i]
  # Linear Model
  lm1 <- lm(y~x)
  yhat1 <- predict(lm1,newdata=list(x=chemical[i]))
  e1[i] <- yhat1-magnetic[i]
  # Quadratic Model
  lm2 <- lm(y~x+I(x^2))
  yhat2 <- predict(lm2,newdata=list(x=chemical[i]))
  e2[i] <- yhat2-magnetic[i]
  # Exponential Model
  lm3 <- lm(log(y)~x)
  yhat3 <- exp(predict(lm3,newdata=list(x=chemical[i])))
  e3[i] <- yhat3-magnetic[i]
  # Cubic Model
  lm4 <- lm(y~x+I(x^2)+I(x^3))
  yhat4 <- predict(lm4,newdata=list(x=chemical[i]))
  e4[i] <- yhat4-magnetic[i]
}
MSE <- matrix(c(mean(e1^2),mean(e2^2),mean(e3^2),mean(e4^2)),ncol=4)
colnames(MSE) <- c("Linear","Quadratic","Exponential","Cubic")
knitr::kable(MSE)

## -----------------------------------------------------------------------------
lm1 <- lm(magnetic~chemical)
lm2 <- lm(magnetic~chemical+I(chemical^2))
lm3 <- lm(log(magnetic)~chemical)
lm4 <- lm(magnetic~chemical+I(chemical^2)+I(chemical^3))
rsquare1 <- 1-sum(residuals(lm1)^2)/(var(magnetic)*(n-1))
adj1 <- 1-(n-1)*(1-rsquare1)/(n-2)
rsquare2 <- 1-sum(residuals(lm2)^2)/(var(magnetic)*(n-1))
adj2 <- 1-(n-1)*(1-rsquare2)/(n-3)
rsquare3 <- 1-sum(residuals(lm3)^2)/(var(log(magnetic))*(n-1))
adj3 <- 1-(n-1)*(1-rsquare3)/(n-2)
rsquare4 <- 1-sum(residuals(lm4)^2)/(var(magnetic)*(n-1))
adj4 <- 1-(n-1)*(1-rsquare4)/(n-4)
AR2 <- matrix(c(adj1,adj2,adj3,adj4),ncol=4)
colnames(AR2) <- c("Linear","Quadratic","Exponential","Cubic")
knitr::kable(AR2)

## -----------------------------------------------------------------------------
library(boot)
extremetest <- function(z,dims){
  boot.obj <- boot(data=z,statistic=b.maxout,R=999,sim="permutation",dims=dims)
  ts <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(ts>=ts[1])
  return(p.value)
}
# Return to Example 6.15 and estimate the type I error under significant level 0.05
n1 <- 20
n2 <- 30
mu1 <- 0
mu2 <- 10
sigma1 <- sigma2 <- 1
m <- 100
set.seed(521)
pvalues <- numeric(m)
for(i in 1:m){
  x <- rnorm(n1, mu1, sigma1)
  y <- rnorm(n2, mu2, sigma2)
  x <- x-mean(x)
  y <- y-mean(y)
  pvalues[i] <- extremetest(c(x,y),c(n1,n2))
}
mean(pvalues<0.05)
# n2 = 50
n1 <- 20
n2 <- 50
mu1 <- mu2 <- 0
sigma1 <- sigma2 <- 1
m <- 100
set.seed(521)
pvalues <- numeric(m)
for(i in 1:m){
  x <- rnorm(n1, mu1, sigma1)
  y <- rnorm(n2, mu2, sigma2)
  x <- x-mean(x)
  y <- y-mean(y)
  pvalues[i] <- extremetest(c(x,y),c(n1,n2))
}
mean(pvalues<0.05)

## -----------------------------------------------------------------------------
library(Ball)
dCov <- function(x, y) {
  x <- as.matrix(x)
  y <- as.matrix(y)
  n <- nrow(x)
  m <- nrow(y)
  if (n != m || n < 2) stop("Sample sizes must agree")
  if (! (all(is.finite(c(x, y)))))
    stop("Data contains missing or infinite values")
  Akl <- function(x) {
    d <- as.matrix(dist(x))
    m <- rowMeans(d)
    M <- mean(d)
    a <- sweep(d, 1, m)
    b <- sweep(a, 2, m)
    return(b + M)
  }
  A <- Akl(x)
  B <- Akl(y)
  dCov <- sqrt(mean(A * B))
  return(dCov)
}
ndCov2 <- function(z, ix, dims) {
  p <- dims[1]
  q1 <- dims[2] + 1
  d <- p + dims[2]
  x <- z[ , 1:p] 
  y <- z[ix, q1:d] #permute rows of y
  return(nrow(z) * dCov(x, y)^2)
}
m <- 100 # Number of replications
N <- seq(from=20,to=120,by=20) # Sample size
library(snowfall)
sfInit(parallel=T,cpus=parallel::detectCores())
sfLibrary(boot)
sfLibrary(Ball)
sfExport("N","m")
sfExport("dCov","ndCov2")
model1.power <- sfSapply(N,function(n){
  result1 <- numeric(m)
  result2 <- numeric(m)
  for(i in 1:m){
    x <- matrix(rnorm(2*n),ncol=2)
    e <- matrix(rnorm(2*n),ncol=2)
    y <- x/4 + e
    z <- cbind(x,y)
    boot.obj <- boot(data=z,statistic=ndCov2,R=999,dims=c(2,2),sim="permutation")
    tb <- c(boot.obj$t0, boot.obj$t)
    p1 <- mean(tb>=tb[1])
    result1[i] <- ifelse(p1 < 0.05,1,0)
    p2 <- bcov.test(x,y,R=999,seed=521*i-n)$p.value
    result2[i] <- ifelse(p2 < 0.05,1,0)
  }
  return(c(mean(result1),mean(result2)))
})
sfStop()
colnames(model1.power) <- paste("n=",N)
rownames(model1.power) <- c("Distance Correlation","Ball Covariance")
knitr::kable(model1.power)
plot(N,model1.power[1,],main="Power Comparison",type="p",pch=4,col="red",xlab="Sample Size",ylab="Power",ylim=c(0,1))
lines(N,model1.power[1,],col="red")
points(N,model1.power[2,],type="p",pch=20,col="blue")
lines(N,model1.power[2,],col="blue")
legend("topleft",c("Distance","Ball"),pch=c(4,20),col=c("red","blue"),cex=0.5)

sfInit(parallel=T,cpus=parallel::detectCores())
sfLibrary(boot)
sfLibrary(Ball)
sfExport("N","m")
sfExport("dCov","ndCov2")
model2.power <- sfSapply(N,function(n){
  result1 <- numeric(m)
  result2 <- numeric(m)
  for(i in 1:m){
    x <- matrix(rnorm(2*n),ncol=2)
    e <- matrix(rnorm(2*n),ncol=2)
    y <- x/4 * e
    z <- cbind(x,y)
    boot.obj <- boot(data=z,statistic=ndCov2,R=999,dims=c(2,2),sim="permutation")
    tb <- c(boot.obj$t0, boot.obj$t)
    p1 <- mean(tb>=tb[1])
    result1[i] <- ifelse(p1 < 0.05,1,0)
    p2 <- bcov.test(x,y,R=999,seed=521*i-n)$p.value
    result2[i] <- ifelse(p2 < 0.05,1,0)
  }
  return(c(mean(result1),mean(result2)))
})
sfStop()
colnames(model2.power) <- paste("n=",N)
rownames(model2.power) <- c("Distance Correlation","Ball Covariance")
knitr::kable(model2.power)
plot(N,model2.power[1,],main="Power Comparison",type="p",pch=4,col="red",xlab="Sample Size",ylab="Power",ylim=c(0,1))
lines(N,model2.power[1,],col="red")
points(N,model2.power[2,],type="p",pch=20,col="blue")
lines(N,model2.power[2,],col="blue")
legend("bottomright",c("Distance","Ball"),pch=c(4,20),col=c("red","blue"),cex=0.5)

## -----------------------------------------------------------------------------
set.seed(521)
N <- 20000
sigma <- c(.05, .5, 2, 16)
x0 <- 0
chain1 <- rw.Metropolis(sigma[1], x0, N)
chain2 <- rw.Metropolis(sigma[2], x0, N)
chain3 <- rw.Metropolis(sigma[3], x0, N)
chain4 <- rw.Metropolis(sigma[4], x0, N)
rej <- matrix(c(chain1[[2]],chain2[[2]],chain3[[2]],chain4[[2]])/N,nrow=1)
colnames(rej)=paste("Scaling parameter = ",sigma)
knitr::kable(rej)

# Trace plot
chains <- cbind(chain1[[1]],chain2[[1]],chain3[[1]],chain4[[1]])
for(i in 1:4){
  plot(1:1000,chains[1:1000,i],type="l",xlab=paste("Scaling parameter = ",sigma[i]),ylab="Chain",ylim=range(chains[,i]))
} 
# Cumulative Mean plot
for(i in 1:4){
  plot(cumsum(chains[,i])/1:N,type="l",main=paste("Culmulative mean plot sd =",sigma[i]),ylab="Mean of chain")
  abline(h=mean(chains[,i]),col="red")
}
qpoints <- seq(from=0.6,to=0.9,by=0.1)
qpoints <- c(qpoints,0.95)
quantlap <- function(alpha){
  return(-log(2*(1-alpha)))
}
quant <- quantlap(qpoints)
truequant <- c(-rev(quant),0,quant)
a <- c(.05, seq(.1, .9, .1), .95)
rw <- cbind(chain1$x, chain2$x, chain3$x, chain4$x)
mc <- rw[501:N, ]
Qrw <- apply(mc, 2, function(x) quantile(x, a))
Qrw <- cbind(truequant,Qrw)
colnames(Qrw) <- c("True",paste("sigma =",sigma))
rownames(Qrw) <- paste(a*100,"%")
knitr::kable(Qrw)

## -----------------------------------------------------------------------------
# Example 1 
a <- log(exp(100))
b <- exp(log(100))
a-b # Not exactly 0
isTRUE(all.equal(a,b)) # Hold with near equality
# Example 2
a <- log(exp(0.001))
b <- exp(log(0.001))
a-b # Not exactly 0
isTRUE(all.equal(a,b)) # Hold with near equality

## -----------------------------------------------------------------------------
k <- c(4,25,100,500,1000) # No need to solve for k = 4:25
f <- function(a,k){
  c1 <- sqrt(a^2*(k-1)/(k-a^2))
  f1 <- function(u) return((1+u^2/(k-1))^(-k/2))
  I1 <- integrate(f1,lower=0,upper=c1)$value
  c2 <- sqrt(a^2*k/(k+1-a^2))
  f2 <- function(u) return((1+u^2/k)^(-(k+1)/2))
  I2 <- integrate(f2,lower=0,upper=c2)$value
  left <- exp(lgamma(k/2)-lgamma((k-1)/2))*I1/sqrt(k-1)
  right <- exp(lgamma((k+1)/2)-lgamma(k/2))*I2/sqrt(k)
  return(left-right)
}
A5 <- numeric(5)
# Get some interval with opposite signs at points
uppers <- c(sqrt(k[1]),sqrt(k[2])/2,sqrt(k[3])/2,sqrt(k[4])/5,sqrt(k[5])/5)
for(i in 1:5){
  A5[i] <- uniroot(f,c(0.1,uppers[i]),k=k[i])$root
}
A5 <- matrix(A5,nrow=1)
colnames(A5) <- paste("k =",k)
knitr::kable(A5)

## -----------------------------------------------------------------------------
nA <- 28
nB <- 24
nO <- 41
nAB <- 70
n <- nA+nB+nO+nAB
result <- EMABO(10,nA,nB,nO,nAB,n)
p <- result$p
q <- result$q
r <- result$r
likes <- result$like
res <- matrix(c(p,q,r),nrow=1)
colnames(res) <- c("p","q","r")
knitr::kable(res)
plot(likes,type="l",main="Incomplete log-likelihood")

## -----------------------------------------------------------------------------
formulas <- list(
mpg ~ disp,
mpg ~ I(1 / disp),
mpg ~ disp + wt,
mpg ~ I(1 / disp) + wt
)
# For loop
out3 <- vector("list", length(formulas))
for(i in seq_along(formulas)){
  out3[[i]] <- lm(formulas[[i]],data=mtcars)
}
out3
#Lapply
lapply(formulas,function(formula) lm(formula, data=mtcars))

## -----------------------------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {
rows <- sample(1:nrow(mtcars), rep = TRUE)
mtcars[rows, ]
})

out4 <- vector("list", length(bootstraps))
for(i in seq_along(bootstraps)){ 
  out4[[i]] <- lm(mpg~disp, data = bootstraps[[i]])
}
out4[1:2] # Just check first two models
lms <- lapply(bootstraps, lm, formula = mpg~disp)
lms[1:2]

## -----------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared
unlist(lapply(out3, rsq))
unlist(lapply(out4, rsq))

## -----------------------------------------------------------------------------
trials <- replicate(
100,
t.test(rpois(10, 10), rpois(7, 10)),
simplify = FALSE
)
pvalue1 <- sapply(trials, function(test) test$p.value)
head(pvalue1)
pvalue2 <- sapply(trials, "[[", "p.value")
head(pvalue2)

## -----------------------------------------------------------------------------
library(parallel)
mcsapply <- function(x, f, mc.cores, ...){
  res <- mclapply(x, f, mc.cores = mc.cores)
  simplify2array(res)
}

mcvapply <- function(x, f, f.value, mc.cores, ...){
  out <- matrix(rep(f.value, length(x)), nrow = length(x))
  res <- mclapply(x, f, mc.cores = mc.cores)
  for(i in seq_along(x)){
    stopifnot(
      length(res[[i]]) == length(f.value),
      typeof(res[[i]]) == typeof(f.value)
    )
    out[i,] <- res[[i]]
  }
  drop(out)
}

# Use examples in textbook to check
boot_df <- function(x) x[sample(nrow(x), rep = T), ]
rsquared <- function(mod) summary(mod)$r.square
boot_lm <- function(i) {
rsquared(lm(mpg ~ wt + disp, data = boot_df(mtcars)))
}
a <- mcsapply(1:500, boot_lm, mc.cores = 2)
head(a)
b <- mcvapply(1:500, boot_lm, numeric(1), mc.cores = 2)
head(b)

## -----------------------------------------------------------------------------
library(Rcpp)
library(microbenchmark)
set.seed(521)
# R-Version
N <- 20000
sigma <- c(.05, .5, 2, 16)
x0 <- 0
Rrw1 <- rw.Metropolis(sigma[1], x0, N)
Rrw2 <- rw.Metropolis(sigma[2], x0, N)
Rrw3 <- rw.Metropolis(sigma[3], x0, N)
Rrw4 <- rw.Metropolis(sigma[4], x0, N)
Crw1 <- rwcpp(sigma[1], x0, N)
Crw2 <- rwcpp(sigma[2], x0, N)
Crw3 <- rwcpp(sigma[3], x0, N)
Crw4 <- rwcpp(sigma[4], x0, N)
racc1 <-  1-c(Rrw1[[2]], Rrw2[[2]], Rrw3[[2]], Rrw4[[2]])/N
racc2 <-  1-c(Crw1[[2]], Crw2[[2]], Crw3[[2]], Crw4[[2]])/N
racc <- rbind(racc1, racc2)
colnames(racc) <- paste("Scaling parameter = ",sigma)
rownames(racc) <- c("R","C++")
knitr::kable(racc)

## -----------------------------------------------------------------------------
Rrw <- cbind(Rrw1[[1]], Rrw2[[1]], Rrw3[[1]], Rrw4[[1]])
Rrw <- Rrw[10001:N, ]
Crw <- cbind(Crw1[[1]], Crw2[[1]], Crw3[[1]], Crw4[[1]])
Crw <- Crw[10001:N, ]
for(i in 1:4){
  qqplot(Rrw[,i], Crw[,i], main = paste("QQplot with Sigma = ", sigma[i]), xlab = "Random Walk in R", ylab = "Random Walk in C++")
}

## -----------------------------------------------------------------------------
time <- microbenchmark(
  rw.Metropolis(sigma[1], x0, N),
  rwcpp(sigma[1], x0, N),
  rw.Metropolis(sigma[2], x0, N),
  rwcpp(sigma[2], x0, N),
  rw.Metropolis(sigma[3], x0, N),
  rwcpp(sigma[3], x0, N),
  rw.Metropolis(sigma[4], x0, N),
  rwcpp(sigma[4], x0, N)
)
a <- summary(time)[,c(1,3,5,6)]
knitr::kable(a)

