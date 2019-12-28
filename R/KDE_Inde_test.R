#' Title Independence Test of Random Variables based on KDE
#'
#' @import kedd
#' @importFrom stats dnorm pnorm
#' @param data A two dimension data matrix 
#'
#' @return test statistic based on KDE and p-value
#' @export
#'
#' @examples
#' \dontrun{
#' KDE_indep_test(faithful)
#' }

KDE_indep_test <- function(data){
  x <- data[,1]
  y <- data[,2]
  n <- length(x)
  jointloocv <- numeric(n)
  hx <- h.ucv(x,deriv.order=0)$h # bandwidth selected by Unbiased CV
  hy <- h.ucv(y,deriv.order=0)$h
  for(i in 1:n){
    jointloocv[i] <- mean(dnorm((x[-i]-x[i])/hx)*dnorm((y[-i]-y[i])/hy))/(hx*hy)
  }
  xloocv <- numeric(n)
  for(i in 1:n){
    xloocv[i] <- mean(dnorm((x[-i]-x[i])/hx)/hx)
  }
  yloocv <- numeric(n)
  for(i in 1:n){
    yloocv[i] <- mean(dnorm((y[-i]-y[i])/hy)/hy)
  }
  I <- mean(jointloocv)+mean(xloocv)*mean(yloocv)-2*mean(xloocv*yloocv)
  X <- matrix(0,nrow=n,ncol=n)
  Y <- matrix(0,nrow=n,ncol=n)
  for(i in 1:n){
    for(j in 1:n){
      if (j!=i){
        X[i,j] <- dnorm((x[i]-x[j])/hx)^2
        Y[i,j] <- dnorm((y[i]-y[j])/hy)^2
      }
    }
  }
  sigma <- sqrt(2*sum(X*Y)/(n^2*hx*hy))
  teststat <- n*sqrt(hx*hy)*I/sigma
  cat("The value of test statistic is", teststat,"\n")
  pvalue <- 1-pnorm(teststat) # One sided p-value
  cat("p-value =",pvalue,"\n")
  if(pvalue<0.05){
    cat("Since p value is smaller than 0.05, we reject the independence assumption under significant level 0.05.")
  }
  else{
    cat("Since p value is greater than 0.05, we accept the independence assumption under significant level 0.05.")
  }
}