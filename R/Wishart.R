#' Generate N random samples from Wishart distribution (with degrees of freedom n)
#' @import stats 
#' @param N sample size
#' @param n degree of freedom
#' @param Sigma parameter
#' 
#' @return N matrix
#' @export
#'
#' @examples
#' \dontrun{
#' Wishart(1,4,diag(2))
#' }
Wishart <- function(N,n,Sigma){ 
  d <- nrow(Sigma)
  R <- chol(Sigma)
  L <- list()
  for (k in 1:N){
    T1 <- matrix(0,nrow=d,ncol=d)
    for (i in 2:d){
      for (j in (1:(i-1))){
        T1[i,j] = rnorm(1)
      }
    }
    for (i in 1:d) T1[i,i] = sqrt(rchisq(1,n-i+1))
    A <- T1 %*% t(T1)
    L [[k]] <- t(R) %*% A %*% R
  }
  L
}