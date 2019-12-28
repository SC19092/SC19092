#' Title EM algorithm for Bolld type
#'
#' @param N the number of random initial values
#' @param nA number of cases A
#' @param nB number of cases B
#' @param nO number of cases O
#' @param nAB number of cases AB
#' @param n number of cases
#'
#' @return the MLE of p, q
#' @export
#'
#' @examples
#' \dontrun{
#' EMABO(1,28,24,41,70,163)
#' }
EMABO <- function(N,nA,nB,nO,nAB,n){ 
  maxp <- maxq <- maxr <- 0
  lmax <- -Inf
  likemax <- NULL
  for(i in 1:N){
    aa <- runif(3)
    p0 <- aa[1]/sum(aa)
    q0 <- aa[2]/sum(aa)
    r0 <- aa[3]/sum(aa)
    like <- NULL
    l0 <- log_like(p0,q0,r0,nA,nB,nO,nAB)
    like <- c(like,l0)
    while(1){
      # E-Step
      nAA <- nA*p0/(p0+2*r0)
      nAO <- nA*2*r0/(p0+2*r0)
      nBB <- nB*q0/(q0+2*r0)
      nBO <- nB*2*r0/(q0+2*r0)
      # M-Step
      p1 <- (2*nAA+nAO+nAB)/(2*n)
      q1 <- (2*nBB+nBO+nAB)/(2*n)
      r1 <- 1-p1-q1
      l1 <- log_like(p1,q1,r1,nA,nB,nO,nAB)
      like <- c(like,l1)
      if(abs(l0-l1)<0.0001)
        break
      else{
        p0 <- p1
        q0 <- q1
        r0 <- r1
        l0 <- l1
      }
    }
    if(l1>lmax){
      maxp <- p1
      maxq <- q1
      maxr <- r1
      lmax <- l1
      likemax <- like
    }
  }
  return(list(p=maxp,q=maxq,r=maxr,like=likemax))
}