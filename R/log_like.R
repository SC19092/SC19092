#' log-likelihood of incomplete likelihood
#'
#' @param p parameter p
#' @param q parameter q
#' @param r parameter r
#' @param nA number of type A
#' @param nB number of type B
#' @param nO number of type O
#' @param nAB number of type AB
#'
#' @return log-likelihood
#' @export
#'
#' @examples
#' \dontrun{
#' log_like(1/3,1/3,1/3,28,24,41,70)
#' }
log_like <- function(p,q,r,nA,nB,nO,nAB){  
  a1 <- nA*log(p^2+2*p*r)
  a2 <- nB*log(q^2+2*q*r)
  a3 <- 2*nO*log(r)
  a4 <- nAB*log(2*p*q)
  return(a1+a2+a3+a4)
}