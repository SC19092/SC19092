#' Generate the random numbers from the mixture distribution
#'
#' @param n The sample size
#' @param p1 Proportion
#' @importFrom stats rnorm
#' @return Sample following mixture gaussian
#' @export
#'
#' @examples
#' \dontrun{
#' mix_gauss(10,0.4)
#' }
mix_gauss <- function(n,p1){  
  p <- sample(0:1,n,replace=T,prob=c(1-p1,p1))
  x1 <- rnorm(n)
  x2 <- rnorm(n,3,1)
  x <- x1*p+x2*(1-p)
  return(x)
}