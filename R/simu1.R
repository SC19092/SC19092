#' Title Monte Carlo integration with antithetic variables
#'
#' @param n sample size
#' @param anti whether to use antithetic variables
#'
#' @return estimated integral
#' @export
#'
#' @examples
#' \dontrun{
#' simu1(100,T)
#' }
simu1 <- function(n,anti=T){
  sample1 <- runif(n/2)
  if (anti) sample <- c(sample1,1-sample1)
  else sample <- c(sample1,runif(n/2))
  y <- exp(-sample)/(1+sample^2)
  return(mean(y))
}