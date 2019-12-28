#' Title Sampling from laplace distribution
#'
#' @param sigma scaling parameter for increment
#' @param x0 initial value
#' @param N sample size
#'
#' @return a list containing sample and number of rejections
#' @export
#'
#' @examples
#' \dontrun{
#' rw.Metropolis(1,0,100)
#' }
rw.Metropolis <- function(sigma, x0, N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= (exp(-abs(y)) / exp(-abs(x[i-1]))))
      x[i] <- y else {
        x[i] <- x[i-1]
        k <- k + 1
      }
  }
  return(list(x=x, k=k))
}