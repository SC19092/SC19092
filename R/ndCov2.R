#' Title Distance Correlation for Bootstrap
#'
#' @param z all sample
#' @param ix index for Bootstrap
#' @param dims dimensions of two sample
#'
#' @return test statistic
#' @export
#'
#' @examples
#' \dontrun{
#' ndCov2(1:100,1:100,c(40,60))
#' }
ndCov2 <- function(z, ix, dims) {
  p <- dims[1]
  q1 <- dims[2] + 1
  d <- p + dims[2]
  x <- z[ , 1:p] 
  y <- z[ix, q1:d] #permute rows of y
  return(nrow(z) * dCov(x, y)^2)
}