#' A function to calculate skewnwss from samples
#'
#' @param x the sample
#'
#' @return skewness
#' @export
#'
#' @examples
#' \dontrun{
#' skew(rnorm(100))
#' }
skew <- function(x){  
  numerator <- mean((x-mean(x))^3)
  denominator <- mean((x-mean(x))^2)^(1.5)
  return(numerator/denominator)
}