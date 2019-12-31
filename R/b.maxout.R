#' maxout statistic for resampling
#'
#' @param z data
#' @param index resampling index
#' @param dims dims of x and y
#'
#' @return maxout statistic
#' @export
#'
#' @examples
#' \dontrun{
#' b.maxout(1:100,51:150,c(50,50))
#' }
b.maxout <- function(z,index,dims){
  n1 <- dims[1]
  n2 <- dims[2]
  z <- z[index]
  x <- z[1:n1]
  y <- z[(n1+1):n2]
  return(maxout(x,y))
}