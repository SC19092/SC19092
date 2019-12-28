#' Title Compute max out statistic
#'
#' @param x first sample
#' @param y second sample
#'
#' @return max out statistic
#' @export
#'
#' @examples
#' \dontrun{
#' maxout(1:10,5:14)
#' }
maxout <- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  return(max(c(outx, outy)))
}