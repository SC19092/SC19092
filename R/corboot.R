#' Bootstrap the correlation
#'
#' @param x data
#' @param B Number of Bootstrap replications
#'
#' @return estimated standard deviation
#' @export
#'
#' @examples
#' \dontrun{
#' f(faithful,1000)
#' }
corboot <- function(x,B){
  n <- nrow(x)
  cors <- numeric(B)
  for(b in 1:B){
    i <- sample(1:n,n,replace=T)
    cors[b] <- cor(x[i,1],x[i,2])
  }
  return(sd(cors))
}