#include <Rcpp.h>
using namespace Rcpp;

//' @title Random Walk Metropolis sampler (Laplace Distribution)
//' @description A MCMC sampler using Rcpp
//' @param sigma the scaling parameter for increment distribution
//' @param x0 the initial value
//' @param N the sample size you want
//' @return a random sample of size \code{N}
//' @useDynLib SC19092
//' @examples
//' \dontrun{
//' Crw <- rwcpp(1, 0, 2000)
//' chain <- Crw[[1]]
//' plot(1:1000,chain[1:1000],type="l",xlab = "Scaling parameter = 1",ylab="Chain",ylim=range(chain))
//' }
//' @export
// [[Rcpp::export]]
List rwcpp(double sigma, double x0, int N){
  NumericVector x(N);
  NumericVector u = runif(N);
  int k = 0;
  x[0] = x0;
  for (int i = 1; i < N; i++){
    double y = rnorm(1, x[i-1], sigma)[0];
    if (u[i] <= (exp(-abs(y))/exp(-abs(x[i-1])))){
      x[i] = y;
    } else {
      x[i] = x[i-1];
      k++;
    }
  }
  return List::create(x, k);
}
