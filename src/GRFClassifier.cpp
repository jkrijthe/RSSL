#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat harmonic_function_cpp(const arma::mat& W, const arma::mat& Y) {
  int l = Y.n_rows; // the number of labeled points
  int n = W.n_rows; // total number of points

  // the graph Laplacian L=D-W
  arma::mat L = diagmat(sum(W,0)) - W;
  //  
  

  //  # the harmonic function.
  //  fu <- -solve(L[(l+1):n, (l+1):n]) %*% L[(l+1):n, 1:l] %*% Y
  arma::mat fu = -solve(L(arma::span(l, n-1), arma::span(l, n-1)), L(arma::span(l, n-1), arma::span(0, l-1)) * Y);
  return(fu);
}
