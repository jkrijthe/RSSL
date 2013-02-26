#include <RcppArmadillo.h> 
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
NumericVector LeastSquaresClassifierXY(NumericVector X_in, NumericVector y_in) {
  arma::mat X = Rcpp::as<arma::mat>(X_in);
  arma::mat y = Rcpp::as<arma::mat>(y_in);
  
  return Rcpp::as<Rcpp::NumericVector>(wrap(solve(X,y))); 
}

// [[Rcpp::export]]
NumericVector ICLeastSquaresClassifierXY(NumericVector X_in, NumericVector y_in) {
  arma::mat X = Rcpp::as<arma::mat>(X_in);
  arma::mat y = Rcpp::as<arma::mat>(y_in);
  
  return Rcpp::as<Rcpp::NumericVector>(wrap(solve(X,y))); 
}