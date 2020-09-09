#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat rowMax(const arma::mat& X) {
  arma::mat A = arma::zeros<arma::mat>(X.n_rows,1);
  
  for (unsigned int i=0; i<X.n_rows; i++) {
    // Rcout << X.row(i).max() << std::endl;
    A(i) = X.row(i).max();
  }
  return(A);
}

// [[Rcpp::export]]
arma::colvec rowMax2(const arma::mat& X) {
  arma::colvec A = max(X,1);
  return(A);
}

// [[Rcpp::export]]
arma::mat which_rowMax(const arma::mat& X) {
  arma::mat A = arma::zeros<arma::mat>(X.n_rows,1);
  
  arma::uword  row;
  arma::uword  col;
  
  for (unsigned int i=0; i<X.n_rows; i++) {
    X.row(i).max(row,col);
    A(i) = col+1;
  }
  return(A);
}

// [[Rcpp::export]]
arma::mat which_rowMax2(const arma::mat& X) {
  arma::ucolvec I = index_max(X,1);
  
  arma::mat A = arma::zeros<arma::mat>(X.n_rows,1);
  for (unsigned int i=0; i<X.n_rows; i++) {
    A(i) = I(i)+1;
  }
  return(A);
}

// [[Rcpp::export]]
arma::mat sort_matrix(const arma::mat& X) {
  arma::mat A = sort(X,"descend",1);
  return(A);
}

// [[Rcpp::export]]
arma::mat rowwise_addition(arma::mat A, arma::rowvec x) 
{ 
  A.each_row() += x;
  return A;
}

// [[Rcpp::export]]
arma::mat factor_to_dummy_cpp(Rcpp::IntegerVector y, int c) {

  Rcpp::IntegerVector xy(y);
  
  int n = xy.length();
  arma::mat Y = arma::zeros<arma::mat>(n,c);
  
  for (int i=0; i<n; i++) {
    Y(i,xy[i]-1) = 1; 
  }
  
  return(Y);
}
