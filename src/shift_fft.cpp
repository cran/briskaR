#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]

arma::mat shift_fft_Cpp(arma::mat xx) {
  
  int i = xx.n_rows;
  int j = xx.n_cols;
  
  int i2 = i / 2;
  int j2 = j / 2;
  
  int i2pred = i2 -1;
  int j2pred = j2 -1;
  
  arma::mat A = xx.submat(0, 0, i2pred, j2pred); // (from_row, from_col, to_row, to_col) ;
  arma::mat B = xx.submat(i2, 0, i-1, j2pred); 
  arma::mat C = xx.submat(0, j2, i2pred, j-1);
  arma::mat D = xx.submat(i2, j2, i-1, j-1); 
  
  arma::mat y1 = arma::join_rows(D, B);
  arma::mat y2 = arma::join_rows(C, A);
  
  // arma::field<arma::mat> F(2,1);
  // F(0,0) = y1;
  // F(1,0) = y2;
  
  arma::mat yy = arma::join_cols(y1, y2);
  
  return yy;
}
