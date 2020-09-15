#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector nigCpp(NumericVector x, NumericVector y,
                     double a1, double a2, double b1, double b2, double b3, double theta, double pi) {
  
  double lambda_x = b1 * cos(theta);
  double lambda_y = b2 * sin(theta);
  
  double p = pow(b3, 2.0) + pow(lambda_x, 2.0) + pow(lambda_y, 2.0) ;
  
  NumericVector q = 1 + pow(a1, 2.0) * pow(x, 2.0) + pow(a2, 2.0) * pow(y, 2.0) ;
  double A = a1 * a2 * exp(b3) / (2 * pi) ; // faster to take pi from R in argument
  NumericVector B = (pow(q, -0.5) + pow(p, 0.5) ) / q ;
  NumericVector C = exp(-sqrt(p * q)) * exp(lambda_x * a1 * x + a2 * lambda_y * y) ;
  
  return A*B*C;
}

// [[Rcpp::export]]
NumericVector geometricCpp(NumericVector x, NumericVector y,
                           double aa, double pi){
  
  NumericVector A = pow(1 + pow(x*x + y*y, 0.5), aa) ;
  double B = ((aa + 1) * (aa + 2)) / (2 * pi) ;
  return A * B ;
}

// [[Rcpp::export]]
NumericVector FatTailCpp(NumericVector x, NumericVector y, double a, double b) {
  return( a * pow(pow(pow(x, 2.0) + pow(y, 2.0),0.5), b)) ;
}

// [[Rcpp::export]]
NumericVector studentCpp(NumericVector x, NumericVector y,
                         double a, double b, double c1, double c2, double theta, double pi){
  
  double A = (b - 1) / (pi * a * a); 
  NumericVector B = pow((1 + (x * x + y * y) / (a * a)),-b);
  double C = exp(c1 * cos(theta - c2));
    
  return A*B*C ;
}
