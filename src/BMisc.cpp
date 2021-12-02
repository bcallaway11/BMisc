#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]


//' element_wise_mult
//'
//' This is a function that takes in two matrices of dimension
//' nxB and nxk and returns a Bxk matrix that comes from
//' element-wise multiplication of every column 
//' in the first matrix times the entire second matrix and the
//' averaging over the n-dimension.  It is equivalent (but faster
//' than) the following R code:
//' `sapply(1:biters, function(b) sqrt(n)*colMeans(Umat[,b]*inf.func))`
//' .  This function is particularly useful for fast computations
//' using the multiplier bootstrap.
//'
//' @param U nxB matrix (e.g., these could be a matrix of
//'  Rademachar weights for B bootstrap iterations using the
//'  multiplier bootstrap
//' @param inf_func nxk matrix of (e.g., these could be a matrix
//'  containing the influence function for different parameter
//'  estimates)
//'
//' @return a Bxk matrix
//' @export
// [[Rcpp::export]]
arma::mat element_wise_mult(arma::mat U, arma::mat inf_func) {

  int n = U.n_rows;
  int B = U.n_cols;
  int K = inf_func.n_cols;

  arma::mat innerMat(n,K);
  arma::vec Ub(n);
  double innerSum;
  arma::mat outMat(B,K);
  
  for (int b=0; b < B; b++) {
    Ub = U.col(b);
    innerMat = inf_func.each_col() % Ub;
    for (int k=0; k < K; k++) {
      innerSum = 0;
      for (int i=0; i < n; i++) {
	innerSum += innerMat(i,k);
      }
      outMat(b,k) = innerSum/n;
    }
  }
      
  return(outMat);

}
