#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]


//' @title element_wise_mult
//'
//' @description This is a function that takes in two matrices of dimension
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


//' Sample Rademacher distribution really fast
//' 
//' @description This uses a fancy trick to draw Rademacher weights very 
//'   quickly. To do so, the function draws from 1:(2^31 - 1), and then 
//'   uses each bit of the integer to determine 31 values of 0/1. This
//'   allows for 31 Rademacher random variables to be drawn per random draw.
//'   Taking those bits * 2 - 1 gives the Rademacher random variables.
//'
//' @param n Integer, number of random variables to draw
//' 
//' @return integer vector of length n with values -1 or 1
//' 
//' @export
arma::vec sample_rademacher(int n) {
  if (n < 0) {
    stop("n must be a positive integer");
  }
  if (n > 2147483647) {
    stop("n must be less than 2147483647");
  }

  arma::vec result(n);

  // Calculate the number of integers needed based on N
  int num_integers = ceil(n / 31.0);

  // 2^31 - 1 = 2147483647
  IntegerVector random_integer = Rcpp::sample(2147483647, num_integers, true);
  
  int k = 0;
  int J = 30;
  for (int i = 0; i < num_integers; i++) {
    int curr = random_integer[i];

    // Make sure not to overfill the result vector
    if (i == num_integers - 1) {
      J = (n % 31) - 1;
    } 
    
    for (int j = J; j >= 0; j--) {
      
      result[k] = ((curr >> j) & 1) * 2 - 1;
      k = k + 1;
    }
  }

  return result;
}

//' @title multiplier_bootstrap
//'
//' @description A function that takes in an influence function (an
//' nxk matrix) and the number of bootstrap iterations and
//' returns a Bxk matrix of bootstrap results. This function
//' uses Rademechar weights.
//' 
//' @param inf_func nxk matrix of (e.g., these could be a matrix
//'  containing the influence function for different parameter
//'  estimates)
//' @param biters the number of bootstrap iterations
//'
//' @return a Bxk matrix
//' @export
// [[Rcpp::export]]
arma::mat multiplier_bootstrap(arma::mat inf_func, int biters) {

  int n = inf_func.n_rows;
  int K = inf_func.n_cols;

  arma::mat innerMat(n,K);
  arma::vec Ub(n);
  //double innerSum;
  arma::mat outMat(biters,K);


  for (int b=0; b<biters; b++) {
    // draw Rademechar weights
    Ub = sample_rademacher(n);
    //Rcout << "Ub : " << Ub << "\n";
    innerMat = inf_func.each_col() % Ub;
    //Rcout << "innerMat : " << innerMat << "\n";
    //Rcout << "sum: " << arma::sum(innerMat, 0)/n;
    outMat.row(b) = arma::sum(innerMat, 0)/n;
    //outMat(b,_) = meanCpp(innerMat.each_col());
  }
  
  return(outMat);

}
