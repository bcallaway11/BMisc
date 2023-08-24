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

// In-place sample rademacher
void fill_rademacher(arma::vec &v)
{
  size_t n = v.size();

  // Calculate the number of integers needed based on N
  int num_integers = ceil(n / 31.0);

  // 2^31 - 1 = 2147483647
  IntegerVector random_integer = Rcpp::sample(2147483647, num_integers, true);

  int k = 0;
  int J = 30;
  for (int i = 0; i < num_integers - 1; ++i)
  {
    int curr = random_integer[i];

    for (int j = J; j >= 0; j--)
    {
      v[k] = ((curr >> j) & 1) * 2 - 1;
      k = k + 1;
    }
  }

  int j = J;
  for (; k < n; ++k, --j)
  {
    v[k] = ((random_integer[num_integers - 1] >> j) & 1) * 2 - 1;
  }
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
arma::mat multiplier_bootstrap(arma::mat inf_func, int biters)
{
  int n = inf_func.n_rows;
  int K = inf_func.n_cols;
  arma::vec Ub(n);
  arma::mat outMat(biters, K);

  for (size_t b = 0; b < biters; ++b)
  {
    fill_rademacher(Ub);
    for (size_t j = 0; j < K; ++j)
    {
      outMat.at(b, j) = arma::dot(inf_func.col(j), Ub);
    }
  }

  return (outMat / n);
}
