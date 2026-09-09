#include <RcppArmadillo.h>
#include <algorithm>
#include <cstdint>
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

// In-place sample iid Rademacher weights.
inline void fill_rademacher_mat(arma::mat &U)
{
  int n = U.n_rows * U.n_cols;
  double* p = U.memptr();

  int full = n / 32;
  for (int i = 0; i < full; ++i)
  {
    uint32_t bits = static_cast<uint32_t>(R::unif_rand() * 4294967296.0);
    for (int j = 0; j < 32; ++j, ++p)
    {
      *p = static_cast<double>((bits >> j) & 1) * 2.0 - 1.0;
    }
  }

  if (n % 32 > 0)
  {
    uint32_t bits = static_cast<uint32_t>(R::unif_rand() * 4294967296.0);
    for (int j = 0; j < n % 32; ++j, ++p)
    {
      *p = static_cast<double>((bits >> j) & 1) * 2.0 - 1.0;
    }
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

  arma::mat outMat(biters, K, arma::fill::zeros);

  // The original version was memory-bound, so that the slow part was re-reading 
  // the inf_func again and again (`biters` times!)
  // 
  // To optimize for cache locality, we want to read from inf_func only once.
  // What we do is grab a tile of size `n_smaller x K` from inf_func, 
  // draw all `biters` iterations of Rademacher weights, and matrix multiply 
  //   U_tile' * inf_func_tile
  // Then looping over each tile and summing across tiles gives the result
  // 
  int tile_n = 4096;
  {
    // Bound the U tile to ~32MB even for very large `biters`.
    long long max_tile = (32LL * 1024 * 1024) / (8LL * std::max(biters, 1));
    if (tile_n > max_tile)
    {
      tile_n = static_cast<int>(std::max(max_tile, 1LL));
    }
  }

  for (int i0 = 0; i0 < n; i0 += tile_n)
  {
    int rows = std::min(tile_n, n - i0);

    arma::mat U(rows, biters);
    fill_rademacher_mat(U);

    arma::subview<double> sub = inf_func.rows(i0, i0 + rows - 1);
    outMat += U.t() * sub;
  }

  return (outMat / n);
}
