#include <RcppArmadillo.h>
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

  arma::uword n = U.n_rows;
  arma::uword B = U.n_cols;
  arma::uword K = inf_func.n_cols;

  arma::mat innerMat(n, K);
  arma::vec Ub(n);
  double innerSum;
  arma::mat outMat(B, K);

  for (arma::uword b = 0; b < B; b++) {
    Ub = U.col(b);
    innerMat = inf_func.each_col() % Ub;
    for (arma::uword k = 0; k < K; k++) {
      innerSum = 0;
      for (arma::uword i = 0; i < n; i++) {
        innerSum += innerMat(i, k);
      }
      outMat(b, k) = innerSum / n;
    }
  }

  return (outMat);
}

// In-place sample iid Rademacher weights.
//
// Each call to R::unif_rand() provides at most 32 random bits, and only 30 of
// them under the Knuth-TAOCP generators, where the two lowest bits are always
// zero. To be correct under every RNG kind (including user-supplied ones, whose
// resolution is unknown), we take only the top 16 bits of each draw, which is
// the largest power-of-two block we can trust, and use one bit per weight.
// That costs two unif_rand() calls per 32 weights, still far fewer than the
// one call per weight that drawing each weight separately would need.
inline void fill_rademacher_mat(arma::mat &U) {
  arma::uword n = U.n_elem;
  double *p = U.memptr();

  for (arma::uword i = 0; i < n; i += 16) {
    uint32_t bits = static_cast<uint32_t>(R::unif_rand() * 65536.0);
    arma::uword block = n - i < 16 ? n - i : 16;
    for (arma::uword j = 0; j < block; ++j, ++p) {
      *p = static_cast<double>((bits >> j) & 1u) * 2.0 - 1.0;
    }
  }
}

// The implementation behind multiplier_bootstrap(). `inf_func` is read from
// memory once instead of once per bootstrap iteration: each tile of rows is
// combined with all `biters` draws,
//   out += U_tile' %*% inf_func_tile
// and summing across tiles gives the result. The weights are drawn per tile,
// identically for both accumulation methods, so `use_gemm` changes the speed
// but not the draws.
// [[Rcpp::export]]
arma::mat multiplier_bootstrap_cpp(arma::mat inf_func, int biters,
                                   bool use_gemm) {
  arma::uword n = inf_func.n_rows;
  arma::uword K = inf_func.n_cols;
  arma::uword B = static_cast<arma::uword>(biters);

  arma::mat outMat(B, K, arma::fill::zeros);

  arma::uword tile_n = 4096;
  {
    // Bound the U tile to ~32MB even for very large `biters`.
    arma::uword max_tile = (32u * 1024u * 1024u) / (8u * (B > 1 ? B : 1));
    if (tile_n > max_tile) {
      tile_n = max_tile > 0 ? max_tile : 1;
    }
  }

  for (arma::uword i0 = 0; i0 < n; i0 += tile_n) {
    arma::uword rows = tile_n < n - i0 ? tile_n : n - i0;

    arma::mat U(rows, B);
    fill_rademacher_mat(U);

    // A contiguous copy, so that the per-draw path gets plain dot products
    // and the tiled path does not have to copy the subview itself.
    arma::mat tile(inf_func.rows(i0, i0 + rows - 1));

    if (use_gemm) {
      outMat += U.t() * tile;
    } else {
      for (arma::uword b = 0; b < B; ++b) {
        for (arma::uword k = 0; k < K; ++k) {
          outMat(b, k) += arma::dot(tile.col(k), U.col(b));
        }
      }
    }
  }

  return outMat / static_cast<double>(n);
}
