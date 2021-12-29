// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(RcppArmadillo)]]
#define STRICT_R_HEADERS
#include <RcppParallel.h>
#include <RcppArmadillo.h>
using namespace RcppParallel;
using namespace Rcpp;

struct MultiplierBootstrap : public Worker {

    // input matrix to read from
    const RMatrix<double> inf_func;

    // output matrix to write to
    RMatrix<double> rmat;

    int row_size;
    int col_size;

    // initialize from Rcpp input and output matrixes (the RMatrix class
    // can be automatically converted to from the Rcpp matrix type)
    MultiplierBootstrap(const NumericMatrix inf_func, NumericMatrix rmat,
                        const int row_size, const int col_size)
        : inf_func(inf_func), rmat(rmat), row_size(row_size), col_size(col_size) {}

    // convert RVector/RMatrix into arma type for Rcpp function
    // and the follwing arma data will be shared in parallel computing
    arma::mat convert()
    {
        RMatrix<double> tmp_mat = inf_func;
        arma::mat MAT(tmp_mat.begin(), row_size, col_size, false);
        return MAT;
    }

    // function call operator that work for the specified range (begin/end)
    void operator()(std::size_t begin, std::size_t end) {
        int n = inf_func.nrow();
        int K = inf_func.ncol();
        arma::vec Ub(n);
        arma::mat innerMat(n,K);

        for (std::size_t i = begin; i < end; i++) {
            Ub = arma::ones<arma::vec>(n) - 2*arma::round(arma::randu<arma::vec>(n));

            for(std::size_t k = 0; k < K; k ++) {
                RMatrix<double>::Column col = inf_func.column(k);
                double out = std::inner_product(col.begin(), col.end(), Ub.begin(), 0);
                rmat(i,k) = out/n;
            }
        }
    }
};

//' @title multiplier_bootstrap_parallel
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
NumericMatrix multiplier_bootstrap_parallel(NumericMatrix inf_func, int biters) {

    int n = inf_func.nrow();
    int K = inf_func.ncol();

    // allocate the matrix we will return
    NumericMatrix rmat(biters, K);

    // create the worker
    MultiplierBootstrap multiplierBootstrap(inf_func, rmat, n, K);

    // call it with parallelFor
    parallelFor(0, biters, multiplierBootstrap);

    return rmat;
}
