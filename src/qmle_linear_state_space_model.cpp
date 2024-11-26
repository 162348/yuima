// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::export]]
double minusloglcpp_linear_state_space_theta1(arma::cube observed_diffusion, arma::mat dx, double h, int drop_terms) {
    // observed_diffusion : 3-dim array of evaluated observed diffusion coefficients
    // dx : 2-dim matrix of delta of observerd variables in each time point
    // h : interval of observations
    int n_data = observed_diffusion.n_slices;

    // calculate quasi-log-likelihood
    double QL = 0;
    arma::mat sq_observed_diffusion(observed_diffusion.n_rows,observed_diffusion.n_rows);
    arma::mat inv_sq_observed_diffusion(observed_diffusion.n_rows,observed_diffusion.n_rows);
    double log_det_sq_observed_diffusion;
    for(int i = 1; i < n_data; i++){
        sq_observed_diffusion = observed_diffusion.slice(i-1) * observed_diffusion.slice(i-1).t();
        try {
            log_det_sq_observed_diffusion = log(arma::det(sq_observed_diffusion));
            inv_sq_observed_diffusion = arma::inv_sympd(sq_observed_diffusion);
        }
        catch (std::runtime_error&) {
            return double(-1e10);
        }
    	if(drop_terms < i){
                QL += -0.5 * log_det_sq_observed_diffusion;
                QL += arma::as_scalar((-1/(2*h)) * dx.row(i-1) * inv_sq_observed_diffusion * dx.row(i-1).t());
    	}
    }
    return QL;
}

// [[Rcpp::export]]
double minusloglcpp_linear_state_space_theta2(arma::mat observed_drift, arma::mat inv_sq_observed_diffusion, arma::mat dx, double h, int drop_terms) {
    // observed_drift : 2-dim matrix of evaluated observed drift coefficients
    // inv_sq_observed_diffusion : (\sigma\sigma^T)^{-1} \sigma is 2-dim array of evaluated observed diffusion coefficients)
    // dx : 2-dim matrix of delta of observerd variables in each time point
    // h : interval of observations
    int d_ob = observed_drift.n_rows;
    int n_data = observed_drift.n_cols;

    // calculate quasi-log-likelihood
    double QL = 0;
    for(int i = drop_terms+1; i < n_data; i++){
      arma::vec observed_drift_col(&observed_drift(0, i-1), d_ob, false, true);
      arma::vec dx_col(&dx(0, i-1), d_ob, false, true);
      arma::vec tmp = observed_drift_col * h - dx_col;
      QL += arma::as_scalar(tmp.t() * inv_sq_observed_diffusion * tmp);
    }
    return -QL/2/h;
}

// [[Rcpp::export]]
arma::mat calc_inverce_square(arma::cube cube) {
  // calculate (AA^T)^(-1) for each slice.
  arma::cube res(cube.n_rows, cube.n_rows, cube.n_slices);
  int slices = cube.n_slices;
  for(int i = 0; i < slices; i++){
    res.slice(i) = arma::inv_sympd(cube.slice(i-1) * cube.slice(i-1).t());
  }
  
  return res;
}