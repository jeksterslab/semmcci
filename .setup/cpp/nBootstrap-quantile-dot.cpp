// -----------------------------------------------------------------------------
// edit .setup/cpp/nBootstrap-quantile-dot.cpp
// Ivan Jacob Agaloos Pesigan
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

//' Sample Quantiles, Type 7 (C++)
//'
//' Returns the sample quantiles of `x` at `probs`
//' with the same arithmetic as `stats::quantile(type = 7)`:
//' with \eqn{h = 1 + (n - 1) p}, \eqn{l = \lfloor h \rfloor},
//' and \eqn{u = \lceil h \rceil},
//' the quantile is \eqn{(1 - (h - l)) x_{(l)} + (h - l) x_{(u)}},
//' or \eqn{x_{(l)}} when \eqn{h = l} or \eqn{x_{(u)} = x_{(l)}}.
//' `x` has no missing values (the callers drop them).
//' An empty `x` gives `NA`, as in R;
//' a probability outside \eqn{\left[ 0, 1 \right]} or missing stops.
//'
//' @author Ivan Jacob Agaloos Pesigan
//'
//' @param x Numeric vector.
//'   Draws, without missing values.
//' @param probs Numeric vector.
//'   Probabilities.
//'
//' @return Numeric vector of the length of `probs`.
//'
//' @examples
//' .Quantile(x = c(3, 1, 2, 5), probs = c(0.025, 0.5, 0.975))
//'
//' @noRd
// [[Rcpp::export(.Quantile)]]
Rcpp::NumericVector Quantile(const arma::vec& x, const arma::vec& probs) {
  const arma::vec xs = arma::sort(x);
  const arma::uword n = xs.n_elem;
  Rcpp::NumericVector out(probs.n_elem, NA_REAL);
  if (n == 0) {
    return out;
  }
  const double nm1 = static_cast<double>(n - 1);
  for (arma::uword i = 0; i < probs.n_elem; i++) {
    if (!(probs(i) >= 0.0 && probs(i) <= 1.0)) {
      Rcpp::stop("probabilities must be in [0, 1].");
    }
    // 1-based index as in R
    const double index = 1.0 + nm1 * probs(i);
    const double lo = std::floor(index);
    const double hi = std::ceil(index);
    const double qs = xs(static_cast<arma::uword>(lo) - 1);
    const double xhi = xs(static_cast<arma::uword>(hi) - 1);
    if (index > lo && xhi != qs) {
      const double h = index - lo;
      out[i] = (1.0 - h) * qs + h * xhi;
    } else {
      out[i] = qs;
    }
  }
  return out;
}
