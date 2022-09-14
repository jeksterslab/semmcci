#' Generate Data from the Multivariate Normal Distribution
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param norm `n` by `k` numeric matrix of standard Gaussians.
#' @param mat Cholesky decomposition of the covariance matrix.
#' @keywords mvn internal
#' @noRd
.MVNChol <- function(norm,
                     mat) {
  norm %*% mat
}
