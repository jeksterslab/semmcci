#' Generate Data from the Multivariate Normal Distribution
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param norm `n` by `k` numeric matrix of standard Gaussians.
#' @param mat Eigen decomposition of the covariance matrix.
#' @keywords mvn internal
#' @noRd
.MVNEigen <- function(norm,
                      mat) {
  norm %*% (
    t(mat$vectors) * sqrt(
      pmax(
        mat$values,
        0
      )
    )
  )
}
