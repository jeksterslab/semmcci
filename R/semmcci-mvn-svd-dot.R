#' Generate Data from the Multivariate Normal Distribution
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param norm `n` by `k` numeric matrix of standard Gaussians.
#' @param mat Singular value decomposition of the covariance matrix.
#' @keywords mvn internal
#' @noRd
.MVNSVD <- function(norm,
                    mat) {
  norm %*% mat$u %*% (
    t(mat$v) * sqrt(
      pmax(
        mat$d,
        0
      )
    )
  )
}
