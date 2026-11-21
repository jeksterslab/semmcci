#' Sampling Distribution of Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param scale Numeric matrix
#'   Sampling variance-covariance matrix of parameter estimates.
#' @param location Numeric vector.
#'   Vector of parameter estimates.
#' @param decomposition Character string.
#'   Matrix decomposition of the sampling variance-covariance matrix
#'   for the data generation.
#'   If `decomposition = "chol"`, use Cholesky decomposition;
#'   if the Cholesky decomposition fails
#'   (for example, a singular matrix),
#'   the eigenvalue decomposition is used
#'   and `decomposition` in the output is `"eigen"`.
#'   If `decomposition = "eigen"`, use eigenvalue decomposition.
#'   If `decomposition = "svd"`, use singular value decomposition.
#' @param pd Logical.
#'   If `pd = TRUE`,
#'   check if the sampling variance-covariance matrix
#'   is positive semidefinite using `tol`
#'   (a singular matrix, with a zero eigenvalue, passes).
#' @param tol Numeric.
#'   Tolerance used for `pd`.
#' @return Returns a list with the following elements:
#' \describe{
#'   \item{`thetahatstar`}{Sampling distribution of parameter estimates.}
#'   \item{`decomposition`}{Matrix decomposition
#'                          used to generate multivariate normal
#'                          random variates.}
#' }
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
#' @keywords semmcci parameters standardized internal
#' @noRd
.ThetaHatStar <- function(R = 20000L,
                          scale,
                          location,
                          decomposition = "eigen",
                          pd = TRUE,
                          tol = 1e-06) {
  if (pd) {
    mat <- eigen(
      x = scale,
      symmetric = TRUE,
      only.values = FALSE
    )
    npd <- !.TestPositiveSemiDefinite(
      eigen = mat,
      tol = tol
    )
    if (npd) {
      stop(
        "The sampling variance-covariance matrix is not positive semidefinite."
      )
    }
  }
  n <- R
  k <- length(x = location)
  z <- .RandomGaussianZ(
    n = n,
    k = k
  )
  if (decomposition == "chol") {
    # a singular matrix (for example, equality constraints)
    # can fail in chol(); use the eigenvalue decomposition then
    chol_scale <- tryCatch(
      expr = chol(x = scale),
      error = function(e) {
        NULL
      }
    )
    if (is.null(x = chol_scale)) {
      decomposition <- "eigen"
    } else {
      dist <- .RandomGaussianChol(
        Z = z,
        chol = chol_scale
      )
    }
  }
  if (decomposition == "eigen") {
    if (!pd) {
      mat <- eigen(
        x = scale,
        symmetric = TRUE,
        only.values = FALSE
      )
    }
    dist <- .RandomGaussianEigen(
      Z = z,
      eigen = mat
    )
  }
  if (decomposition == "svd") {
    dist <- .RandomGaussianSVD(
      Z = z,
      svd = svd(
        x = scale
      )
    )
  }
  dist <- .Location(
    X = dist,
    location = location,
    n = n,
    k = k
  )
  colnames(
    x = dist
  ) <- names(
    x = location
  )
  list(
    thetahatstar = dist,
    decomposition = decomposition
  )
}
