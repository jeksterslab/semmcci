#' Sampling Distribution of Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param scale Numeric matrix
#'   Sampling variance-covariance matrix of parameter estimates..
#' @param location Numeric vector.
#'   Vector of parameter estimates.
#' @param decomposition Character string.
#'   Matrix decomposition of the sampling variance-covariance matrix for the data generation.
#'   If `decomposition = "chol"`, use Cholesky decomposition.
#'   If `decomposition = "eigen"`, use eigenvalue decomposition.
#'   If `decomposition = "svd"`, use singular value decomposition.
#'   If `decomposition = NULL`, try Cholesky decomposition.
#'   If Cholesky decomposition fails, try eigenvalue decomposition.
#'   Finally, if eigenvalue decomposition fails, try singular value decomposition.
#' @param pd Logical.
#'   If `pd = TRUE`, check if the sampling variance-covariance matrix is positive definite using `tol`.
#' @param tol Numeric.
#'   Tolerance used for `pd`..
#' @return Returns a list with the following elements:
#' \describe{
#'   \item{`thetahatstar`}{Sampling distribution of parameter estimates.}
#'   \item{`decomposition`}{Matrix decomposition used to generate multivariate normal random variates.}
#' }
#' @keywords matrix standardized internal
#' @noRd
.ThetaStar <- function(R = 20000L,
                       scale,
                       location,
                       decomposition = NULL,
                       pd = FALSE,
                       tol = 1e-06) {
  if (is.null(decomposition)) {
    pd <- FALSE
  }
  if (pd) {
    mat <- eigen(
      scale,
      symmetric = TRUE,
      only.values = FALSE
    )
    npd <- !.TestPositiveDefinite(
      eigen = mat,
      tol = tol
    )
    if (npd) {
      stop(
        "The sampling variance-covariance matrix is nonpositive definite."
      )
    }
  }
  k <- length(location)
  n <- R
  norm <- matrix(
    data = stats::rnorm(
      n = n * k
    ),
    nrow = n,
    ncol = k
  )
  run_chol <- FALSE
  run_eigen <- FALSE
  run_svd <- FALSE
  if (is.null(decomposition)) {
    run_chol <- TRUE
  } else {
    stopifnot(
      decomposition %in% c(
        "chol",
        "eigen",
        "svd"
      )
    )
    if (decomposition == "chol") {
      run_chol <- TRUE
    }
    if (decomposition == "eigen") {
      run_eigen <- TRUE
    }
    if (decomposition == "svd") {
      run_svd <- TRUE
    }
  }
  if (run_chol) {
    mat <- tryCatch(
      {
        chol(scale)
      },
      warning = function(w) {
        return(NULL)
      },
      error = function(e) {
        return(NULL)
      }
    )
    if (is.null(mat)) {
      if (is.null(decomposition)) {
        run_eigen <- TRUE
      } else {
        if (decomposition == "chol") {
          stop(
            "Error in Cholesky decomposition. Try using `decomposition = \"eigen\"`."
          )
        }
      }
    } else {
      output <- .MVNChol(
        norm = norm,
        mat = mat
      )
      mvn <- "chol"
    }
  }
  if (run_eigen) {
    if (!pd) {
      mat <- eigen(
        scale,
        symmetric = TRUE,
        only.values = FALSE
      )
      npd <- !.TestPositiveDefinite(
        eigen = mat,
        tol = tol
      )
      if (npd) {
        if (is.null(decomposition)) {
          run_svd <- TRUE
        } else {
          if (decomposition == "eigen") {
            stop(
              "Error in eigenvalue decomposition. Try using `decomposition = \"svd\"`."
            )
          }
        }
      }
    }
    if (!npd) {
      output <- .MVNEigen(
        norm = norm,
        mat = mat
      )
      mvn <- "eigen"
    }
  }
  if (run_svd) {
    output <- .MVNSVD(
      norm = norm,
      mat = svd(scale)
    )
    mvn <- "svd"
  }
  # add location
  output <- output + rep(
    x = location,
    times = rep(
      x = n,
      times = k
    )
  )
  colnames(
    output
  ) <- names(
    location
  )
  return(
    list(
      thetahatstar = output,
      decomposition = mvn
    )
  )
}
