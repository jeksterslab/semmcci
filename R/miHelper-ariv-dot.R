#' Average Relative Increase in Variance
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @details The average relative increase in variance
#'   is given by
#'   \deqn{
#'     \mathrm{ARIV}
#'     =
#'     \left( 1 + M^{-1} \right)
#'     \mathrm{tr}
#'     \left(
#'        \mathbf{V}_{\mathrm{between}}
#'        \mathbf{V}_{\mathrm{within}}^{-1}
#'     \right)
#'   }
#'
#' @param between Numeric matrix.
#'   Covariance between imputations
#'   \eqn{\mathbf{V}_{\mathrm{between}}}.
#' @param within Numeric matrix.
#'   Covariance within imputations
#'   \eqn{\mathbf{V}_{\mathrm{within}}}.
#' @param M Positive integer.
#'   Number of imputations.
#' @param k Positive integer.
#'   Number of parameters.
#'
#' @return Returns a numeric vector of length one.
#'
#' @references
#'   Li, K. H., Raghunathan, T. E., & Rubin, D. B. (1991).
#'   Large-sample significance levels from multiply imputed data
#'   using moment-based statistics and an F reference distribution.
#'   *Journal of the American Statistical Association*, 86 (416), 1065<U+2013>1073.
#'   \doi{10.1080/01621459.1991.10475152}
#'
#'   Rubin, D. B. (1987).
#'   *Multiple imputation for nonresponse in surveys*.
#'   John Wiley & Sons, Inc.
#'   \doi{10.1002/9780470316696}
#'
#' @family Multiple Imputation Helper Functions
#' @keywords miHelper combine
#' @noRd
.ARIV <- function(between,
                  within,
                  M,
                  k) {
  return(
    (
      (
        1 + (
          1 / M
        )
      ) * sum(
        diag(
          between %*% chol2inv(
            chol(
              within
            )
          )
        )
      )
    ) / k
  )
}
