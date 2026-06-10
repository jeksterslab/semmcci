#' Adjusted Total Sampling Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @details The adjusted total sampling covariance matrix
#'   is given by
#'   \deqn{
#'     \tilde{\mathbf{V}}_{\mathrm{total}}
#'     =
#'     \left( 1 + \mathrm{ARIV} \right)
#'     \mathbf{V}_{\mathrm{within}}
#'   }
#'
#' @param ariv Numeric.
#'   Average relative increase in variance.
#' @param within Numeric matrix.
#'   Covariance within imputations
#'   \eqn{\mathbf{V}_{\mathrm{within}}}.
#'
#' @references
#'   Li, K. H., Raghunathan, T. E., & Rubin, D. B. (1991).
#'   Large-sample significance levels from multiply imputed data
#'   using moment-based statistics and an F reference distribution.
#'   *Journal of the American Statistical Association*,
#'   86 (416), 1065–1073.
#'   \doi{10.1080/01621459.1991.10475152}
#'
#' Rubin, D. B. (1987).
#' *Multiple imputation for nonresponse in surveys*.
#' John Wiley & Sons, Inc.
#' \doi{10.1002/9780470316696}
#'
#' @family Multiple Imputation Helper Functions
#' @keywords miHelper combine
#' @noRd
.TotalAdj <- function(ariv,
                      within) {
  (1 + ariv) * within
}
