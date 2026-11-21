#' Combine Multiple Imputation Estimates and Sampling Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @details The pooled vector of parameter estimates is given by
#'   \deqn{
#'     \bar{\boldsymbol{\theta}}
#'     =
#'     M^{-1}
#'     \sum_{m = 1}^{M}
#'     \hat{\boldsymbol{\theta}}_{m}
#'   }
#'   where \eqn{M} is the number of imputations,
#'   \eqn{m = \left\{ 1, 2, \cdots, M \right\}},
#'   and
#'   \eqn{\hat{\boldsymbol{\theta}}_{m}}
#'   is the vector of parameter estimates for the
#'   \eqn{m^{\mathrm{th}}}
#'   imputation.
#'
#'   The pooled or total sampling variance-covariance matrix
#'   consists of combining between and within imputation variances
#'   given by
#'   \deqn{
#'     \boldsymbol{V}_{\mathrm{within}}
#'     =
#'     M^{-1}
#'     \sum_{m = 1}^{M}
#'     \operatorname{Var}
#'     \left(
#'     \hat{\boldsymbol{\theta}}_{m}
#'     \right)
#'   }
#'
#'   \deqn{
#'     \boldsymbol{V}_{\mathrm{between}}
#'     =
#'     \left(
#'     M - 1
#'     \right)^{-1}
#'     \sum_{m = 1}^{M}
#'     \left(
#'     \hat{\boldsymbol{\theta}}_{m}
#'     -
#'     \bar{\boldsymbol{\theta}}
#'     \right)
#'     \left(
#'     \hat{\boldsymbol{\theta}}_{m}
#'     -
#'     \bar{\boldsymbol{\theta}}
#'     \right)^{\prime}
#'   }
#'
#'   \deqn{
#'     \boldsymbol{V}_{\mathrm{total}}
#'     =
#'     \boldsymbol{V}_{\mathrm{within}}
#'     +
#'     \boldsymbol{V}_{\mathrm{between}}
#'     +
#'     M^{-1}
#'     \boldsymbol{V}_{\mathrm{between}} .
#'   }
#'
#'   An alternative total variance was introduced by
#'   Li, Raghunathan, and Rubin (1991)
#'   and is given by
#'   \deqn{
#'     \tilde{\boldsymbol{V}}_{\mathrm{total}}
#'     =
#'     \left( 1 + \operatorname{ARIV} \right)
#'     \boldsymbol{V}_{\mathrm{within}}
#'   }
#'   where \eqn{\operatorname{ARIV}} is given by
#'   \deqn{
#'     \operatorname{ARIV}
#'     =
#'     \frac{
#'       \left( 1 + M^{-1} \right)
#'       \operatorname{tr}
#'       \left(
#'          \boldsymbol{V}_{\mathrm{between}}
#'          \boldsymbol{V}_{\mathrm{within}}^{-1}
#'       \right)
#'     }{
#'       k
#'     }
#'   }
#'
#' @param coefs List.
#'   Each element is a vector of paramater estimates.
#' @param vcovs List.
#'   Each element is a matrix of sampling covariances.
#' @param M Positive integer.
#'   Number of imputations.
#' @param adj Logical.
#'   If `adj = TRUE`,
#'   use Li, Raghunathan, and Rubin (1991) adjustment.
#'
#' @return Returns a list with the following elements:
#'   \describe{
#'     \item{`M`}{
#'       Number of imputations
#'       \eqn{M}.
#'     }
#'     \item{`est`}{
#'       Vector of pooled coefficients/parameter estimates
#'       \eqn{\bar{\boldsymbol{\theta}}}.
#'     }
#'     \item{`within`}{
#'       Covariance within imputations
#'       \eqn{\boldsymbol{V}_{\mathrm{within}}}.
#'     }
#'     \item{`between`}{
#'       Covariance between imputations
#'       \eqn{\boldsymbol{V}_{\mathrm{between}}}.
#'     }
#'     \item{`total`}{
#'       Total covariance matrix
#'       \eqn{\boldsymbol{V}_{\mathrm{total}}}.
#'     }
#'   }
#'
#' @references
#'   Li, K. H., Raghunathan, T. E., & Rubin, D. B. (1991).
#'   Large-sample significance levels from multiply imputed data
#'   using moment-based statistics and an F reference distribution.
#'   *Journal of the American Statistical Association*,
#'   86 (416), 1065--1073.
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
.MICombine <- function(coefs,
                       vcovs,
                       M,
                       k,
                       adj = FALSE) {
  est <- colMeans(
    x = do.call(
      what = "rbind",
      args = coefs
    )
  )
  within <- (
    1 / M
  ) * Reduce(
    f = `+`,
    x = vcovs
  )
  between <- (
    1 / (
      M - 1
    )
  ) * Reduce(
    f = `+`,
    x = lapply(
      X = coefs,
      FUN = function(i,
                     est) {
        tcrossprod(x = i - est)
      },
      est = est
    )
  )
  colnames(x = between) <- rownames(x = between) <- rownames(x = within)
  total <- within + between + (1 / M) * between
  if (adj) {
    ariv <- .ARIV(
      between = between,
      within = within,
      M = M,
      k = length(x = est)
    )
    total_adj <- .TotalAdj(
      ariv = ariv,
      within = within
    )
  } else {
    ariv <- NA
    total_adj <- NA
  }
  list(
    M = M,
    est = est,
    within = within,
    between = between,
    total = total,
    ariv = ariv,
    total_adj = total_adj
  )
}
