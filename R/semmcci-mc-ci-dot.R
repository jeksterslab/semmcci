#' Confidence Intervals
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object object of class `semmcci`.
#' @param alpha Numeric vector.
#'   Significance level.
#'   If `alpha = NULL`,
#'   extract `alpha` from `semmcci`.
#' @return Returns a matrix with the following columns:
#' \describe{
#'   \item{`est`}{Parameter estimates.}
#'   \item{`se`}{Standard errors or the square root
#'               of the diagonals of the Monte Carlo sampling distribution
#'               of parameter estimates.}
#'   \item{`R`}{Number of valid Monte Carlo replications.}
#'   \item{...}{Percentiles that correspond to the confidence intervals
#'              defined by `alpha`.}
#' }
#' Note that the rows in `ci` correspond to the model parameters.
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
#' @keywords semmcci mc internal
#' @noRd
.MCCI <- function(object,
                  alpha = NULL) {
  stopifnot(
    inherits(
      object,
      "semmcci"
    )
  )
  thetahatstar <- object$thetahatstar
  thetahat <- object$thetahat
  if (is.null(alpha)) {
    alpha <- object$args$alpha
  }
  stopifnot(
    all(alpha > 0 & alpha < 1)
  )
  probs <- .PCProbs(alpha = alpha)
  ci <- vector(
    mode = "list",
    length = dim(thetahatstar)[2]
  )
  for (i in seq_len(dim(thetahatstar)[2])) {
    ci[[i]] <- .PCCI(
      thetahatstar = thetahatstar[, i],
      thetahat = thetahat$est[[i]],
      probs = probs
    )
  }
  ci <- do.call(
    what = "rbind",
    args = ci
  )
  varnames <- colnames(thetahatstar)
  if (!is.null(varnames)) {
    rownames(ci) <- varnames
  }
  return(ci)
}
