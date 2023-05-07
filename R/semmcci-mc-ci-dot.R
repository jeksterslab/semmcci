#' Confidence Intervals
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object object of class `semmcci` or `semmccistd`.
#' @param alpha Numeric vector.
#'   Significance level.
#'   If `alpha = NULL`,
#'   extract `alpha` from `semmcci` or `semmccistd`.
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
  if (
    inherits(
      object,
      "semmcci"
    )
  ) {
    thetahatstar <- object$thetahatstar
    thetahat <- object$thetahat
    if (is.null(alpha)) {
      alpha <- object$alpha
    }
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
    rownames(ci) <- colnames(thetahatstar)
    ci <- ci[which(!rownames(ci) %in% thetahat$fixed), ]
    return(ci)
  }
  if (
    inherits(
      object,
      "semmccistd"
    )
  ) {
    thetahatstar_std <- object$thetahatstar_std
    thetahat_std <- object$thetahat_std
    if (is.null(alpha)) {
      alpha <- object$alpha
    }
    probs <- .PCProbs(alpha = alpha)
    ci_std <- vector(
      mode = "list",
      length = dim(thetahatstar_std)[2]
    )
    for (i in seq_len(dim(thetahatstar_std)[2])) {
      ci_std[[i]] <- .PCCI(
        thetahatstar = thetahatstar_std[, i],
        thetahat = thetahat_std[[i]],
        probs = probs
      )
    }
    ci_std <- do.call(
      what = "rbind",
      args = ci_std
    )
    rownames(ci_std) <- colnames(thetahatstar_std)
    ci_std <- ci_std[which(object$lavaan@ParTable$op != "~1"), ]
    ci_std <- ci_std[which(!rownames(ci_std) %in% object$thetahat$fixed), ]
    return(ci_std)
  }
}
