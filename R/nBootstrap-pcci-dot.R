#' Generate Percentile Confidence Intervals
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param thetahatstar Numeric vector.
#'   Sampling distribution.
#' @param thetahat Numeric.
#'   Parameter estimate.
#' @param alpha Numeric vector.
#'   Significance level.
#'   Default value is `alpha = c(0.001, 0.01, 0.05)`.
#' @return Returns a matrix of estimates, standard errors,
#'   number of replications, and confidence intervals.
#'
#' @family Confidence Intervals Functions
#' @keywords nBootstrap ci internal
#' @noRd
.PCCI <- function(thetahatstar,
                  thetahat,
                  alpha = c(0.001, 0.01, 0.05)) {
  thetahatstar <- as.vector(thetahatstar)
  thetahatstar <- thetahatstar[stats::complete.cases(thetahatstar)]
  alpha <- sort(alpha)
  prob_ll <- alpha / 2
  prob_ul <- rev(1 - prob_ll)
  probs <- c(prob_ll, prob_ul)
  ci <- stats::quantile(
    x = thetahatstar,
    probs = probs
  )
  ci <- c(
    thetahat,
    stats::sd(thetahatstar),
    length(thetahatstar),
    ci
  )
  names(ci) <- c(
    "est",
    "se",
    "R",
    paste0(
      probs * 100,
      "%"
    )
  )
  return(ci)
}
