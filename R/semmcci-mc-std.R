#' Standardized Monte Carlo Confidence Intervals
#'
#' Calculates standardized Monte Carlo confidence intervals
#' for free and defined parameters.
#'
#' The empirical sampling distribution
#' of parameter estimates from the argument `object` is standardized,
#' that is, each randomly generated vector of parameters is standardized.
#' Confidence intervals are generated
#' using the standardized empirical sampling distribution.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams MC
#' @param object object of class `semmcci`.
#'   Output of the `MC()` function.
#' @return Returns an object of class `semmcci_std`
#' which is a list with the following elements:
#' \itemize{
#'   \item{`lavaan`}{`lavaan` object.}
#'   \item{`mu`}{Mean vector used in the Monte Carlo simulation.}
#'   \item{`Sigma`}{Variance-covariance matrix used in the Monte Carlo simulation.}
#'   \item{`thetahat`}{Parameter estimates.}
#'   \item{`thetahatstar`}{Sampling distribution of parameter estimates.}
#'   \item{`ci`}{Confidence intervals.}
#'   \item{`thetahat.std`}{Standardized parameter estimates.}
#'   \item{`thetahatstar.std`}{Standardized sampling distribution of parameter estimates.}
#'   \item{`ci.std`}{Standardized confidence intervals.}
#' }
#' The list element `ci.std` is a matrix with the following columns:
#' \itemize{
#'   \item{`est`}{Standardized parameter estimates.}
#'   \item{`se`}{Standard errors or the square root of the diagonals of the standardized Monte Carlo sampling distribution of parameter estimates.}
#'   \item{`R`}{Number of Monte Carlo replications.}
#'   \item{...}{Percentiles that correspond to the confidence intervals defined by `alpha`.}
#' }
#' Note that the rows in `ci.std` correspond to the standardized model parameters.
#' Parameters with zero standard errors and constant confidence limits are fixed parameters.
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Generate Data ------------------------------------------------------------
#' n <- 1000
#' x <- rnorm(n = n)
#' m <- 0.50 * x + rnorm(n = n)
#' y <- 0.25 * x + 0.50 * m + rnorm(n = n)
#' data <- data.frame(x, m, y)
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   y ~ cp * x + b * m
#'   m ~ a * x
#'   ab := a * b
#' "
#' fit <- sem(data = data, model = model, fixed.x = FALSE)
#'
#' # Monte Carlo --------------------------------------------------------------
#' output <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = c(0.001, 0.01, 0.05)
#' )
#'
#' # Standardized Monte Carlo -------------------------------------------------
#' MCStd(output)
#' @importFrom lavaan standardizedSolution
#' @keywords mc
#' @export
MCStd <- function(object,
                  alpha = c(0.001, 0.01, 0.05)) {
  stopifnot(
    methods::is(
      object,
      "semmcci"
    )
  )
  # if (object$lavaan@pta$ngroups > 1) {
  #   stop("Multiple groups analysis is not yet supported.")
  # }
  thetahat <- as.vector(
    lavaan::standardizedSolution(
      object = object$lavaan,
      type = "std.all",
      se = FALSE,
      zstat = FALSE,
      pvalue = FALSE,
      ci = FALSE,
      remove.eq = FALSE,
      remove.ineq = FALSE,
      remove.def = FALSE
    )[, "est.std"]
  )
  names(thetahat) <- colnames(object$thetahatstar)
  i_free <- lavaan::parameterTable(object$lavaan)$free > 0
  foo <- function(i) {
    return(
      .StdLav(
        est = object$thetahatstar[i, i_free],
        object = object$lavaan
      )
    )
  }
  thetahatstar <- lapply(
    X = seq_len(dim(object$thetahatstar)[1]),
    FUN = foo
  )
  thetahatstar <- do.call(
    what = "rbind",
    args = thetahatstar
  )
  colnames(thetahatstar) <- colnames(object$thetahatstar)
  se <- sqrt(diag(stats::var(thetahatstar)))
  ci <- vector(
    mode = "list",
    length = dim(thetahatstar)[2]
  )
  for (i in seq_len(dim(thetahatstar)[2])) {
    ci[[i]] <- .PCCI(
      thetahatstar = thetahatstar[, i],
      thetahat = thetahat[[i]],
      alpha = alpha
    )
  }
  ci <- do.call(
    what = "rbind",
    args = ci
  )
  rownames(ci) <- colnames(thetahatstar)
  ci <- ci[which(object$thetahat$op != "~1"), ]
  ci <- ci[which(!rownames(ci) %in% object$thetahat$fixed), ]
  out <- list(
    lavaan = object$lavaan,
    mu = object$mu,
    Sigma = object$Sigma,
    thetahat = object$thetahat,
    thetahatstar = object$thetahatstar,
    ci = object$ci,
    thetahat.std = thetahat,
    thetahatstar.std = thetahatstar,
    ci.std = ci
  )
  class(out) <- c(
    "semmcci_std",
    class(out)
  )
  out
}
