#' Standardized Monte Carlo Confidence Intervals
#'
#' Calculates standardized Monte Carlo confidence intervals
#' for free and defined parameters.
#'
#' The empirical sampling distribution
#' of parameter estimates from the argument `object` is standardized,
#' that is, each randomly generated vector of parameters is standardized.
#' Defined parameters are computed from the standardized component parameters.
#' Confidence intervals are generated
#' using the standardized empirical sampling distribution.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams MC
#' @param object object of class `semmcci`.
#'   Output of the `MC()` function.
#' @return Returns an object of class `semmccistd`
#' which is a list with the following elements:
#' \describe{
#'   \item{`R`}{Number of Monte Carlo replications.}
#'   \item{`alpha`}{Significance level specified.}
#'   \item{`lavaan`}{`lavaan` object.}
#'   \item{`mvn`}{Method used to generate multivariate normal random variates.}
#'   \item{`thetahat`}{Parameter estimates.}
#'   \item{`thetahatstar`}{Sampling distribution of parameter estimates.}
#'   \item{`ci`}{Confidence intervals.}
#'   \item{`thetahat_std`}{Standardized parameter estimates.}
#'   \item{`thetahatstar_std`}{Standardized sampling distribution of parameter estimates.}
#' }
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
  thetahat_std <- as.vector(
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
  names(thetahat_std) <- colnames(object$thetahatstar)
  i_free <- object$lavaan@ParTable$free > 0
  foo <- function(i, p) {
    tryCatch(
      {
        return(
          .StdLav(
            est = object$thetahatstar[i, i_free],
            object = object$lavaan
          )
        )
      },
      warning = function(w) {
        return(
          rep(
            x = NA,
            times = p
          )
        )
      },
      error = function(e) {
        return(
          rep(
            x = NA,
            times = p
          )
        )
      }
    )
  }
  thetahatstar_std <- lapply(
    X = seq_len(dim(object$thetahatstar)[1]),
    FUN = foo,
    p = length(thetahat_std)
  )
  thetahatstar_std <- do.call(
    what = "rbind",
    args = thetahatstar_std
  )
  colnames(thetahatstar_std) <- colnames(object$thetahatstar)
  # remove rows with NAs
  # thetahatstar_std <- thetahatstar_std[stats::complete.cases(thetahatstar_std), ]
  #   se <- sqrt(diag(stats::var(thetahatstar_std)))
  #   ci_std <- vector(
  #     mode = "list",
  #     length = dim(thetahatstar_std)[2]
  #   )
  #   for (i in seq_len(dim(thetahatstar_std)[2])) {
  #     ci_std[[i]] <- .PCCI(
  #       thetahatstar = thetahatstar_std[, i],
  #       thetahat = thetahat_std[[i]],
  #       alpha = alpha
  #     )
  #   }
  #   ci_std <- do.call(
  #     what = "rbind",
  #     args = ci_std
  #   )
  #   rownames(ci_std) <- colnames(thetahatstar_std)
  #   ci_std <- ci_std[which(object$lavaan@ParTable$op != "~1"), ]
  #   ci_std <- ci_std[which(!rownames(ci_std) %in% object$thetahat$fixed), ]
  out <- list(
    R = object$R,
    alpha = object$alpha,
    lavaan = object$lavaan,
    mvn = object$mvn,
    thetahat = object$thetahat,
    thetahatstar = object$thetahatstar,
    ci = object$ci,
    thetahat_std = thetahat_std,
    thetahatstar_std = thetahatstar_std
    # ci_std = ci_std
  )
  class(out) <- c(
    "semmccistd",
    class(out)
  )
  return(out)
}
