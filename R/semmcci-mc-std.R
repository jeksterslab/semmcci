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
#'   Output of the [MC()] function.
#' @return Returns an object of class `semmccistd` which is
#'   a list with the following elements:
#'   \describe{
#'     \item{`R`}{Number of Monte Carlo replications.}
#'     \item{`alpha`}{Significance level \eqn{\alpha} specified.}
#'     \item{`lavaan`}{`lavaan` object.}
#'     \item{`decomposition`}{Matrix decomposition
#'                            used to generate multivariate normal
#'                            random variates.}
#'     \item{`thetahat`}{Parameter estimates \eqn{\hat{\theta}}.}
#'     \item{`thetahatstar`}{Sampling distribution of parameter estimates
#'                           \eqn{\hat{\theta}^{\ast}}.}
#'     \item{`ci`}{Confidence intervals.}
#'     \item{`thetahat_std`}{Standardized parameter estimates
#'                           \eqn{\hat{\theta}_{\mathrm{std}}}.}
#'     \item{`thetahatstar_std`}{Standardized sampling distribution
#'                               of parameter estimates
#'                             \eqn{\hat{\theta}^{\ast}_{\mathrm{std}}}.}
#'   }
#'
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # MC() ---------------------------------------------------------------------
#' # Data ---------------------------------------------------------------------
#' data("Tal.Or", package = "psych")
#' df <- Tal.Or
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   reaction ~ cp * cond + b * pmi
#'   pmi ~ a * cond
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#'
#' fit_complete_data <- sem(data = df, model = model, fixed.x = FALSE)
#'
#' # Monte Carlo --------------------------------------------------------------
#' complete_data <- MC(
#'   fit_complete_data,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = 0.05
#' )
#'
#' # Standardized Monte Carlo -------------------------------------------------
#' MCStd(complete_data, alpha = 0.05)
#'
#' # MCMI() -------------------------------------------------------------------
#' # Data ---------------------------------------------------------------------
#' df <- mice::ampute(Tal.Or)$amp
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' fit_missing_data <- sem(data = df, model = model, fixed.x = FALSE)
#'
#' # Monte Carlo --------------------------------------------------------------
#' missing_data <- MC(
#'   fit_missing_data,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = 0.05
#' )
#'
#' # Standardized Monte Carlo -------------------------------------------------
#' MCStd(missing_data, alpha = 0.05)
#'
#' @references
#' Pesigan, I. J. A., & Cheung, S. F. (2023).
#' Monte Carlo confidence intervals for the indirect effect with missing data.
#' *Behavior Research Methods*.
#' \doi{10.3758/s13428-023-02114-4}
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
#' @keywords semmcci mc
#' @export
MCStd <- function(object,
                  alpha = c(0.001, 0.01, 0.05)) {
  stopifnot(
    inherits(
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
    )[
      ,
      "est.std"
    ]
  )
  names(
    thetahat_std
  ) <- colnames(
    object$thetahatstar
  )
  i_free <- object$lavaan@ParTable$free > 0
  foo <- function(i,
                  p) {
    tryCatch(
      {
        return(
          .StdLav(
            est = object$thetahatstar[
              i,
              i_free
            ],
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
    X = seq_len(
      dim(
        object$thetahatstar
      )[1]
    ),
    FUN = foo,
    p = length(
      thetahat_std
    )
  )
  thetahatstar_std <- do.call(
    what = "rbind",
    args = thetahatstar_std
  )
  colnames(
    thetahatstar_std
  ) <- colnames(
    object$thetahatstar
  )
  out <- list(
    R = object$R,
    alpha = object$alpha,
    lavaan = object$lavaan,
    decomposition = object$decomposition,
    thetahat = object$thetahat,
    thetahatstar = object$thetahatstar,
    thetahat_std = thetahat_std,
    thetahatstar_std = thetahatstar_std
  )
  class(out) <- c(
    "semmccistd",
    class(out)
  )
  return(out)
}
