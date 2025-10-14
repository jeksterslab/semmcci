#' Standardized Monte Carlo Confidence Intervals
#'
#' Calculates standardized Monte Carlo confidence intervals
#' for free and defined parameters.
#'
#' The empirical sampling distribution
#' of parameter estimates from the argument `mc` is standardized,
#' that is, each randomly generated vector of parameters is standardized.
#' Defined parameters are computed from the standardized component parameters.
#' Confidence intervals are generated
#' using the standardized empirical sampling distribution.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams MC
#' @param mc Output of the [MC()] or [MCMI()] function.
#'
#' @return Returns an object of class `semmcci` which is
#'   a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{List of function arguments.}
#'     \item{thetahat}{Parameter estimates \eqn{\hat{\theta}}.}
#'     \item{thetahatstar}{Sampling distribution of parameter estimates
#'                         \eqn{\hat{\theta}^{\ast}}.}
#'     \item{fun}{Function used ("MCStd").}
#'   }
#'
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Data ---------------------------------------------------------------------
#' data("Tal.Or", package = "psych")
#' df <- mice::ampute(Tal.Or)$amp
#'
#' # Monte Carlo --------------------------------------------------------------
#' ## Fit Model in lavaan -----------------------------------------------------
#' model <- "
#'   reaction ~ cp * cond + b * pmi
#'   pmi ~ a * cond
#'   cond ~~ cond
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#' fit <- sem(data = df, model = model, missing = "fiml")
#'
#' ## MC() --------------------------------------------------------------------
#' unstd <- MC(
#'   fit,
#'   R = 5L, # use a large value e.g., 20000L for actual research
#'   alpha = 0.05
#' )
#'
#' ## Standardized Monte Carlo ------------------------------------------------
#' MCStd(unstd, alpha = 0.05)
#'
#' # Monte Carlo (Multiple Imputation) ----------------------------------------
#' ## Multiple Imputation -----------------------------------------------------
#' mi <- mice::mice(
#'   data = df,
#'   print = FALSE,
#'   m = 5L, # use a large value e.g., 100L for actual research,
#'   seed = 42
#' )
#'
#' ## Fit Model in lavaan -----------------------------------------------------
#' fit <- sem(data = df, model = model) # use default listwise deletion
#'
#' ## MCMI() ------------------------------------------------------------------
#' unstd <- MCMI(
#'   fit,
#'   mi = mi,
#'   R = 5L, # use a large value e.g., 20000L for actual research
#'   alpha = 0.05
#' )
#'
#' ## Standardized Monte Carlo ------------------------------------------------
#' MCStd(unstd, alpha = 0.05)
#'
#' @references
#' Pesigan, I. J. A., & Cheung, S. F. (2024).
#' Monte Carlo confidence intervals for the indirect effect with missing data.
#' *Behavior Research Methods*.
#' \doi{10.3758/s13428-023-02114-4}
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
#' @keywords semmcci mc
#' @export
MCStd <- function(mc,
                  alpha = c(0.001, 0.01, 0.05)) {
  stopifnot(
    inherits(
      x = mc,
      what = "semmcci"
    )
  )
  stopifnot(
    mc$fun %in% c("MC", "MCMI")
  )
  args <- list(
    mc = mc,
    alpha = alpha
  )
  thetahat_std <- as.vector(
    lavaan::standardizedSolution(
      object = mc$args$lav,
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
    mc$thetahatstar
  )
  i_free <- mc$args$lav@ParTable$free > 0
  foo <- function(i,
                  p) {
    out <- tryCatch(
      {
        .StdLav(
          est = mc$thetahatstar[
            i,
            i_free
          ],
          object = mc$args$lav
        )
      },
      warning = function(w) {
        rep(
          x = NA,
          times = p
        )
      },
      error = function(e) {
        rep(
          x = NA,
          times = p
        )
      }
    )
    out
  }
  thetahatstar_std <- lapply(
    X = seq_len(
      dim(
        mc$thetahatstar
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
    mc$thetahatstar
  )
  # remove mean structure
  thetahatstar_std <- thetahatstar_std[
    ,
    which(mc$args$lav@ParTable$op != "~1"),
    drop = FALSE
  ]
  out <- list(
    call = match.call(),
    args = args,
    thetahat = .ThetaHat(
      object = mc$args$lav,
      est = thetahat_std
    ),
    thetahatstar = thetahatstar_std,
    fun = "MCStd"
  )
  class(out) <- c(
    "semmcci",
    class(out)
  )
  out
}
