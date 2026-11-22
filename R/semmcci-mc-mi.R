#' Monte Carlo Confidence Intervals (Multiple Imputation)
#'
#' Calculates Monte Carlo confidence intervals
#' for free and defined parameters.
#' Missing values are handled using multilple imputation.
#'
#' A sampling distribution of parameter estimates is generated
#' from the multivariate normal distribution
#' using the parameter estimates and the sampling variance-covariance matrix
#' obtained using multiple imputation.
#' Confidence intervals for free and defined parameters
#' are generated using the simulated sampling distribution.
#' Parameters can be defined using the `:=` operator
#' in the `lavaan` model syntax.
#'
#' @inheritParams MC
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @details
#' Let \eqn{\hat{\boldsymbol{\theta}}_{m}} and
#' \eqn{\hat{\boldsymbol{V}}_{m}} be the estimates and their
#' sampling covariance matrix in imputed data set \eqn{m = 1, \dots, M}.
#' The pooled estimates and covariance matrices (Rubin's rules) are
#' \deqn{\bar{\boldsymbol{\theta}} = \frac{1}{M} \sum_{m = 1}^{M}
#' \hat{\boldsymbol{\theta}}_{m}, \quad \bar{\boldsymbol{W}} = \frac{1}{M}
#' \sum_{m = 1}^{M} \hat{\boldsymbol{V}}_{m}, \quad \boldsymbol{B} =
#' \frac{1}{M - 1} \sum_{m = 1}^{M} \left( \hat{\boldsymbol{\theta}}_{m} -
#' \bar{\boldsymbol{\theta}} \right) \left( \hat{\boldsymbol{\theta}}_{m} -
#' \bar{\boldsymbol{\theta}} \right)^{\prime},}
#' \deqn{\boldsymbol{T} = \bar{\boldsymbol{W}} + \left( 1 + \frac{1}{M}
#' \right) \boldsymbol{B}.}
#' The draws are generated as in [MC()],
#' with \eqn{\bar{\boldsymbol{\theta}}} and \eqn{\boldsymbol{T}}
#' in place of \eqn{\hat{\boldsymbol{\theta}}} and
#' \eqn{\hat{\boldsymbol{V}}}.
#' A defined parameter \eqn{\phi = h \left( \boldsymbol{\theta} \right)},
#' given by the `:=` operator in the model syntax,
#' has the draws
#' \eqn{\hat{\phi}^{\ast}_{r} = h \left( \hat{\boldsymbol{\theta}}^{\ast}_{r}
#' \right)}.
#' The confidence intervals are the percentile intervals
#' \eqn{\left[ \hat{G}^{-1} \left( \alpha / 2 \right), \hat{G}^{-1} \left( 1 -
#' \alpha / 2 \right) \right]},
#' where \eqn{\hat{G}} is the empirical distribution function of the draws
#' (Preacher & Selig, 2012).
#'
#' @param mi Object of class `mids` (output of [mice::mice()]),
#'   object of class `amelia` (output of [Amelia::amelia()]),
#'   or a list of multiply imputed data sets.
#'
#' @return Returns an object of class `semmcci` which is
#'   a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{List of function arguments.}
#'     \item{thetahat}{Parameter estimates \eqn{\hat{\theta}}.}
#'     \item{thetahatstar}{Sampling distribution of parameter estimates
#'                         \eqn{\hat{\theta}^{\ast}}.}
#'     \item{fun}{Function used ("MCMI").}
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
#' model <- "
#'   reaction ~ cp * cond + b * pmi
#'   pmi ~ a * cond
#'   cond ~~ cond
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#' fit <- sem(data = df, model = model) # use default listwise deletion
#'
#' ## MCMI() ------------------------------------------------------------------
#' MCMI(
#'   fit,
#'   mi = mi,
#'   R = 5L, # use a large value e.g., 20000L for actual research
#'   alpha = 0.05
#' )
#'
#' @references
#' Pesigan, I. J. A., & Cheung, S. F. (2024).
#' Monte Carlo confidence intervals for the indirect effect with missing data.
#' *Behavior Research Methods*.
#' \doi{10.3758/s13428-023-02114-4}
#'
#' Rubin, D. B. (1987).
#' *Multiple imputation for nonresponse in surveys*.
#' John Wiley & Sons, Inc.
#'
#' Preacher, K. J., & Selig, J. P. (2012).
#' Advantages of Monte Carlo confidence intervals for indirect effects.
#' *Communication Methods and Measures*, *6*(2), 77--98.
#' \doi{10.1080/19312458.2012.679848}
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
#' @keywords semmcci mc
#' @export
MCMI <- function(lav,
                 mi,
                 R = 20000L,
                 alpha = c(0.001, 0.01, 0.05),
                 decomposition = "eigen",
                 pd = TRUE,
                 tol = 1e-06,
                 seed = NULL) {
  stopifnot(
    inherits(
      x = lav,
      what = "lavaan"
    )
  )
  if (lav@Data@data.type == "moment") {
    stop(
      "The \'data\' argument in \'lavaan\' is required by \'MCMI()\'."
    )
  }
  call0 <- stats::getCall(x = lav)
  call1 <- call0
  for (i in seq_along(along.with = call0)) {
    call1[[i]] <- eval(
      expr = call0[[i]],
      envir = parent.frame()
    )
  }
  if (
    inherits(
      x = mi,
      what = "mids"
    )
  ) {
    imp <- mice::complete(
      data = mi,
      action = "all"
    )
  } else if (
    inherits(
      x = mi,
      what = "amelia"
    )
  ) {
    imp <- mi$imputations
  } else if (
    inherits(
      x = mi,
      what = "list"
    )
  ) {
    imp <- mi
  } else {
    stop("Invalid \'mi\' argument.")
  }
  fits <- lapply(
    X = imp,
    FUN = function(x,
                   call1) {
      call1$data <- x
      eval(expr = call1)
    },
    call1 = call1
  )
  coefs <- lapply(
    X = fits,
    FUN = lavaan::coef
  )
  vcovs <- lapply(
    X = fits,
    FUN = lavaan::vcov
  )
  pooled <- .MICombine(
    coefs = coefs,
    vcovs = vcovs,
    M = length(x = coefs),
    k = length(x = coefs[[1]]),
    adj = TRUE
  )
  scale <- pooled$total
  location <- pooled$est
  # mc
  if (!is.null(x = decomposition)) {
    if (decomposition == "chol") {
      pd <- FALSE
    }
  }
  ## set up Monte Carlo
  if (!is.null(x = seed)) {
    set.seed(seed = seed)
  }
  thetahatstar <- .ThetaHatStar(
    R = R,
    scale = scale,
    location = location,
    decomposition = decomposition,
    pd = pd,
    tol = tol
  )
  thetahatstar_orig <- thetahatstar$thetahatstar
  decomposition <- thetahatstar$decomposition
  ## extract all estimates including fixed parameters
  ## update est using pooled estimates
  thetahat <- .ThetaHat(
    object = lav,
    est = colMeans(
      x = do.call(
        what = "rbind",
        args = lapply(
          X = fits,
          FUN = function(lav) {
            lav@ParTable$est
          }
        )
      )
    )
  )
  # defined parameters
  thetahatstar <- .MCDef(
    object = lav,
    thetahat = thetahat,
    thetahatstar_orig = thetahatstar_orig
  )
  # output
  out <- list(
    call = match.call(),
    args = list(
      lav = lav,
      mi = mi,
      R = R,
      alpha = alpha,
      decomposition = decomposition,
      pd = pd,
      tol = tol,
      seed = seed,
      imp = imp,
      pooled = pooled
    ),
    thetahat = thetahat,
    thetahatstar = thetahatstar,
    fun = "MCMI"
  )
  class(x = out) <- c(
    "semmcci",
    class(x = out)
  )
  out
}
