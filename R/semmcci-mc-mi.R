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
#' @param adj Logical.
#'   If `adj = TRUE`,
#'   use Li, Raghunathan, and Rubin (1991)
#'   sampling covariance matrix adjustment.
#'   If `adj = FALSE`,
#'   use the multivariate version of Rubin's (1987)
#'   sampling covariance matrix.
#' @param seed_mc Integer.
#'   Random seed for the Monte Carlo method.
#' @param seed_mi Integer.
#'   Random seed for multiple imputation.
#' @param imp Optional argument.
#'   A list of multiply imputed data sets.
#' @param ... Additional arguments to pass to `mice::mice()`.
#'   DO NOT supply `data`, `seed`, or `print`.
#'
#' @inherit MC return
#'
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Data with Missing Values -------------------------------------------------
#' data("Tal.Or", package = "psych")
#' df <- mice::ampute(Tal.Or)$amp
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
#' # Fit the model in lavaan using the default listwise deletion method.
#' fit <- sem(data = df, model = model)
#'
#' # Monte Carlo --------------------------------------------------------------
#' MCMI(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = 0.05,
#'   m = 5 # use a large value e.g., 100L for actual research
#' )
#'
#' @references
#' Li, K. H., Raghunathan, T. E., & Rubin, D. B. (1991).
#' Large-sample significance levels from multiply imputed data
#' using moment-based statistics and an F reference distribution.
#' Journal of the American Statistical Association, 86 (416), 1065–1073.
#' \doi{10.1080/01621459.1991.10475152}
#'
#' Pesigan, I. J. A., & Cheung, S. F. (2023).
#' Monte Carlo confidence intervals for the indirect effect with missing data.
#' *Behavior Research Methods*.
#' \doi{10.3758/s13428-023-02114-4}
#'
#' Rubin, D. B. (1987).
#' *Multiple imputation for nonresponse in surveys*.
#' John Wiley & Sons, Inc.
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
#' @keywords semmcci mc
#' @export
MCMI <- function(object,
                 R = 20000L,
                 alpha = c(0.001, 0.01, 0.05),
                 decomposition = "eigen",
                 pd = TRUE,
                 tol = 1e-06,
                 adj = FALSE,
                 seed_mc = NULL,
                 seed_mi = NA,
                 imp = NULL,
                 ...) {
  stopifnot(
    inherits(
      object,
      "lavaan"
    )
  )
  if (object@Data@data.type == "moment") {
    stop(
      "The \'data\' argument is required by \'MCMI()\'."
    )
  }
  call0 <- stats::getCall(object)
  call1 <- call0
  for (i in seq_along(call0)) {
    call1[[i]] <- eval(
      expr = call0[[i]],
      envir = parent.frame()
    )
  }
  data0 <- eval(
    call0$data,
    envir = parent.frame()
  )
  if (!is.null(imp)) {
    stopifnot(
      inherits(
        imp,
        "list"
      )
    )
    mi <- imp
  } else {
    mi <- mice::complete(
      mice::mice(
        data = data0,
        print = FALSE,
        seed = seed_mi,
        ...
      ),
      action = "all"
    )
  }
  fits <- lapply(
    X = mi,
    FUN = function(x) {
      call1$data <- x
      return(
        eval(expr = call1)
      )
    }
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
    M = length(coefs),
    k = length(coefs[[1]]),
    adj = adj
  )
  if (adj) {
    scale <- pooled$total_adj
  } else {
    scale <- pooled$total
  }
  location <- pooled$est
  # mc
  if (!is.null(decomposition)) {
    if (decomposition == "chol") {
      pd <- FALSE
    }
  }
  ## set up Monte Carlo
  set.seed(seed_mc)
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
    object = object,
    est = colMeans(
      do.call(
        what = "rbind",
        args = lapply(
          X = fits,
          FUN = function(object) {
            return(
              object@ParTable$est
            )
          }
        )
      )
    )
  )
  # defined parameters
  thetahatstar <- .MCDef(
    object = object,
    thetahat = thetahat,
    thetahatstar_orig = thetahatstar_orig
  )
  # output
  out <- list(
    R = R,
    alpha = alpha,
    lavaan = object,
    decomposition = decomposition,
    thetahat = thetahat,
    thetahatstar = thetahatstar,
    mi = mi
  )
  class(out) <- c(
    "semmcci",
    class(out)
  )
  return(out)
}
