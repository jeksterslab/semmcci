#' Monte Carlo Confidence Intervals (Generic)
#'
#' Calculates Monte Carlo confidence intervals
#' for defined parameters
#' for any fitted model object with `coef` and `vcov` methods.
#'
#' A sampling distribution of parameter estimates is generated
#' from the multivariate normal distribution
#' using the parameter estimates and the sampling variance-covariance matrix.
#' Confidence intervals for defined parameters
#' are generated using the simulated sampling distribution.
#' Parameters are defined using the `def` argument.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object R object.
#'   Fitted model object with `coef` and `vcov` methods
#'   that return a named vector of
#'   estimated parameters and sampling variance-covariance matrix,
#'   respectively.
#' @param def List of character strings.
#'   A list of defined functions of parameters.
#'   The string should be a valid R expression when parsed
#'   and should result a single value when evaluated.
#' @inheritParams MC
#'
#' @return Returns an object of class `semmcci` which is
#'   a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{List of function arguments.}
#'     \item{thetahat}{Parameter estimates \eqn{\hat{\theta}}.}
#'     \item{thetahatstar}{Sampling distribution of parameter estimates
#'                         \eqn{\hat{\theta}^{\ast}}.}
#'     \item{fun}{Function used ("MCGeneric").}
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
#' "
#' fit <- sem(data = df, model = model, missing = "fiml")
#'
#' ## MCGeneric() -------------------------------------------------------------
#' MCGeneric(
#'   fit,
#'   R = 5L, # use a large value e.g., 20000L for actual research
#'   alpha = 0.05,
#'   def = list(
#'     "a * b",
#'     "cp + (a * b)"
#'   )
#' )
#'
#' @references
#' MacKinnon, D. P., Lockwood, C. M., & Williams, J. (2004).
#' Confidence limits for the indirect effect:
#' Distribution of the product and resampling methods.
#' *Multivariate Behavioral Research*, *39*(1), 99-128.
#' \doi{10.1207/s15327906mbr3901_4}
#'
#' Pesigan, I. J. A., & Cheung, S. F. (2023).
#' Monte Carlo confidence intervals for the indirect effect with missing data.
#' *Behavior Research Methods*.
#' \doi{10.3758/s13428-023-02114-4}
#'
#' Preacher, K. J., & Selig, J. P. (2012).
#' Advantages of Monte Carlo confidence intervals for indirect effects.
#' *Communication Methods and Measures*, *6*(2), 77–98.
#' \doi{10.1080/19312458.2012.679848}
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
#' @keywords semmcci mc
#' @export
MCGeneric <- function(object,
                      def,
                      R = 20000L,
                      alpha = c(0.001, 0.01, 0.05),
                      decomposition = "eigen",
                      pd = TRUE,
                      tol = 1e-06,
                      seed = NULL) {
  args <- list(
    lav = NA,
    R = R,
    alpha = alpha,
    decomposition = decomposition,
    pd = pd,
    tol = tol,
    seed = seed,
    object = object,
    def = def
  )
  # mc
  ## identify coefficients used and do mc only for them
  defs_exp <- lapply(
    X = def,
    FUN = function(x) {
      parse(text = x)
    }
  )
  def_vars <- unique(
    unlist(
      sapply(
        X = defs_exp, FUN = all.vars
      )
    )
  )
  ## def to be used as names
  def_vec <- def
  dim(def_vec) <- NULL
  ## set up Monte Carlo
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (!is.null(decomposition)) {
    if (decomposition == "chol") {
      pd <- FALSE
    }
  }
  if (
    inherits(
      object,
      "lavaan"
    )
  ) {
    location <- lavaan::coef(object)[def_vars]
    scale <- lavaan::vcov(object)[def_vars, def_vars]
  } else {
    location <- stats::coef(object)[def_vars]
    scale <- stats::vcov(object)[def_vars, def_vars]
  }
  thetahatstar <- .ThetaHatStar(
    R = R,
    scale = scale,
    location = location,
    decomposition = decomposition,
    pd = pd,
    tol = tol
  )
  ## defined parameters
  ### single replication
  thetahatstar_orig <- thetahatstar$thetahatstar
  var_names <- colnames(thetahatstar_orig)
  foo <- function(def,
                  thetahat) {
    names(thetahat) <- var_names
    env <- list2env(
      as.list(thetahat)
    )
    return(
      eval(
        parse(text = def),
        envir = env
      )
    )
  }
  ### R replications for 1 def
  bar <- function(def) {
    thetahatstar_dist <- rep(x = NA, times = R)
    for (i in seq_len(R)) {
      thetahatstar_i <- thetahatstar_orig[i, ]
      thetahatstar_dist[i] <- foo(
        def = def,
        thetahat = thetahatstar_i
      )
    }
    return(thetahatstar_dist)
  }
  ### R replications for all def
  thetahatstar <- do.call(
    what = "cbind",
    args = lapply(
      X = def,
      FUN = bar
    )
  )
  colnames(thetahatstar) <- def_vec
  ## estimates
  thetahat <- rep(x = NA, times = length(def))
  for (i in seq_along(thetahat)) {
    thetahat[i] <- foo(
      def = def[[i]],
      thetahat = location
    )
  }
  names(thetahat) <- def_vec
  # output
  out <- list(
    call = match.call(),
    args = args,
    thetahat = list(
      est = thetahat
    ),
    thetahatstar = thetahatstar,
    fun = "MCGeneric"
  )
  class(out) <- c(
    "semmcci",
    class(out)
  )
  return(out)
}
