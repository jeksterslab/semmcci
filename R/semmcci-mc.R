#' Monte Carlo Confidence Intervals
#'
#' Calculates Monte Carlo confidence intervals
#' for free and defined parameters.
#'
#' A sampling distribution of parameter estimates is generated
#' from the multivariate normal distribution
#' using the parameter estimates and the sampling variance-covariance matrix.
#' Confidence intervals for free and defined parameters
#' are generated using the simulated sampling distribution.
#' Parameters can be defined using the `:=` operator
#' in the `lavaan` model syntax.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param lav Object of class `lavaan`.
#' @param R Positive integer.
#'   Number of Monte Carlo replications.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#' @param decomposition Character string.
#'   Matrix decomposition of the sampling variance-covariance matrix
#'   for the data generation.
#'   If `decomposition = "chol"`, use Cholesky decomposition.
#'   If `decomposition = "eigen"`, use eigenvalue decomposition.
#'   If `decomposition = "svd"`, use singular value decomposition.
#' @param pd Logical.
#'   If `pd = TRUE`,
#'   check if the sampling variance-covariance matrix
#'   is positive definite using `tol`.
#' @param tol Numeric.
#'   Tolerance used for `pd`.
#' @param seed Integer.
#'   Random seed for reproducibility.
#'
#' @return Returns an object of class `semmcci` which is
#'   a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{List of function arguments.}
#'     \item{thetahat}{Parameter estimates \eqn{\hat{\theta}}.}
#'     \item{thetahatstar}{Sampling distribution of parameter estimates
#'                         \eqn{\hat{\theta}^{\ast}}.}
#'     \item{fun}{Function used ("MC").}
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
#' MC(
#'   fit,
#'   R = 5L, # use a large value e.g., 20000L for actual research
#'   alpha = 0.05
#' )
#'
#' @references
#' MacKinnon, D. P., Lockwood, C. M., & Williams, J. (2004).
#' Confidence limits for the indirect effect:
#' Distribution of the product and resampling methods.
#' *Multivariate Behavioral Research*, *39*(1), 99-128.
#' \doi{10.1207/s15327906mbr3901_4}
#'
#' Pesigan, I. J. A., & Cheung, S. F. (2024).
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
MC <- function(lav,
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
  args <- list(
    lav = lav,
    R = R,
    alpha = alpha,
    decomposition = decomposition,
    pd = pd,
    tol = tol,
    seed = seed
  )
  # mc
  if (!is.null(decomposition)) {
    if (decomposition == "chol") {
      pd <- FALSE
    }
  }
  ## set up Monte Carlo
  if (!is.null(seed)) {
    set.seed(seed)
  }
  thetahatstar <- .ThetaHatStar(
    R = R,
    scale = lavaan::vcov(lav),
    location = lavaan::coef(lav),
    decomposition = decomposition,
    pd = pd,
    tol = tol
  )
  thetahatstar_orig <- thetahatstar$thetahatstar
  decomposition <- thetahatstar$decomposition
  ## extract all estimates including fixed parameters
  thetahat <- .ThetaHat(
    object = lav,
    est = NULL # should always be null
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
    args = args,
    thetahat = thetahat,
    thetahatstar = thetahatstar,
    fun = "MC"
  )
  class(out) <- c(
    "semmcci",
    class(out)
  )
  out
}
