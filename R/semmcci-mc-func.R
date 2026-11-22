#' Monte Carlo Confidence Intervals (Function)
#'
#' Calculates Monte Carlo confidence intervals
#' for defined parameters.
#'
#' A sampling distribution of parameter estimates is generated
#' from the multivariate normal distribution
#' using the parameter estimates and the sampling variance-covariance matrix.
#' Confidence intervals for defined parameters
#' are generated using the simulated sampling distribution.
#' Parameters are defined using the `func` argument.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @details
#' Let \eqn{\hat{\boldsymbol{\theta}}} be the vector of parameter estimates
#' and \eqn{\hat{\boldsymbol{V}}} its sampling covariance matrix,
#' the arguments `coef` and `vcov`.
#' The Monte Carlo draws are
#' \deqn{\hat{\boldsymbol{\theta}}^{\ast}_{r} = \hat{\boldsymbol{\theta}} +
#' \boldsymbol{A}^{\prime} \boldsymbol{z}_{r}, \quad \boldsymbol{z}_{r} \sim
#' \mathcal{N} \left( \boldsymbol{0}, \boldsymbol{I} \right), \quad r = 1,
#' \dots, R,}
#' where \eqn{\boldsymbol{A}^{\prime} \boldsymbol{A} = \hat{\boldsymbol{V}}}.
#' With `decomposition = "eigen"`,
#' \eqn{\hat{\boldsymbol{V}} = \boldsymbol{Q} \boldsymbol{\Lambda}
#' \boldsymbol{Q}^{\prime}} and
#' \eqn{\boldsymbol{A} = \boldsymbol{\Lambda}_{+}^{1 / 2}
#' \boldsymbol{Q}^{\prime}},
#' with negative eigenvalues set to zero;
#' with `decomposition = "chol"`, \eqn{\boldsymbol{A}} is the upper Cholesky
#' factor (the eigenvalue decomposition is used if it fails);
#' `decomposition = "svd"` uses the singular value decomposition.
#' With `pd = TRUE`, the function stops unless every eigenvalue
#' \eqn{\lambda_{l} \geq - \mathrm{tol} \left| \lambda_{1} \right|}.
#' A defined parameter \eqn{\phi = h \left( \boldsymbol{\theta} \right)},
#' given by `func`,
#' has the draws
#' \eqn{\hat{\phi}^{\ast}_{r} = h \left( \hat{\boldsymbol{\theta}}^{\ast}_{r}
#' \right)}.
#' The confidence intervals are the percentile intervals
#' \eqn{\left[ \hat{G}^{-1} \left( \alpha / 2 \right), \hat{G}^{-1} \left( 1 -
#' \alpha / 2 \right) \right]},
#' where \eqn{\hat{G}} is the empirical distribution function of the draws
#' (Preacher & Selig, 2012).
#'
#' @param coef Numeric vector.
#'   Vector of estimated parameters.
#' @param vcov Numeric matrix.
#'   Sampling variance-covariance matrix of estimated parameters.
#' @param ncores Positive integer.
#'   Number of cores to use.
#'   If `ncores = NULL`, use single core.
#' @inheritParams Func
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
#'     \item{fun}{Function used ("MCFunc").}
#'   }
#'
#' @examples
#' library(semmcci)
#'
#' ## MCFunc() ----------------------------------------------------------------
#' ### Define func ------------------------------------------------------------
#' func <- function(x) {
#'   out <- exp(x)
#'   names(out) <- "exp"
#'   out
#' }
#' ### Generate Confidence Intervals ------------------------------------------
#' MCFunc(
#'   coef = 0,
#'   vcov = matrix(1),
#'   func = func,
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
#' *Communication Methods and Measures*, *6*(2), 77--98.
#' \doi{10.1080/19312458.2012.679848}
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
#' @keywords semmcci mc
#' @export
MCFunc <- function(coef,
                   vcov,
                   func,
                   ...,
                   R = 20000L,
                   alpha = c(0.001, 0.01, 0.05),
                   decomposition = "eigen",
                   pd = TRUE,
                   tol = 1e-06,
                   seed = NULL,
                   ncores = NULL) {
  args <- list(
    coef = coef,
    vcov = vcov,
    func = func,
    dots = ...,
    R = R,
    alpha = alpha,
    decomposition = decomposition,
    pd = pd,
    tol = tol,
    seed = seed,
    ncores = ncores
  )
  # mc
  ## set up Monte Carlo
  if (!is.null(x = seed)) {
    set.seed(seed = seed)
  }
  thetahatstar <- as.data.frame(
    x = t(
      x = .ThetaHatStar(
        R = R,
        scale = as.matrix(x = vcov),
        location = coef,
        decomposition = decomposition,
        pd = pd,
        tol = tol
      )$thetahatstar
    )
  )
  thetahatstar <- .MCFuncApply(
    X = thetahatstar,
    FUN = func,
    ...,
    ncores = ncores
  )
  thetahatstar <- do.call(
    what = "rbind",
    args = thetahatstar
  )
  # output
  out <- list(
    call = match.call(),
    args = args,
    thetahat = list(
      est = func(coef)
    ),
    thetahatstar = thetahatstar,
    fun = "Func"
  )
  class(x = out) <- c(
    "semmcci",
    class(x = out)
  )
  out
}
