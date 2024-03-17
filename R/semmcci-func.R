#' Monte Carlo Confidence Intervals (List)
#'
#' Calculates Monte Carlo confidence intervals
#' for defined parameters.
#'
#' The distribution of parameters is provided as a list (`params`)
#' and the definition of the function of paremeters
#' is provided by a function (`func`).
#' Confidence intervals for defined parameters
#' are generated using the generated sampling distribution.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param coef List.
#'   A list of parameters.
#' @param func R function.
#'   1. The first argument `x` is the argument `coef`.
#'   2. The function algebraically manipulates `coef`
#'      to return at a new numeric vector.
#'      It is best to have a named vector as an output.
#'   3. The function can take additional named arguments
#'      passed using `...`.
#' @param ... Additional arguments to pass to `func`.
#' @param est Numeric vector.
#'   Vector of original parameter estimates.
#' @param ncores Positive integer.
#'   Number of cores to use.
#'   If `ncores = NULL`, use single core.
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
#'     \item{fun}{Function used ("Func").}
#'   }
#'
#' @examples
#' library(semmcci)
#'
#' ## Generate Parameters -----------------------------------------------------
#' coef <- lapply(
#'   X = 1:5,
#'   FUN = function(i) {
#'     rnorm(n = 1)
#'   }
#' )
#'
#' ## Func() ------------------------------------------------------------------
#' ### Define func ------------------------------------------------------------
#' func <- function(x) {
#'   out <- exp(x)
#'   names(out) <- "exp"
#'   return(out)
#' }
#' ### Generate Confidence Intervals ------------------------------------------
#' Func(
#'   coef,
#'   func = func,
#'   est = 1,
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
Func <- function(coef,
                 func,
                 ...,
                 est,
                 alpha = c(0.001, 0.01, 0.05),
                 ncores = NULL) {
  args <- list(
    coef = coef,
    func = func,
    dots = ...,
    est = est,
    alpha = alpha,
    ncores = ncores
  )
  par <- FALSE
  if (!is.null(ncores)) {
    ncores <- as.integer(ncores)
    if (ncores > 1) {
      par <- TRUE
    }
  }
  if (par) {
    cl <- parallel::makeCluster(ncores)
    on.exit(
      parallel::stopCluster(cl = cl)
    )
    thetahatstar <- parallel::parLapply(
      cl = cl,
      X = coef,
      fun = func,
      ...
    )
  } else {
    thetahatstar <- lapply(
      X = coef,
      FUN = func,
      ...
    )
  }
  thetahatstar <- do.call(
    what = "rbind",
    args = thetahatstar
  )
  # output
  out <- list(
    call = match.call(),
    args = args,
    thetahat = list(
      est = func(est)
    ),
    thetahatstar = thetahatstar,
    fun = "Func"
  )
  class(out) <- c(
    "semmcci",
    class(out)
  )
  return(out)
}
