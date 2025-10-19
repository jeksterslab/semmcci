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
#' *Communication Methods and Measures*, *6*(2), 77–98.
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
  if (!is.null(seed)) {
    set.seed(seed)
  }
  thetahatstar <- as.data.frame(
    t(
      .ThetaHatStar(
        R = R,
        scale = as.matrix(vcov),
        location = coef,
        decomposition = decomposition,
        pd = pd,
        tol = tol
      )$thetahatstar
    )
  )
  par <- FALSE
  if (!is.null(ncores)) {
    # nolint start
    ncores <- as.integer(ncores)
    if (ncores > 1) {
      par <- TRUE
    }
    # nolint end
  }
  if (par) {
    # nolint start
    available_cores <- parallel::detectCores()
    if (ncores >= available_cores) {
      ncores <- available_cores
    }
    os_type <- Sys.info()["sysname"]
    if (os_type == "Darwin") {
      fork <- TRUE
    } else if (os_type == "Linux") {
      fork <- TRUE
    } else {
      fork <- FALSE
    }
    if (fork) {
      thetahatstar <- parallel::mclapply(
        X = thetahatstar,
        FUN = func,
        mc.cores = ncores,
        ...
      )
    } else {
      cl <- parallel::makeCluster(ncores)
      on.exit(
        parallel::stopCluster(cl = cl)
      )
      thetahatstar <- parallel::parLapply(
        cl = cl,
        X = thetahatstar,
        fun = func,
        ...
      )
    }
    # nolint end
  } else {
    thetahatstar <- lapply(
      X = thetahatstar,
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
      est = func(coef)
    ),
    thetahatstar = thetahatstar,
    fun = "Func"
  )
  class(out) <- c(
    "semmcci",
    class(out)
  )
  out
}
