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
#' @inheritParams lavaan::standardizedSolution
#' @param object object of class `semmcci`.
#'   Output of the `MC()` function.
#' @return Returns an object of class `semmcci_std`
#' which is a list with the following elements:
#' \itemize{
#'   \item{`lavaan`}{`lavaan` object.}
#'   \item{`mu`}{Mean vector used in the Monte Carlo simulation.}
#'   \item{`Sigma`}{Variance-covariance matrix used in the Monte Carlo simulation.}
#'   \item{`thetahat`}{Parameter estimates.}
#'   \item{`thetahatstar`}{Sampling distribution of parameter estimates.}
#'   \item{`ci`}{Confidence intervals.}
#'   \item{`thetahat.std`}{Standardized parameter estimates.}
#'   \item{`thetahatstar.std`}{Standardized sampling distribution of parameter estimates.}
#'   \item{`ci.std`}{Standardized confidence intervals.}
#' }
#' The list element `ci.std` is a matrix with the following columns:
#' \itemize{
#'   \item{`est`}{Standardized parameter estimates.}
#'   \item{`se`}{Standard errors or the square root of the diagonals of the standardized Monte Carlo sampling distribution of parameter estimates.}
#'   \item{`R`}{Number of Monte Carlo replications.}
#'   \item{...}{Percentiles that correspond to the confidence intervals defined by `alpha`.}
#' }
#' Note that the rows in `ci.std` correspond to the standardized model parameters.
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
                  alpha = c(0.001, 0.01, 0.05),
                  type = "std.all",
                  par = FALSE,
                  ncores = NULL) {
  stopifnot(
    methods::is(
      object,
      "semmcci"
    )
  )
  if (par) {
    if (is.null(ncores)) {
      ncores <- parallel::detectCores()
    }
    cl <- parallel::makeCluster(ncores)
    pkgs <- c(
      "lavaan",
      "semmcci"
    )
    parallel::clusterExport(
      cl = cl,
      varlist = "pkgs",
      envir = environment()
    )
    parallel::clusterEvalQ(
      cl = cl,
      expr = {
        sapply(
          X = pkgs,
          FUN = function(x) {
            library(
              package = x,
              character.only = TRUE
            )
          }
        )
      }
    )
    parallel::clusterExport(
      cl = cl,
      varlist = ls(envir = parent.frame()),
      envir = environment()
    )
    on.exit(
      parallel::stopCluster(cl),
      add = TRUE
    )
  } else {
    cl <- NULL
  }
  thetahat <- as.vector(
    lavaan::standardizedSolution(
      object = object$lavaan,
      type = type,
      se = FALSE,
      zstat = FALSE,
      pvalue = FALSE,
      ci = FALSE,
      remove.eq = FALSE,
      remove.ineq = FALSE,
      remove.def = FALSE
    )[, "est.std"]
  )
  names(thetahat) <- colnames(object$thetahatstar)
  i_free <- object$lavaan@ParTable$free > 0
  foo <- function(i) {
    GLIST_i <- lavaan::lav_model_set_parameters(
      lavmodel = object$lavaan@Model,
      x = object$thetahatstar[i, i_free]
    )@GLIST
    return(
      as.vector(
        lavaan::standardizedSolution(
          object = object$lavaan,
          type = type,
          est = object$thetahatstar[i, ],
          GLIST = GLIST_i,
          se = FALSE,
          zstat = FALSE,
          pvalue = FALSE,
          ci = FALSE
        )[, "est.std"]
      )
    )
  }
  thetahatstar <- pbapply::pblapply(
    X = seq_len(dim(object$thetahatstar)[1]),
    FUN = foo,
    cl = cl
  )
  thetahatstar <- do.call(
    what = "rbind",
    args = thetahatstar
  )
  colnames(thetahatstar) <- colnames(object$thetahatstar)
  se <- sqrt(diag(stats::var(thetahatstar)))
  ci <- vector(
    mode = "list",
    length = dim(thetahatstar)[2]
  )
  for (i in seq_len(dim(thetahatstar)[2])) {
    ci[[i]] <- .PCCI(
      thetahatstar = thetahatstar[, i],
      thetahat = thetahat[[i]],
      alpha = alpha
    )
  }
  ci <- do.call(
    what = "rbind",
    args = ci
  )
  rownames(ci) <- colnames(thetahatstar)
  # ci <- ci[which(object$lavaan@ParTable$op != "~1"), ]
  ci <- ci[which(!rownames(ci) %in% object$thetahat$fixed), ]
  out <- list(
    lavaan = object$lavaan,
    mu = object$mu,
    Sigma = object$Sigma,
    thetahat = object$thetahat,
    thetahatstar = object$thetahatstar,
    ci = object$ci,
    thetahat.std = thetahat,
    thetahatstar.std = thetahatstar,
    ci.std = ci
  )
  class(out) <- c(
    "semmcci_std",
    class(out)
  )
  return(out)
}
