#' Standardized Monte Carlo Confidence Intervals
#'
#' Calculates standardized Monte Carlo confidence intervals
#' for free and defined parameters.
#'
#' The empirical sampling distribution
#' of parameter estimates from the argument `object` is standardized.
#' Confidence intervals are generated
#' using the standardized empirical sampling distribution.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams MC
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
#' @importFrom methods is
#' @importFrom lavaan standardizedSolution
#' @importFrom parallel detectCores makeCluster parLapply stopCluster
#' @importFrom stats var complete.cases
#' @export
MCStd <- function(object,
                  alpha = c(0.001, 0.01, 0.05),
                  par = FALSE,
                  ncores = NULL) {
  stopifnot(
    methods::is(
      object,
      "semmcci"
    )
  )
  thetahat <- as.vector(
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
    )[, "est.std"]
  )
  names(thetahat) <- colnames(object$thetahatstar)
  i_free <- lavaan::parameterTable(object$lavaan)$free > 0
  foo <- function(i) {
    .StdLav(
      est = object$thetahatstar[i, i_free],
      object = object$lavaan
    )
  }
  if (par) {
    if (is.null(ncores)) {
      ncores <- parallel::detectCores()
    }
    cl <- parallel::makeCluster(ncores)
    thetahatstar <- parallel::parLapply(
      cl = cl,
      X = seq_len(dim(object$thetahatstar)[1]),
      fun = foo
    )
    parallel::stopCluster(cl)
  } else {
    thetahatstar <- lapply(
      X = seq_len(dim(object$thetahatstar)[1]),
      FUN = foo
    )
  }
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
  # put NA to rows of fixed parameters
  if (length(object$thetahat$fixed) > 0) {
    ci_rownames <- rownames(ci)
    ci_colnames <- colnames(ci)
    ci_colnames <- ifelse(
      test = ci_colnames == "est",
      yes = NA,
      no = ci_colnames
    )
    ci_colnames <- ci_colnames[stats::complete.cases(ci_colnames)]
    for (i in seq_along(ci_rownames)) {
      for (j in seq_along(object$thetahat$fixed)) {
        if (ci_rownames[i] == object$thetahat$fixed[j]) {
          ci[i, ci_colnames] <- NA
        }
      }
    }
  }
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
  out
}
