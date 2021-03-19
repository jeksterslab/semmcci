#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Standardized Monte Carlo Confidence Intervals
#'
#' @description Calculates standardized Monte Carlo confidence intervals.
#'
#' @details The empirical sampling distribution
#'   of parameter estimates from the argument `object` are standardized.
#'   Confidence intervals are generated
#'   using the standardized empirical sampling distribution.
#'
#' @inheritParams lavaan::standardizedSolution
#' @inheritParams mc
#' @param object object of class `semmcci`.
#'   The output of `semmcci::mc`.
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
#' fit <- sem(
#'   data = data,
#'   model = model,
#'   fixed.x = FALSE
#' )
#'
#' # Monte Carlo --------------------------------------------------------------
#' mc <- mc(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = c(0.001, 0.01, 0.05)
#' )
#'
#' # Standardized Monte Carlo -------------------------------------------------
#' std <- std(mc)
#' print(std)
#' @export
std <- function(object,
                type = "std.all",
                alpha = c(0.001, 0.01, 0.05),
                par = FALSE,
                ncores = NULL) {
  stopifnot(
    methods::is(
      object,
      "mc"
    )
  )
  thetahat <- as.vector(
    lavaan::standardizedSolution(
      object = object$lavaan,
      type = type,
      se = FALSE,
      zstat = FALSE,
      pvalue = FALSE,
      ci = FALSE
    )[, "est.std"]
  )
  names(thetahat) <- colnames(object$thetahatstar)
  foo <- function(i) {
    return(
      as.vector(
        lavaan::standardizedSolution(
          object = object$lavaan,
          type = type,
          est = object$thetahatstar[i, ],
          se = FALSE,
          zstat = FALSE,
          pvalue = FALSE,
          ci = FALSE
        )[, "est.std"]
      )
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
    ci[[i]] <- .pcci(
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
  thetahat_free <- thetahat[names(object$thetahat.free)]
  index <- !(names(thetahat) %in% names(thetahat_free))
  fixed <- thetahat[index]
  fixed_names <- names(thetahat[index])
  if (length(fixed) > 0) {
    ci_rownames <- rownames(ci)
    ci_colnames <- colnames(ci)
    ci_colnames <- ifelse(
      test = ci_colnames == "est",
      yes = NA,
      no = ci_colnames
    )
    ci_colnames <- ci_colnames[stats::complete.cases(ci_colnames)]
    for (i in seq_along(ci_rownames)) {
      for (j in seq_along(fixed_names)) {
        if (ci_rownames[i] == fixed_names[j]) {
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
    thetahat.free = object$thetahat.free,
    thetahatstar = object$thetahatstar,
    ci = object$ci,
    thetahat.std = thetahat,
    thetahat.free.std = thetahat_free,
    thetahatstar.std = thetahatstar,
    ci.std = ci
  )
  class(out) <- c(
    "std",
    class(out)
  )
  invisible(out)
}
