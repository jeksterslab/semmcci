#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Confidence Intervals
#'
#' @description Calculates Monte Carlo confidence intervals.
#'
#' @details An empirical sampling distribution of parameter estimates
#'   is generated using the fitted model given by
#'
#'   \deqn{
#'     \hat{\boldsymbol{\theta}}^{\ast}
#'     \sim
#'     \mathcal{N}_{p}
#'     \left(
#'       \boldsymbol{\mu} = \hat{\boldsymbol{\theta}},
#'       \boldsymbol{\Sigma} = \mathrm{Cov}
#'       \left( \hat{\boldsymbol{\theta}} \right)
#'     \right) .
#'   }
#'
#'   Confidence intervals are generated
#'   using the empirical sampling distribution.
#'
#' @family Monte Carlo method functions
#' @keywords mc
#' @param object object of class `lavaan`.
#' @param R Integer.
#'   Number of Monte Carlo replications.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'   Default value is `alpha = c(0.001, 0.01, 0.05)`.
#' @param par Logical.
#'   If `par = TRUE`, use multiple cores.
#' @param ncores Integer.
#'   Number of cores to use if `par = TRUE`.
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
#'   model = model
#' )
#'
#' # Monte Carlo --------------------------------------------------------------
#' mc <- mc(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = c(0.001, 0.01, 0.05)
#' )
#' print(mc)
#' @export
mc <- function(object,
               R = 20000L,
               alpha = c(0.001, 0.01, 0.05),
               par = FALSE,
               ncores = NULL) {
  stopifnot(
    methods::is(
      object,
      "lavaan"
    )
  )
  # extract all estimates including fixed parameters
  thetahat <- .thetahat(object)
  # set up Monte Carlo
  R <- as.integer(R)
  mu <- lavaan::coef(object)
  Sigma <- lavaan::vcov(object)
  # sampling distribution
  thetahatstar_orig <- MASS::mvrnorm(
    n = R,
    mu = mu,
    Sigma = Sigma
  )
  # generate defined parameters
  foo <- function(i) {
    return(
      object@Model@def.function(
        thetahatstar_orig[i, ]
      )
    )
  }
  if (par) {
    if (is.null(ncores)) {
      ncores <- parallel::detectCores()
    }
    cl <- parallel::makeCluster(ncores)
    thetahatstar_def <- parallel::parLapply(
      cl = cl,
      X = seq_len(dim(thetahatstar_orig)[1]),
      fun = foo
    )
    parallel::stopCluster(cl)
  } else {
    thetahatstar_def <- lapply(
      X = seq_len(dim(thetahatstar_orig)[1]),
      FUN = foo
    )
  }
  thetahatstar_def <- do.call(
    what = "rbind",
    args = thetahatstar_def
  )
  thetahatstar <- cbind(
    thetahatstar_orig,
    thetahatstar_def
  )
  # thetahat_free
  thetahat_free <- thetahat[colnames(thetahatstar)]
  # add fixed parameters to sampling distribution
  index <- !(names(thetahat) %in% names(thetahat_free))
  fixed <- thetahat[index]
  fixed_names <- names(thetahat[index])
  if (length(fixed) > 0) {
    append <- vector(
      mode = "list",
      length = length(fixed)
    )
    for (i in length(fixed)) {
      append[[i]] <- as.matrix(
        rep(
          x = fixed[[i]],
          length = dim(thetahatstar)[1]
        )
      )
      colnames(append[[i]]) <- fixed_names[i]
      thetahatstar <- cbind(
        thetahatstar,
        append[[i]]
      )
    }
  }
  thetahatstar <- thetahatstar[, names(thetahat)]
  # inferences
  se <- sqrt(diag(stats::var(thetahatstar)))
  ci <- vector(
    mode = "list",
    length = dim(thetahatstar)[2]
  )
  for (i in 1:dim(thetahatstar)[2]) {
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
    lavaan = object,
    mu = mu,
    Sigma = Sigma,
    thetahat = thetahat,
    thetahat.free = thetahat_free,
    thetahatstar = thetahatstar,
    ci = ci
  )
  class(out) <- c(
    "mc",
    class(out)
  )
  invisible(out)
}
