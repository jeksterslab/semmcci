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
#'   If unspecified, uses the output of `parallel::detectCores()`.
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
  thetahatstar_orig <- MASS::mvrnorm(
    n = R,
    mu = mu,
    Sigma = Sigma
  )
  # generate defined parameters
  if (length(thetahat$def) > 0) {
    def <- function(i) {
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
        fun = def
      )
      parallel::stopCluster(cl)
    } else {
      thetahatstar_def <- lapply(
        X = seq_len(dim(thetahatstar_orig)[1]),
        FUN = def
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
  } else {
    thetahatstar <- thetahatstar_orig
  }
  # generate equality
  if (length(thetahat$ceq) > 0) {
    ceq <- function(i) {
      out <- object@Model@ceq.function(
        thetahatstar[i, ]
      )
      names(out) <- paste0(thetahat$ceq, "_ceq")
      return(out)
    }
    if (par) {
      if (is.null(ncores)) {
        ncores <- parallel::detectCores()
      }
      cl <- parallel::makeCluster(ncores)
      thetahatstar_ceq <- parallel::parLapply(
        cl = cl,
        X = seq_len(dim(thetahatstar)[1]),
        fun = ceq
      )
      parallel::stopCluster(cl)
    } else {
      thetahatstar_ceq <- lapply(
        X = seq_len(dim(thetahatstar)[1]),
        FUN = ceq
      )
    }
    thetahatstar_ceq <- do.call(
      what = "rbind",
      args = thetahatstar_ceq
    )
    thetahatstar <- cbind(
      thetahatstar,
      thetahatstar_ceq
    )
  }
  # generate inequality
  if (length(thetahat$cin) > 0) {
    cin <- function(i) {
      out <- object@Model@cin.function(
        thetahatstar[i, ]
      )
      names(out) <- paste0(thetahat$cin, "_cin")
      return(out)
    }
    if (par) {
      if (is.null(ncores)) {
        ncores <- parallel::detectCores()
      }
      cl <- parallel::makeCluster(ncores)
      thetahatstar_cin <- parallel::parLapply(
        cl = cl,
        X = seq_len(dim(thetahatstar)[1]),
        fun = cin
      )
      parallel::stopCluster(cl)
    } else {
      thetahatstar_cin <- lapply(
        X = seq_len(dim(thetahatstar)[1]),
        FUN = cin
      )
    }
    thetahatstar_cin <- do.call(
      what = "rbind",
      args = thetahatstar_cin
    )
    thetahatstar <- cbind(
      thetahatstar,
      thetahatstar_cin
    )
  }
  # generate fixed
  if (length(thetahat$fixed) > 0) {
    fixed <- matrix(
      NA,
      ncol = length(thetahat$fixed),
      nrow = dim(thetahatstar)[1]
    )
    colnames(fixed) <- thetahat$fixed
    for (i in seq_len(dim(fixed)[2])) {
      fixed[, i] <- thetahat$est[thetahat$fixed[[i]]]
    }
    thetahatstar <- cbind(
      thetahatstar,
      fixed
    )
  }
  # rearrange
  thetahatstar <- thetahatstar[, thetahat$labels]
  # inferences
  se <- sqrt(diag(stats::var(thetahatstar)))
  ci <- vector(
    mode = "list",
    length = dim(thetahatstar)[2]
  )
  for (i in seq_len(dim(thetahatstar)[2])) {
    ci[[i]] <- .pcci(
      thetahatstar = thetahatstar[, i],
      thetahat = thetahat$est[[i]],
      alpha = alpha
    )
  }
  ci <- do.call(
    what = "rbind",
    args = ci
  )
  rownames(ci) <- colnames(thetahatstar)
  # put NA to rows of fixed parameters
  if (length(thetahat$fixed) > 0) {
    ci_rownames <- rownames(ci)
    ci_colnames <- colnames(ci)
    ci_colnames <- ifelse(
      test = ci_colnames == "est",
      yes = NA,
      no = ci_colnames
    )
    ci_colnames <- ci_colnames[stats::complete.cases(ci_colnames)]
    for (i in seq_along(ci_rownames)) {
      for (j in seq_along(thetahat$fixed)) {
        if (ci_rownames[i] == thetahat$fixed[j]) {
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
    thetahatstar = thetahatstar,
    ci = ci
  )
  class(out) <- c(
    "mc",
    class(out)
  )
  invisible(out)
}
