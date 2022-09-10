#' Monte Carlo Confidence Intervals
#'
#' Calculates Monte Carlo confidence intervals
#' for free and defined parameters
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
#' @param object object of class `lavaan`.
#' @param R Positive integer.
#'   Number of Monte Carlo replications.
#' @param alpha Numeric vector.
#'   Significance level.
#'   Default value is `alpha = c(0.001, 0.01, 0.05)`.
#' @return Returns an object of class `semmcci` which is a list with the following elements:
#' \itemize{
#'   \item{`lavaan`}{`lavaan` object.}
#'   \item{`mu`}{Mean vector used in the Monte Carlo simulation.}
#'   \item{`Sigma`}{Variance-covariance matrix used in the Monte Carlo simulation.}
#'   \item{`thetahat`}{Parameter estimates.}
#'   \item{`thetahatstar`}{Sampling distribution of parameter estimates.}
#'   \item{`ci`}{Confidence intervals.}
#' }
#' The list element `ci` is a matrix with the following columns:
#' \itemize{
#'   \item{`est`}{Parameter estimates.}
#'   \item{`se`}{Standard errors or the square root of the diagonals of the Monte Carlo sampling distribution of parameter estimates.}
#'   \item{`R`}{Number of Monte Carlo replications.}
#'   \item{...}{Percentiles that correspond to the confidence intervals defined by `alpha`.}
#' }
#' Note that the rows in `ci` correspond to the model parameters.
#' Parameters with zero standard errors and constant confidence limits are fixed parameters.
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
#' fit <- sem(data = data, model = model)
#'
#' # Monte Carlo --------------------------------------------------------------
#' MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = c(0.001, 0.01, 0.05)
#' )
#' @importFrom methods is
#' @importFrom lavaan coef vcov
#' @importFrom MASS mvrnorm
#' @importFrom stats var complete.cases
#' @keywords mc
#' @export
MC <- function(object,
               R = 20000L,
               alpha = c(0.001, 0.01, 0.05)) {
  stopifnot(
    methods::is(
      object,
      "lavaan"
    )
  )
  if (object@pta$nlevels > 1) {
    stop("Multilevel analysis is not yet supported.")
  }
  # extract all estimates including fixed parameters
  thetahat <- .ThetaHat(object)
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
    thetahatstar_def <- lapply(
      X = seq_len(dim(thetahatstar_orig)[1]),
      FUN = def
    )
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
    thetahatstar_ceq <- lapply(
      X = seq_len(dim(thetahatstar)[1]),
      FUN = ceq
    )
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
    thetahatstar_cin <- lapply(
      X = seq_len(dim(thetahatstar)[1]),
      FUN = cin
    )
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
    ci[[i]] <- .PCCI(
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
  ci <- ci[which(!rownames(ci) %in% thetahat$fixed), ]
  out <- list(
    lavaan = object,
    mu = mu,
    Sigma = Sigma,
    thetahat = thetahat,
    thetahatstar = thetahatstar,
    ci = ci
  )
  class(out) <- c(
    "semmcci",
    class(out)
  )
  out
}
