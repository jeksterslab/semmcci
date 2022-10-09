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
#' @param decomposition Character string.
#'   Matrix decomposition of the sampling variance-covariance matrix for the data generation.
#'   If `decomposition = "chol"`, use Cholesky decomposition.
#'   If `decomposition = "eigen"`, use eigenvalue decomposition.
#'   If `decomposition = "svd"`, use singular value decomposition.
#'   If `decomposition = NULL`, try Cholesky decomposition.
#'   If Cholesky decomposition fails, try eigenvalue decomposition.
#'   Finally, if eigenvalue decomposition fails, try singular value decomposition.
#' @param pd Logical.
#'   If `pd = TRUE`, check if the sampling variance-covariance matrix is positive definite using `tol` if `decomposition %in% c("eigen", "svd")`.
#' @param tol Numeric.
#'   Tolerance used for `pd`..
#' @return Returns an object of class `semmcci` which is a list with the following elements:
#' \describe{
#'   \item{`R`}{Number of Monte Carlo replications.}
#'   \item{`alpha`}{Significance level specified.}
#'   \item{`lavaan`}{`lavaan` object.}
#'   \item{`decomposition`}{Matrix decomposition used to generate multivariate normal random variates.}
#'   \item{`thetahat`}{Parameter estimates.}
#'   \item{`thetahatstar`}{Sampling distribution of parameter estimates.}
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
#' fit <- sem(data = data, model = model)
#'
#' # Monte Carlo --------------------------------------------------------------
#' MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = c(0.001, 0.01, 0.05)
#' )
#' @importFrom methods is
#' @importFrom stats var complete.cases
#' @keywords mc
#' @export
MC <- function(object,
               R = 20000L,
               alpha = c(0.001, 0.01, 0.05),
               decomposition = "chol",
               pd = TRUE,
               tol = 1e-06) {
  stopifnot(
    methods::is(
      object,
      "lavaan"
    )
  )
  if (!is.null(decomposition)) {
    if (decomposition == "chol") {
      pd <- FALSE
    }
  }
  # set up Monte Carlo
  thetahatstar <- .ThetaStar(
    R = R,
    scale = lavaan::vcov(object),
    location = lavaan::coef(object),
    decomposition = decomposition,
    pd = pd,
    tol = tol
  )
  thetahatstar_orig <- thetahatstar$thetahatstar
  decomposition <- thetahatstar$decomposition
  # extract all estimates including fixed parameters
  thetahat <- .ThetaHat(
    object = object
  )
  # generate defined parameters
  if (length(thetahat$def) > 0) {
    def <- function(i) {
      tryCatch(
        {
          return(
            object@Model@def.function(
              thetahatstar_orig[
                i,
              ]
            )
          )
        },
        warning = function(w) {
          return(NA)
        },
        error = function(e) {
          return(NA)
        }
      )
    }
    thetahatstar_def <- lapply(
      X = seq_len(
        dim(
          thetahatstar_orig
        )[1]
      ),
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
        thetahatstar[
          i,
        ]
      )
      names(out) <- paste0(
        thetahat$ceq,
        "_ceq"
      )
      return(out)
    }
    thetahatstar_ceq <- lapply(
      X = seq_len(
        dim(
          thetahatstar
        )[1]
      ),
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
        thetahatstar[
          i,
        ]
      )
      names(out) <- paste0(
        thetahat$cin,
        "_cin"
      )
      return(out)
    }
    thetahatstar_cin <- lapply(
      X = seq_len(
        dim(
          thetahatstar
        )[1]
      ),
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
      ncol = length(
        thetahat$fixed
      ),
      nrow = dim(
        thetahatstar
      )[1]
    )
    colnames(
      fixed
    ) <- thetahat$fixed
    for (i in seq_len(dim(fixed)[2])) {
      fixed[
        ,
        i
      ] <- thetahat$est[
        thetahat$fixed[[i]]
      ]
    }
    thetahatstar <- cbind(
      thetahatstar,
      fixed
    )
  }
  # rearrange
  thetahatstar <- thetahatstar[
    ,
    thetahat$par_names
  ]
  # output
  out <- list(
    R = R,
    alpha = alpha,
    lavaan = object,
    decomposition = decomposition,
    thetahat = thetahat,
    thetahatstar = thetahatstar
  )
  class(out) <- c(
    "semmcci",
    class(out)
  )
  return(out)
}
