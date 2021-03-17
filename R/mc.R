#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Confidence Intervals
#'
#' @description Calculates Monte Carlo confidence intervals
#'   for parameters defined using the `:=` operator in [lavaan].
#'
#' @family Monte Carlo method functions
#' @keywords mc
#' @param object lavaan object
#'   with defined parameters using the `:=` operator
#'   in the [lavaan] model syntax.
#' @param R Integer.
#'   Number of Monte Carlo replications.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'   Default value is `alpha = c(0.001, 0.01, 0.05)`.
#' @param plot Logical.
#'  If `TRUE`, plots the sampling distribution of the defined parameter estimate/s.
#' @return Returns a matrix with the following columns:
#'   \describe{
#'     \item{est}{Parameter estimate.}
#'     \item{se}{Standard error of Monte Carlo sampling distribution.}
#'     \item{R}{Number of Monte Carlo replications.}
#'     \item{limits}{Confidence limits \eqn{\frac{\alpha}{2} , 1 - \frac{\alpha}{2}}.}
#'   }
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' n <- 1000
#' x <- rnorm(n = n)
#' m <- 0.50 * x + rnorm(n = n)
#' y <- 0.25 * x + 0.50 * m + rnorm(n = n)
#' data <- data.frame(x, m, y)
#' model <- "
#'   y ~ cp * x + b * m
#'   m ~ a * x
#'   ab := a * b
#' "
#' object <- sem(
#'   data = data,
#'   model = model
#' )
#'
#' # Monte Carlo
#'
#' mc(
#'   object = object,
#'   R = 20000L,
#'   alpha = c(0.001, 0.01, 0.05),
#'   plot = TRUE
#' )
#' @export
mc <- function(object,
               R = 20000L,
               alpha = c(0.001, 0.01, 0.05),
               plot = TRUE) {
  if (class(object) != "lavaan") {
    stop(
      "The `object` argument should be of class `lavaan`."
    )
  }
  R <- as.integer(R)
  mu <- lavaan::coef(object)
  Sigma <- lavaan::vcov(object)
  tryCatch(
    {
      thetahat <- object@Model@def.function(mu)
    },
    error = function(e) {
      stop(
        "Make sure you have defined parameters using `:=` in your lavaan model syntax."
      )
    }
  )
  thetahatstar <- MASS::mvrnorm(
    n = R,
    mu = mu,
    Sigma = Sigma
  )
  thetahatstar <- lapply(
    X = 1:nrow(thetahatstar),
    FUN = .def,
    object = object,
    thetahatstar = thetahatstar
  )
  thetahatstar <- do.call(
    what = "rbind",
    args = thetahatstar
  )
  se <- sqrt(diag(stats::var(thetahatstar)))
  ci <- vector(
    mode = "list",
    length = dim(thetahatstar)[2]
  )
  for (i in 1:dim(thetahatstar)[2]) {
    if (plot) {
      graphics::hist(
        thetahatstar[, i],
        main = paste("Histogram of", colnames(thetahatstar)[i]),
        xlab = colnames(thetahatstar)[i]
      )
    }
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
  print(
    round(
      ci,
      digits = 4
    )
  )
  invisible(ci)
}

.pcci <- function(thetahatstar,
                  thetahat,
                  alpha = c(0.001, 0.01, 0.05)) {
  thetahatstar <- as.vector(thetahatstar)
  thetahatstar <- thetahatstar[stats::complete.cases(thetahatstar)]
  alpha <- sort(alpha)
  prob_ll <- alpha / 2
  prob_ul <- rev(1 - prob_ll)
  probs <- c(prob_ll, prob_ul)
  ci <- stats::quantile(
    x = thetahatstar,
    probs = probs
  )
  out <- c(
    thetahat,
    stats::sd(thetahatstar),
    length(thetahatstar),
    ci
  )
  names(out) <- c(
    "est",
    "se",
    "R",
    paste0(
      "ci_",
      probs * 100
    )
  )
  out
}

.def <- function(i,
                 object,
                 thetahatstar) {
  object@Model@def.function(thetahatstar[i, ])
}
