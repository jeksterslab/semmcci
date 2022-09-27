#' Print Method for Object of Class `semmcci`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `semmcci`.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#' @return Returns a matrix of estimates, standard errors, number of Monte Carlo replications, and confidence intervals.
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
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = c(0.001, 0.01, 0.05)
#' )
#' print(unstd)
#' @keywords method
#' @export
print.semmcci <- function(x,
                          digits = 4,
                          ...) {
  cat("Monte Carlo Confidence Intervals\n")
  base::print(
    round(
      .MCCI(x),
      digits = digits
    )
  )
}

#' Print Method for Object of Class `semmccistd`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `semmccistd`.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#' @return Returns a matrix of estimates, standard errors, number of Monte Carlo replications, and confidence intervals.
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
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = c(0.001, 0.01, 0.05)
#' )
#'
#' # Standardized Monte Carlo -------------------------------------------------
#' std <- MCStd(unstd)
#' print(std)
#' @keywords method
#' @export
print.semmccistd <- function(x,
                             digits = 4,
                             ...) {
  cat("Standardized Monte Carlo Confidence Intervals\n")
  base::print(
    round(
      .MCCI(x),
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class `semmcci`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `semmcci`.
#' @param ... additional arguments.
#' @param digits Digits to print.
#' @return Returns a matrix of estimates, standard errors, number of Monte Carlo replications, and confidence intervals.
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
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = c(0.001, 0.01, 0.05)
#' )
#' summary(unstd)
#' @keywords method
#' @export
summary.semmcci <- function(object,
                            digits = 4,
                            ...) {
  cat("Monte Carlo Confidence Intervals\n")
  return(
    round(
      .MCCI(object),
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class `semmccistd`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `semmccistd`.
#' @param ... additional arguments.
#' @param digits Digits to print.
#' @return Returns a matrix of estimates, standard errors, number of Monte Carlo replications, and confidence intervals.
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
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = c(0.001, 0.01, 0.05)
#' )
#'
#' # Standardized Monte Carlo -------------------------------------------------
#' std <- MCStd(unstd)
#' summary(std)
#' @keywords method
#' @export
summary.semmccistd <- function(object,
                               digits = 4,
                               ...) {
  cat("Standardized Monte Carlo Confidence Intervals\n")
  return(
    round(
      .MCCI(object),
      digits = digits
    )
  )
}

#' Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `semmcci`.
#' @param ... additional arguments.
#' @return Returns a vector of parameter estimates.
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
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = c(0.001, 0.01, 0.05)
#' )
#' coef(unstd)
#' @keywords method
#' @importFrom stats coef
#' @export
coef.semmcci <- function(object,
                         ...) {
  object$thetahat$est
}

#' Standardized Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `semmccistd`.
#' @param ... additional arguments.
#' @return Returns a vector of standardized parameter estimates.
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
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = c(0.001, 0.01, 0.05)
#' )
#'
#' # Standardized Monte Carlo -------------------------------------------------
#' std <- MCStd(unstd)
#' coef(std)
#' @keywords method
#' @importFrom stats coef
#' @export
coef.semmccistd <- function(object,
                            ...) {
  object$thetahat_std
}

#' Sampling Covariance Matrix of the Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `semmccistd`.
#' @param ... additional arguments.
#' @return Returns a matrix of the variance-covariance matrix of parameter estimates.
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
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = c(0.001, 0.01, 0.05)
#' )
#' vcov(unstd)
#' @keywords method
#' @importFrom stats vcov
#' @export
vcov.semmcci <- function(object,
                         ...) {
  stats::var(
    object$thetahatstar
  )
}

#' Sampling Covariance Matrix of the Standardized Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `semmccistd`.
#' @param ... additional arguments.
#' @return Returns a matrix of the variance-covariance matrix of standardized parameter estimates.
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
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = c(0.001, 0.01, 0.05)
#' )
#'
#' # Standardized Monte Carlo -------------------------------------------------
#' std <- MCStd(unstd)
#' vcov(std)
#' @keywords method
#' @importFrom stats vcov
#' @export
vcov.semmccistd <- function(object,
                            ...) {
  stats::var(
    object$thetahatstar_std
  )
}

#' Monte Carlo Confidence Intervals for the Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `semmcci`.
#' @param ... additional arguments.
#' @param parm a specification of which parameters are to be given confidence intervals,
#'   either a vector of numbers or a vector of names. If missing, all parameters are considered.
#' @param level the confidence level required.
#' @return Returns a matrix of confidence intervals.
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
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = c(0.001, 0.01, 0.05)
#' )
#' confint(unstd)
#' @keywords method
#' @export
confint.semmcci <- function(object,
                            parm = NULL,
                            level = 0.95,
                            ...) {
  ci <- .MCCI(
    object,
    alpha = 1 - level[1]
  )
  if (is.null(parm)) {
    parm <- rownames(
      ci
    )
  }
  return(
    ci[parm, 4:5]
  )
}

#' Monte Carlo Confidence Intervals for the Standardized Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `semmccistd`.
#' @param ... additional arguments.
#' @param parm a specification of which parameters are to be given confidence intervals,
#'   either a vector of numbers or a vector of names. If missing, all parameters are considered.
#' @param level the confidence level required.
#' @return Returns a matrix of confidence intervals.
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
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = c(0.001, 0.01, 0.05)
#' )
#'
#' # Standardized Monte Carlo -------------------------------------------------
#' std <- MCStd(unstd)
#' confint(std)
#' @keywords method
#' @export
confint.semmccistd <- function(object,
                               parm = NULL,
                               level = 0.95,
                               ...) {
  ci <- .MCCI(
    object,
    alpha = 1 - level[1]
  )
  if (is.null(parm)) {
    parm <- rownames(
      ci
    )
  }
  return(
    ci[parm, 4:5]
  )
}
