#' Print Method for Object of Class `semmcci`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `semmcci`.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#' @return Returns a matrix of estimates, standard errors,
#'   number of Monte Carlo replications, and confidence intervals.
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Generate Data ------------------------------------------------------------
#' n <- 1000
#' a <- 0.50
#' b <- 0.50
#' cp <- 0.25
#' s2_em <- 1 - a^2
#' s2_ey <- 1 - cp^2 - a^2 * b^2 - b^2 * s2_em - 2 * cp * a * b
#' em <- rnorm(n = n, mean = 0, sd = sqrt(s2_em))
#' ey <- rnorm(n = n, mean = 0, sd = sqrt(s2_ey))
#' X <- rnorm(n = n)
#' M <- a * X + em
#' Y <- cp * X + b * M + ey
#' df <- data.frame(X, M, Y)
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   Y ~ cp * X + b * M
#'   M ~ a * X
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#' fit <- sem(data = df, model = model)
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
#' @return Returns a matrix of estimates, standard errors,
#'   number of Monte Carlo replications, and confidence intervals.
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Generate Data ------------------------------------------------------------
#' n <- 1000
#' a <- 0.50
#' b <- 0.50
#' cp <- 0.25
#' s2_em <- 1 - a^2
#' s2_ey <- 1 - cp^2 - a^2 * b^2 - b^2 * s2_em - 2 * cp * a * b
#' em <- rnorm(n = n, mean = 0, sd = sqrt(s2_em))
#' ey <- rnorm(n = n, mean = 0, sd = sqrt(s2_ey))
#' X <- rnorm(n = n)
#' M <- a * X + em
#' Y <- cp * X + b * M + ey
#' df <- data.frame(X, M, Y)
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   Y ~ cp * X + b * M
#'   M ~ a * X
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#' fit <- sem(data = df, model = model, fixed.x = FALSE)
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
#' @return Returns a matrix of estimates, standard errors,
#'   number of Monte Carlo replications, and confidence intervals.
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Generate Data ------------------------------------------------------------
#' n <- 1000
#' a <- 0.50
#' b <- 0.50
#' cp <- 0.25
#' s2_em <- 1 - a^2
#' s2_ey <- 1 - cp^2 - a^2 * b^2 - b^2 * s2_em - 2 * cp * a * b
#' em <- rnorm(n = n, mean = 0, sd = sqrt(s2_em))
#' ey <- rnorm(n = n, mean = 0, sd = sqrt(s2_ey))
#' X <- rnorm(n = n)
#' M <- a * X + em
#' Y <- cp * X + b * M + ey
#' df <- data.frame(X, M, Y)
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   Y ~ cp * X + b * M
#'   M ~ a * X
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#' fit <- sem(data = df, model = model)
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
#' @return Returns a matrix of estimates, standard errors,
#'   number of Monte Carlo replications, and confidence intervals.
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Generate Data ------------------------------------------------------------
#' n <- 1000
#' a <- 0.50
#' b <- 0.50
#' cp <- 0.25
#' s2_em <- 1 - a^2
#' s2_ey <- 1 - cp^2 - a^2 * b^2 - b^2 * s2_em - 2 * cp * a * b
#' em <- rnorm(n = n, mean = 0, sd = sqrt(s2_em))
#' ey <- rnorm(n = n, mean = 0, sd = sqrt(s2_ey))
#' X <- rnorm(n = n)
#' M <- a * X + em
#' Y <- cp * X + b * M + ey
#' df <- data.frame(X, M, Y)
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   Y ~ cp * X + b * M
#'   M ~ a * X
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#' fit <- sem(data = df, model = model, fixed.x = FALSE)
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
#' a <- 0.50
#' b <- 0.50
#' cp <- 0.25
#' s2_em <- 1 - a^2
#' s2_ey <- 1 - cp^2 - a^2 * b^2 - b^2 * s2_em - 2 * cp * a * b
#' em <- rnorm(n = n, mean = 0, sd = sqrt(s2_em))
#' ey <- rnorm(n = n, mean = 0, sd = sqrt(s2_ey))
#' X <- rnorm(n = n)
#' M <- a * X + em
#' Y <- cp * X + b * M + ey
#' df <- data.frame(X, M, Y)
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   Y ~ cp * X + b * M
#'   M ~ a * X
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#' fit <- sem(data = df, model = model)
#'
#' # Monte Carlo --------------------------------------------------------------
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = c(0.001, 0.01, 0.05)
#' )
#' coef(unstd)
#' @keywords method
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
#' a <- 0.50
#' b <- 0.50
#' cp <- 0.25
#' s2_em <- 1 - a^2
#' s2_ey <- 1 - cp^2 - a^2 * b^2 - b^2 * s2_em - 2 * cp * a * b
#' em <- rnorm(n = n, mean = 0, sd = sqrt(s2_em))
#' ey <- rnorm(n = n, mean = 0, sd = sqrt(s2_ey))
#' X <- rnorm(n = n)
#' M <- a * X + em
#' Y <- cp * X + b * M + ey
#' df <- data.frame(X, M, Y)
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   Y ~ cp * X + b * M
#'   M ~ a * X
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#' fit <- sem(data = df, model = model, fixed.x = FALSE)
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
#' @return Returns a matrix of the variance-covariance matrix
#'   of parameter estimates.
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Generate Data ------------------------------------------------------------
#' n <- 1000
#' a <- 0.50
#' b <- 0.50
#' cp <- 0.25
#' s2_em <- 1 - a^2
#' s2_ey <- 1 - cp^2 - a^2 * b^2 - b^2 * s2_em - 2 * cp * a * b
#' em <- rnorm(n = n, mean = 0, sd = sqrt(s2_em))
#' ey <- rnorm(n = n, mean = 0, sd = sqrt(s2_ey))
#' X <- rnorm(n = n)
#' M <- a * X + em
#' Y <- cp * X + b * M + ey
#' df <- data.frame(X, M, Y)
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   Y ~ cp * X + b * M
#'   M ~ a * X
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#' fit <- sem(data = df, model = model)
#'
#' # Monte Carlo --------------------------------------------------------------
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = c(0.001, 0.01, 0.05)
#' )
#' vcov(unstd)
#' @keywords method
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
#' @return Returns a matrix of the variance-covariance matrix
#'   of standardized parameter estimates.
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Generate Data ------------------------------------------------------------
#' n <- 1000
#' a <- 0.50
#' b <- 0.50
#' cp <- 0.25
#' s2_em <- 1 - a^2
#' s2_ey <- 1 - cp^2 - a^2 * b^2 - b^2 * s2_em - 2 * cp * a * b
#' em <- rnorm(n = n, mean = 0, sd = sqrt(s2_em))
#' ey <- rnorm(n = n, mean = 0, sd = sqrt(s2_ey))
#' X <- rnorm(n = n)
#' M <- a * X + em
#' Y <- cp * X + b * M + ey
#' df <- data.frame(X, M, Y)
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   Y ~ cp * X + b * M
#'   M ~ a * X
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#' fit <- sem(data = df, model = model, fixed.x = FALSE)
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
#' @param parm a specification of which parameters
#'   are to be given confidence intervals,
#'   either a vector of numbers or a vector of names.
#'   If missing, all parameters are considered.
#' @param level the confidence level required.
#' @return Returns a matrix of confidence intervals.
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Generate Data ------------------------------------------------------------
#' n <- 1000
#' a <- 0.50
#' b <- 0.50
#' cp <- 0.25
#' s2_em <- 1 - a^2
#' s2_ey <- 1 - cp^2 - a^2 * b^2 - b^2 * s2_em - 2 * cp * a * b
#' em <- rnorm(n = n, mean = 0, sd = sqrt(s2_em))
#' ey <- rnorm(n = n, mean = 0, sd = sqrt(s2_ey))
#' X <- rnorm(n = n)
#' M <- a * X + em
#' Y <- cp * X + b * M + ey
#' df <- data.frame(X, M, Y)
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   Y ~ cp * X + b * M
#'   M ~ a * X
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#' fit <- sem(data = df, model = model)
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
#' @param parm a specification of which parameters
#'   are to be given confidence intervals,
#'   either a vector of numbers or a vector of names.
#'   If missing, all parameters are considered.
#' @param level the confidence level required.
#' @return Returns a matrix of confidence intervals.
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Generate Data ------------------------------------------------------------
#' n <- 1000
#' a <- 0.50
#' b <- 0.50
#' cp <- 0.25
#' s2_em <- 1 - a^2
#' s2_ey <- 1 - cp^2 - a^2 * b^2 - b^2 * s2_em - 2 * cp * a * b
#' em <- rnorm(n = n, mean = 0, sd = sqrt(s2_em))
#' ey <- rnorm(n = n, mean = 0, sd = sqrt(s2_ey))
#' X <- rnorm(n = n)
#' M <- a * X + em
#' Y <- cp * X + b * M + ey
#' df <- data.frame(X, M, Y)
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   Y ~ cp * X + b * M
#'   M ~ a * X
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#' fit <- sem(data = df, model = model, fixed.x = FALSE)
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
