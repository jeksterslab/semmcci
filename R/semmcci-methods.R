#' Print Method for Object of Class `semmcci`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `semmcci`.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#' @return Returns a matrix of estimates, standard errors,
#'   number of Monte Carlo replications, and confidence intervals.
#'
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Data ---------------------------------------------------------------------
#' data("Tal.Or", package = "psych")
#' df <- Tal.Or
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   reaction ~ cp * cond + b * pmi
#'   pmi ~ a * cond
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#'
#' fit <- sem(data = df, model = model)
#'
#' # Monte Carlo --------------------------------------------------------------
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = 0.05
#' )
#' print(unstd)
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
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
#'
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Data ---------------------------------------------------------------------
#' data("Tal.Or", package = "psych")
#' df <- Tal.Or
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   reaction ~ cp * cond + b * pmi
#'   pmi ~ a * cond
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#'
#' fit <- sem(data = df, model = model, fixed.x = FALSE)
#'
#' # Monte Carlo --------------------------------------------------------------
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = 0.05
#' )
#'
#' # Standardized Monte Carlo -------------------------------------------------
#' std <- MCStd(unstd, alpha = 0.05)
#' print(std)
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
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
#'
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Data ---------------------------------------------------------------------
#' data("Tal.Or", package = "psych")
#' df <- Tal.Or
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   reaction ~ cp * cond + b * pmi
#'   pmi ~ a * cond
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#'
#' fit <- sem(data = df, model = model)
#'
#' # Monte Carlo --------------------------------------------------------------
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = 0.05
#' )
#' summary(unstd)
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
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
#'
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Data ---------------------------------------------------------------------
#' data("Tal.Or", package = "psych")
#' df <- Tal.Or
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   reaction ~ cp * cond + b * pmi
#'   pmi ~ a * cond
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#'
#' fit <- sem(data = df, model = model, fixed.x = FALSE)
#'
#' # Monte Carlo --------------------------------------------------------------
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = 0.05
#' )
#'
#' # Standardized Monte Carlo -------------------------------------------------
#' std <- MCStd(unstd, alpha = 0.05)
#' summary(std)
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
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
#'
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Data ---------------------------------------------------------------------
#' data("Tal.Or", package = "psych")
#' df <- Tal.Or
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   reaction ~ cp * cond + b * pmi
#'   pmi ~ a * cond
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#'
#' fit <- sem(data = df, model = model)
#'
#' # Monte Carlo --------------------------------------------------------------
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = 0.05
#' )
#' coef(unstd)
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
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
#'
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Data ---------------------------------------------------------------------
#' data("Tal.Or", package = "psych")
#' df <- Tal.Or
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   reaction ~ cp * cond + b * pmi
#'   pmi ~ a * cond
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#'
#' fit <- sem(data = df, model = model, fixed.x = FALSE)
#'
#' # Monte Carlo --------------------------------------------------------------
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = 0.05
#' )
#'
#' # Standardized Monte Carlo -------------------------------------------------
#' std <- MCStd(unstd, alpha = 0.05)
#' coef(std)
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
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
#'
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Data ---------------------------------------------------------------------
#' data("Tal.Or", package = "psych")
#' df <- Tal.Or
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   reaction ~ cp * cond + b * pmi
#'   pmi ~ a * cond
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#'
#' fit <- sem(data = df, model = model)
#'
#' # Monte Carlo --------------------------------------------------------------
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = 0.05
#' )
#' vcov(unstd)
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
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
#'
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Data ---------------------------------------------------------------------
#' data("Tal.Or", package = "psych")
#' df <- Tal.Or
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   reaction ~ cp * cond + b * pmi
#'   pmi ~ a * cond
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#'
#' fit <- sem(data = df, model = model, fixed.x = FALSE)
#'
#' # Monte Carlo --------------------------------------------------------------
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = 0.05
#' )
#'
#' # Standardized Monte Carlo -------------------------------------------------
#' std <- MCStd(unstd, alpha = 0.05)
#' vcov(std)
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
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
#'
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Data ---------------------------------------------------------------------
#' data("Tal.Or", package = "psych")
#' df <- Tal.Or
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   reaction ~ cp * cond + b * pmi
#'   pmi ~ a * cond
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#'
#' fit <- sem(data = df, model = model)
#'
#' # Monte Carlo --------------------------------------------------------------
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = 0.05
#' )
#' confint(unstd)
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
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
#'
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Data ---------------------------------------------------------------------
#' data("Tal.Or", package = "psych")
#' df <- Tal.Or
#'
#' # Fit Model in lavaan ------------------------------------------------------
#' model <- "
#'   reaction ~ cp * cond + b * pmi
#'   pmi ~ a * cond
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#'
#' fit <- sem(data = df, model = model, fixed.x = FALSE)
#'
#' # Monte Carlo --------------------------------------------------------------
#' unstd <- MC(
#'   fit,
#'   R = 100L, # use a large value e.g., 20000L for actual research
#'   alpha = 0.05
#' )
#'
#' # Standardized Monte Carlo -------------------------------------------------
#' std <- MCStd(unstd, alpha = 0.05)
#' confint(std)
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
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
