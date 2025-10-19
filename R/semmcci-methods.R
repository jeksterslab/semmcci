#' Print Method for Object of Class `semmcci`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `semmcci`.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'   If `alpha = NULL`,
#'   use the argument `alpha` used in `x`.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#' @return Prints a matrix of estimates, standard errors,
#'   number of Monte Carlo replications, and confidence intervals.
#'
#' @examples
#' library(semmcci)
#' library(lavaan)
#'
#' # Data ---------------------------------------------------------------------
#' data("Tal.Or", package = "psych")
#' df <- mice::ampute(Tal.Or)$amp
#'
#' # Monte Carlo --------------------------------------------------------------
#' ## Fit Model in lavaan -----------------------------------------------------
#' model <- "
#'   reaction ~ cp * cond + b * pmi
#'   pmi ~ a * cond
#'   cond ~~ cond
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#' fit <- sem(data = df, model = model, missing = "fiml")
#'
#' ## MC() --------------------------------------------------------------------
#' unstd <- MC(
#'   fit,
#'   R = 5L # use a large value e.g., 20000L for actual research
#' )
#'
#' ## Standardized Monte Carlo ------------------------------------------------
#' std <- MCStd(unstd)
#' print(unstd)
#' print(std)
#'
#' # Monte Carlo (Multiple Imputation) ----------------------------------------
#' ## Multiple Imputation -----------------------------------------------------
#' mi <- mice::mice(
#'   data = df,
#'   print = FALSE,
#'   m = 5L, # use a large value e.g., 100L for actual research,
#'   seed = 42
#' )
#'
#' ## Fit Model in lavaan -----------------------------------------------------
#' fit <- sem(data = df, model = model) # use default listwise deletion
#'
#' ## MCMI() ------------------------------------------------------------------
#' unstd <- MCMI(
#'   fit,
#'   mi = mi,
#'   R = 5L # use a large value e.g., 20000L for actual research
#' )
#'
#' ## Standardized Monte Carlo ------------------------------------------------
#' std <- MCStd(unstd)
#' print(unstd)
#' print(std)
#'
#' @keywords methods
#' @export
print.semmcci <- function(x,
                          alpha = NULL,
                          digits = 4,
                          ...) {
  print.summary.semmcci(
    summary.semmcci(
      object = x,
      alpha = alpha,
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class `semmcci`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `semmcci`.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'   If `alpha = NULL`,
#'   use the argument `alpha` used in `object`.
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
#' df <- mice::ampute(Tal.Or)$amp
#'
#' # Monte Carlo --------------------------------------------------------------
#' ## Fit Model in lavaan -----------------------------------------------------
#' model <- "
#'   reaction ~ cp * cond + b * pmi
#'   pmi ~ a * cond
#'   cond ~~ cond
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#' fit <- sem(data = df, model = model, missing = "fiml")
#'
#' ## MC() --------------------------------------------------------------------
#' unstd <- MC(
#'   fit,
#'   R = 5L # use a large value e.g., 20000L for actual research
#' )
#'
#' ## Standardized Monte Carlo ------------------------------------------------
#' std <- MCStd(unstd)
#' summary(unstd)
#' summary(std)
#'
#' # Monte Carlo (Multiple Imputation) ----------------------------------------
#' ## Multiple Imputation -----------------------------------------------------
#' mi <- mice::mice(
#'   data = df,
#'   print = FALSE,
#'   m = 5L, # use a large value e.g., 100L for actual research,
#'   seed = 42
#' )
#'
#' ## Fit Model in lavaan -----------------------------------------------------
#' fit <- sem(data = df, model = model) # use default listwise deletion
#'
#' ## MCMI() ------------------------------------------------------------------
#' unstd <- MCMI(
#'   fit,
#'   mi = mi,
#'   R = 5L # use a large value e.g., 20000L for actual research
#' )
#'
#' ## Standardized Monte Carlo ------------------------------------------------
#' std <- MCStd(unstd)
#' summary(unstd)
#' summary(std)
#'
#' @keywords methods
#' @export
summary.semmcci <- function(object,
                            alpha = NULL,
                            digits = 4,
                            ...) {
  ci <- .MCCI(
    object = object,
    alpha = alpha
  )
  print_summary <- round(
    x = ci,
    digits = digits
  )
  attr(
    x = ci,
    which = "fit"
  ) <- object
  attr(
    x = ci,
    which = "print_summary"
  ) <- print_summary
  attr(
    x = ci,
    which = "alpha"
  ) <- alpha
  attr(
    x = ci,
    which = "digits"
  ) <- digits
  class(ci) <- "summary.semmcci"
  ci
}

#' @noRd
#' @keywords internal
#' @exportS3Method print summary.semmcci
print.summary.semmcci <- function(x,
                                  ...) {
  print_summary <- attr(
    x = x,
    which = "print_summary"
  )
  object <- attr(
    x = x,
    which = "fit"
  )
  if (object$fun == "MC") {
    cat("Monte Carlo Confidence Intervals\n")
  }
  if (object$fun == "MCMI") {
    cat("Monte Carlo Confidence Intervals (Multiple Imputation Estimates)\n")
  }
  if (object$fun == "MCStd") {
    cat("Standardized Monte Carlo Confidence Intervals\n")
  }
  print(print_summary)
  invisible(x)
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
#' df <- mice::ampute(Tal.Or)$amp
#'
#' # Monte Carlo --------------------------------------------------------------
#' ## Fit Model in lavaan -----------------------------------------------------
#' model <- "
#'   reaction ~ cp * cond + b * pmi
#'   pmi ~ a * cond
#'   cond ~~ cond
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#' fit <- sem(data = df, model = model, missing = "fiml")
#'
#' ## MC() --------------------------------------------------------------------
#' unstd <- MC(
#'   fit,
#'   R = 5L # use a large value e.g., 20000L for actual research
#' )
#'
#' ## Standardized Monte Carlo ------------------------------------------------
#' std <- MCStd(unstd)
#' coef(unstd)
#' coef(std)
#'
#' # Monte Carlo (Multiple Imputation) ----------------------------------------
#' ## Multiple Imputation -----------------------------------------------------
#' mi <- mice::mice(
#'   data = df,
#'   print = FALSE,
#'   m = 5L, # use a large value e.g., 100L for actual research,
#'   seed = 42
#' )
#'
#' ## Fit Model in lavaan -----------------------------------------------------
#' fit <- sem(data = df, model = model) # use default listwise deletion
#'
#' ## MCMI() ------------------------------------------------------------------
#' unstd <- MCMI(
#'   fit,
#'   mi = mi,
#'   R = 5L # use a large value e.g., 20000L for actual research
#' )
#'
#' ## Standardized Monte Carlo ------------------------------------------------
#' std <- MCStd(unstd)
#' coef(unstd)
#' coef(std)
#'
#' @keywords methods
#' @export
coef.semmcci <- function(object,
                         ...) {
  object$thetahat$est
}

#' Sampling Covariance Matrix of the Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `semmcci`.
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
#' df <- mice::ampute(Tal.Or)$amp
#'
#' # Monte Carlo --------------------------------------------------------------
#' ## Fit Model in lavaan -----------------------------------------------------
#' model <- "
#'   reaction ~ cp * cond + b * pmi
#'   pmi ~ a * cond
#'   cond ~~ cond
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#' fit <- sem(data = df, model = model, missing = "fiml")
#'
#' ## MC() --------------------------------------------------------------------
#' unstd <- MC(
#'   fit,
#'   R = 5L # use a large value e.g., 20000L for actual research
#' )
#'
#' ## Standardized Monte Carlo ------------------------------------------------
#' std <- MCStd(unstd)
#' vcov(unstd)
#' vcov(std)
#'
#' # Monte Carlo (Multiple Imputation) ----------------------------------------
#' ## Multiple Imputation -----------------------------------------------------
#' mi <- mice::mice(
#'   data = df,
#'   print = FALSE,
#'   m = 5L, # use a large value e.g., 100L for actual research,
#'   seed = 42
#' )
#'
#' ## Fit Model in lavaan -----------------------------------------------------
#' fit <- sem(data = df, model = model) # use default listwise deletion
#'
#' ## MCMI() ------------------------------------------------------------------
#' unstd <- MCMI(
#'   fit,
#'   mi = mi,
#'   R = 5L # use a large value e.g., 20000L for actual research
#' )
#'
#' ## Standardized Monte Carlo ------------------------------------------------
#' std <- MCStd(unstd)
#' vcov(unstd)
#' vcov(std)
#'
#' @keywords methods
#' @export
vcov.semmcci <- function(object,
                         ...) {
  stats::var(
    object$thetahatstar
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
#' df <- mice::ampute(Tal.Or)$amp
#'
#' # Monte Carlo --------------------------------------------------------------
#' ## Fit Model in lavaan -----------------------------------------------------
#' model <- "
#'   reaction ~ cp * cond + b * pmi
#'   pmi ~ a * cond
#'   cond ~~ cond
#'   indirect := a * b
#'   direct := cp
#'   total := cp + (a * b)
#' "
#' fit <- sem(data = df, model = model, missing = "fiml")
#'
#' ## MC() --------------------------------------------------------------------
#' unstd <- MC(
#'   fit,
#'   R = 5L # use a large value e.g., 20000L for actual research
#' )
#'
#' ## Standardized Monte Carlo ------------------------------------------------
#' std <- MCStd(unstd)
#' confint(unstd)
#' confint(std)
#'
#' # Monte Carlo (Multiple Imputation) ----------------------------------------
#' ## Multiple Imputation -----------------------------------------------------
#' mi <- mice::mice(
#'   data = df,
#'   print = FALSE,
#'   m = 5L, # use a large value e.g., 100L for actual research,
#'   seed = 42
#' )
#'
#' ## Fit Model in lavaan -----------------------------------------------------
#' fit <- sem(data = df, model = model) # use default listwise deletion
#'
#' ## MCMI() ------------------------------------------------------------------
#' unstd <- MCMI(
#'   fit,
#'   mi = mi,
#'   R = 5L # use a large value e.g., 20000L for actual research
#' )
#'
#' ## Standardized Monte Carlo ------------------------------------------------
#' std <- MCStd(unstd)
#' confint(unstd)
#' confint(std)
#'
#' @keywords methods
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
    parameters <- rownames(
      ci
    )
    if (!is.null(parameters)) {
      parm <- parameters
    } else {
      parm <- seq_len(dim(ci)[1])
    }
  }
  ci <- ci[parm, 4:5, drop = FALSE]
  varnames <- colnames(ci)
  varnames <- gsub(
    pattern = "%",
    replacement = " %",
    x = varnames
  )
  colnames(ci) <- varnames
  ci
}
