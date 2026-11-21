#' semmcci: Monte Carlo Confidence Intervals in Structural Equation Modeling
#'
#' Generates Monte Carlo confidence intervals for free and defined parameters
#' in models fitted in the structural equation modeling package `lavaan`
#' (Pesigan & Cheung, 2024).
#' A sampling distribution of the parameter estimates is generated
#' from the multivariate normal distribution
#' using the parameter estimates and their sampling covariance matrix.
#' Confidence limits are the percentiles of the generated distribution
#' (Preacher & Selig, 2012; MacKinnon et al., 2004).
#'
#' @section Models fitted in `lavaan`:
#' [MC()] generates Monte Carlo confidence intervals
#' from a fitted `lavaan` model.
#' [MCMI()] does the same for a `lavaan` model fitted to multiply imputed data.
#' [MCStd()] generates Monte Carlo confidence intervals
#' for the standardized estimates
#' from the output of [MC()] or [MCMI()].
#'
#' @section Other models:
#' [MCGeneric()] and [MCFunc()] generate Monte Carlo confidence intervals
#' for functions of parameters
#' when the estimates and their sampling covariance matrix are supplied.
#' [Func()] does the same for a list of parameters.
#'
#' @section Methods:
#' The `coef()`, `vcov()`, `confint()`, `summary()`, and `print()` methods are
#' available for objects of class `semmcci`.
#'
#' @references
#' MacKinnon, D. P., Lockwood, C. M., & Williams, J. (2004).
#' Confidence limits for the indirect effect:
#' Distribution of the product and resampling methods.
#' *Multivariate Behavioral Research*, *39*(1), 99--128.
#' \doi{10.1207/s15327906mbr3901_4}
#'
#' Pesigan, I. J. A., & Cheung, S. F. (2024).
#' Monte Carlo confidence intervals for the indirect effect
#' with missing data.
#' *Behavior Research Methods*.
#' \doi{10.3758/s13428-023-02114-4}
#'
#' Preacher, K. J., & Selig, J. P. (2012).
#' Advantages of Monte Carlo confidence intervals for indirect effects.
#' *Communication Methods and Measures*, *6*(2), 77--98.
#' \doi{10.1080/19312458.2012.679848}
#'
#' @aliases semmcci-package
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib semmcci, .registration = TRUE
## usethis namespace: end
NULL
