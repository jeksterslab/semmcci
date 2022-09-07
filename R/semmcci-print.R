#' Print Method for Object of Class `semmcci`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `semmcci`.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#' @return Returns a matrix of estimates, standard errors and confidence intervals.
#' @keywords method
#' @export
print.semmcci <- function(x,
                          digits = 4,
                          ...) {
  cat("Monte Carlo Confidence Intervals\n")
  base::print(round(x$ci, digits = digits))
}

#' Print Method for Object of Class `semmcci_std`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `semmcci_std`.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#' @return Returns a matrix of estimates, standard errors and confidence intervals.
#' @keywords method
#' @export
print.semmcci_std <- function(x,
                              digits = 4,
                              ...) {
  cat("Standardized Monte Carlo Confidence Intervals\n")
  base::print(round(x$ci.std, digits = digits))
}
