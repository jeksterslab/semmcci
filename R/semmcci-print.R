#' Print Method for Object of Class `semmcci`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `semmcci`.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments passed to or from other methods.
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
#' @param ... further arguments passed to or from other methods.
#' @export
print.semmcci_std <- function(x,
                              digits = 4,
                              ...) {
  cat("Standardized Monte Carlo Confidence Intervals\n")
  base::print(round(x$ci.std, digits = digits))
}
