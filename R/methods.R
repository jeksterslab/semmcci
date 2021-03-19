#' Print Value
#'
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @export
print <- function(x,
                  ...) {
  UseMethod("print")
}

#' Print Value
#'
#' @param x an object used to select a method.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments passed to or from other methods.
#' @export
print.mc <- function(x,
                     digits = 4,
                     ...) {
  cat("Monte Carlo Confidence Intervals\n")
  base::print(round(x$ci, digits = digits))
}

#' Print Value
#'
#' @param x an object used to select a method.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments passed to or from other methods.
#' @export
print.std <- function(x,
                      digits = 4,
                      ...) {
  cat("Standardized Monte Carlo Confidence Intervals\n")
  base::print(round(x$ci.std, digits = digits))
}
