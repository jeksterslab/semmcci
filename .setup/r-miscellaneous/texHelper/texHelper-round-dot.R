#' Round to Specified Number of Digits
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @returns Returns a character string.
#'
#' @param x Numeric vector.
#' @param digits Digits for numeric values.
#'
#' @family Rounding Functions
#' @keywords texHelper round internal
#' @noRd
.Round <- function(x,
                   digits = 2) {
  format(
    x = round(
      x = x,
      digits = digits
    ),
    nsmall = digits
  )
}
