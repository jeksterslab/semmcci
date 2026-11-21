#' Format Number
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @returns Returns a character string of formatted number.
#'
#' @param x Numeric.
#' @param digits Integer.
#'   Decimal places.
#' @param nozero Logical.
#'   If `TRUE`, no leading zeroes before the decimal point.
#'
#' @family Format Functions
#' @keywords texHelper format internal
#' @noRd
.FormatNum <- function(x,
                       digits = 2,
                       nozero = TRUE) {
  y <- trimws(
    x = format(
      x = round(
        x = x,
        digits = digits
      ),
      nsmall = digits
    )
  )
  if (nozero) {
    y <- sub(
      pattern = "^0+",
      replacement = "",
      x = y
    )
    y <- sub(
      pattern = "^-0+",
      replacement = "-",
      x = y
    )
  }
  y
}
