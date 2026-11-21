#' The Okabe-Ito Palette
#'
#' Colour-blind safe; pair each colour with a point shape and a line type, so
#' that figures read in greyscale.
#'
#' @returns Returns a character vector of colours, without black.
#' @keywords texHelper figure internal
#' @noRd
.OkabeIto <- function() {
  unname(obj = grDevices::palette.colors(palette = "Okabe-Ito"))[-1L]
}
