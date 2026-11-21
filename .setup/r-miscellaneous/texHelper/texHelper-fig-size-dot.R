#' Figure Size Presets
#'
#' Letter paper with 1-inch margins: `"column"` is the `jou` column, `"page"`
#' the text width, and `"slide"` the 16:9 frame (larger sans-serif font).
#'
#' @param size Character string. `"column"`, `"page"`, or `"slide"`.
#' @param height Numeric. Overrides the preset's height, in inches.
#' @returns Returns a list: width, height, family, and base_size.
#' @keywords texHelper figure internal
#' @noRd
.FigSize <- function(size = "page",
                     height = NULL) {
  presets <- list(
    column = list(width = 3.3, height = 2.5, family = "serif", base_size = 9),
    page = list(width = 6.5, height = 4, family = "serif", base_size = 10),
    slide = list(width = 5.5, height = 3.1, family = "sans", base_size = 12)
  )
  out <- presets[[match.arg(arg = size, choices = names(x = presets))]]
  if (!is.null(x = height)) {
    out$height <- height
  }
  out
}
