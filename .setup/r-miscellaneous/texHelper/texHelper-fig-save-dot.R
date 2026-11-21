#' Save a Figure as PDF and PNG
#'
#' Vector PDF for papers and slides, and PNG at 300 dpi for the web, from one
#' source, in `.setup/latex/figures/pdf/` and `.setup/latex/figures/png/`.
#'
#' @param plot A ggplot2 plot.
#' @param name Character string. The file name, without an extension.
#' @param size Character string. See `.FigSize()`.
#' @param height Numeric. See `.FigSize()`.
#' @param path Character string. The project root.
#' @returns Invisibly returns the paths of the two files.
#' @keywords texHelper figure internal
#' @noRd
.FigSave <- function(plot,
                     name,
                     size = "page",
                     height = NULL,
                     path = ".") {
  preset <- .FigSize(size = size, height = height)
  files <- c(
    pdf = file.path(
      path, ".setup", "latex", "figures", "pdf", paste0(name, ".pdf")
    ),
    png = file.path(
      path, ".setup", "latex", "figures", "png", paste0(name, ".png")
    )
  )
  for (file in files) {
    dir.create(
      path = dirname(path = file),
      showWarnings = FALSE,
      recursive = TRUE
    )
    ggplot2::ggsave(
      filename = file,
      plot = plot,
      width = preset$width,
      height = preset$height,
      units = "in",
      dpi = 300
    )
  }
  invisible(x = files)
}
