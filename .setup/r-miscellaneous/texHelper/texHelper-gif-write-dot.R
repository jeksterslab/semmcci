#' Write an Animated Figure
#'
#' Calls a figure function once per frame. One source gives the GIF
#' (pkgdown, Quarto, README, Reveal.js), the PNG frames (`\PSUanimation`),
#' and a multi-page PDF (`animate`, `\PSUanimationpdf`), in
#' `.setup/latex/figures/`. Papers also show chosen frames as small
#' multiples.
#'
#' @param fig Function. Takes `frame` and `size`, and returns a ggplot2 plot.
#' @param frames Vector. The values of `frame`.
#' @param name Character string. The file name, without an extension.
#' @param fps Numeric. Frames per second: 2, 4, 5, or 10.
#' @param size Character string. See `.FigSize()`.
#' @param path Character string. The project root.
#' @returns Invisibly returns the path of the GIF.
#' @keywords texHelper figure internal
#' @noRd
.GifWrite <- function(fig,
                      frames,
                      name,
                      fps = 5,
                      size = "slide",
                      path = ".") {
  # magick needs a factor of 100; over 3 per second is fine for motion, but a
  # figure must not flash (the caption says what changes)
  stopifnot(fps %in% c(2, 4, 5, 10))
  preset <- .FigSize(size = size)
  figures <- file.path(path, ".setup", "latex", "figures")
  frame_folder <- file.path(figures, "gif-frames", name)
  gif_folder <- file.path(figures, "gif")
  dir.create(path = frame_folder, showWarnings = FALSE, recursive = TRUE)
  dir.create(path = gif_folder, showWarnings = FALSE, recursive = TRUE)
  unlink(x = list.files(path = frame_folder, full.names = TRUE))
  plots <- lapply(X = frames, FUN = function(frame) {
    fig(frame = frame, size = size)
  })
  pngs <- file.path(
    frame_folder,
    paste0("frame-", seq_along(along.with = plots), ".png")
  )
  for (i in seq_along(along.with = plots)) {
    ggplot2::ggsave(
      filename = pngs[i],
      plot = plots[[i]],
      width = preset$width,
      height = preset$height,
      units = "in",
      dpi = 300
    )
  }
  grDevices::pdf(
    file = file.path(gif_folder, paste0(name, ".pdf")),
    width = preset$width,
    height = preset$height,
    onefile = TRUE
  )
  for (plot in plots) {
    print(x = plot)
  }
  grDevices::dev.off()
  gif <- file.path(gif_folder, paste0(name, ".gif"))
  magick::image_write(
    image = magick::image_animate(
      image = magick::image_scale(
        image = magick::image_read(path = pngs),
        geometry = "960x"
      ),
      fps = fps,
      loop = 0
    ),
    path = gif
  )
  if (file.size(gif) > 10 * 1024^2) {
    warning(gif, " is over 10 MB.", call. = FALSE)
  }
  invisible(x = gif)
}
