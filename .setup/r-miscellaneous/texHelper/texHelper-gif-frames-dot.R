#' Split GIFs Into PNG Frames for Beamer
#'
#' Reads every `.gif` in `.setup/latex/figures/gif/` and writes its frames to
#' `.setup/latex/figures/gif-frames/<name>/frame-<k>.png`, the layout that
#' `\PSUanimation` expects. A GIF is split again only when it is newer than
#' its frames. Frames are flattened on white.
#'
#' @param path Character string. The project root.
#' @param width Integer. Maximum frame width in pixels.
#' @returns Invisibly returns a named integer vector of frame counts.
#' @keywords texHelper figure internal
#' @noRd
.GifFrames <- function(path,
                       width = 960) {
  figures <- file.path(path, ".setup", "latex", "figures")
  gif_files <- list.files(
    path = file.path(figures, "gif"),
    pattern = "\\.gif$",
    full.names = TRUE
  )
  if (
    length(x = gif_files) == 0L ||
      !requireNamespace(package = "magick", quietly = TRUE)
  ) {
    invisible(x = integer(length = 0L))
  } else {
    counts <- vapply(
      X = gif_files,
      FUN = function(gif_file) {
        name <- sub(
          pattern = "\\.gif$",
          replacement = "",
          x = basename(path = gif_file)
        )
        out_folder <- file.path(figures, "gif-frames", name)
        old <- list.files(
          path = out_folder,
          pattern = "^frame-[0-9]+\\.png$",
          full.names = TRUE
        )
        if (
          length(x = old) > 0L &&
            all(file.mtime(old) >= file.mtime(gif_file))
        ) {
          length(x = old)
        } else {
          unlink(x = old)
          dir.create(path = out_folder, showWarnings = FALSE, recursive = TRUE)
          frames <- magick::image_coalesce(
            image = magick::image_read(path = gif_file)
          )
          info <- magick::image_info(image = frames)
          if (info$width[1] > width) {
            frames <- magick::image_resize(
              image = frames,
              geometry = paste0(width, "x")
            )
          }
          frames <- magick::image_background(
            image = frames,
            color = "white",
            flatten = TRUE
          )
          n <- nrow(x = info)
          for (k in seq_len(length.out = n)) {
            magick::image_write(
              image = frames[k],
              path = file.path(out_folder, paste0("frame-", k, ".png")),
              format = "png"
            )
          }
          as.integer(x = n)
        }
      },
      FUN.VALUE = integer(length = 1L)
    )
    invisible(x = counts)
  }
}
