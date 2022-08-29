root <- rprojroot::is_rstudio_project
bin_dot_r <- root$find_file(
  "bin",
  "bin.R"
)
x <- list.files(
  path = root$find_file(
    "bin"
  ),
  pattern = "\\.R$",
  full.names = TRUE,
  all.files = TRUE,
  recursive = TRUE
)
x <- x[!(x %in% bin_dot_r)]
if (length(x) > 0) {
  invisible(
    lapply(
      X = x,
      FUN = source
    )
  )
}
