#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
################################################################################
# ====[ library ]===============================================================
################################################################################
dot_library_folder <- file.path(
  as.character(args[1]),
  ".library"
)
dir.create(
  dot_library_folder,
  showWarnings = FALSE,
  recursive = TRUE
)
.libPaths(
  dot_library_folder
)
Sys.setenv(
  R_LIBS_USER = dot_library_folder
)
################################################################################
# ====[ rprojroot ]=============================================================
################################################################################
installed <- installed.packages()
pkg_installed <- installed[, "Package"]
if (!("rprojroot" %in% pkg_installed)) {
  install.packages(
    "rprojroot",
    repos = c(REPO_NAME = "https://packagemanager.rstudio.com/all/latest"),
    lib = dot_library_folder
  )
}
root <- rprojroot::is_rstudio_project
bin_dot_r <- root$find_file(
  ".bin",
  "bin.R"
)
x <- list.files(
  path = root$find_file(
    ".bin"
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
