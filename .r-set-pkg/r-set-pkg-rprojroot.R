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
