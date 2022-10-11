#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
################################################################################
# ====[ project packages ]======================================================
################################################################################
pkg <- c(
  "rstudio/tinytex",
  "jeksterslab/semmcci"
)
################################################################################
# ====[ library ]================================================================
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
# ====[ remotes ]===============================================================
################################################################################
installed <- installed.packages()
pkg_installed <- installed[, "Package"]
if (!("remotes" %in% pkg_installed)) {
  install.packages(
    "remotes",
    repos = c(REPO_NAME = "https://packagemanager.rstudio.com/all/latest"),
    lib = dot_library_folder
  )
}
################################################################################
# ====[ install ]===============================================================
################################################################################
for (i in seq_along(pkg)) {
  remotes::install_github(
    repo = pkg[i],
    lib = dot_library_folder
  )
}
