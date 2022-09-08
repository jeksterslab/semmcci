#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
################################################################################
# ====[ project packages ]======================================================
################################################################################
pkg <- c(
  "MASS",
  "lavaan"
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
################################################################################
# ====[ install ]===============================================================
################################################################################
sys <- as.character(args[2])
source(
  root$find_file(
    ".r-set-pkg",
    "r-set-pkg-repo.R"
  )
)
for (i in seq_along(pkg)) {
  if (!(pkg[i] %in% pkg_installed)) {
    install.packages(
      pkg[i],
      repos = repos,
      lib = dot_library_folder
    )
  }
}
