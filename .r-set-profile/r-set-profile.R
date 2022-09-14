#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
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
# ====[ profile ]===============================================================
################################################################################
sys <- as.character(args[2])
source(
  root$find_file(
    ".r-set-pkg",
    "r-set-pkg-repo.R"
  )
)
dot_library_folder <- root$find_file(
  ".library"
)
lib <- paste0(
  ".libPaths(",
  "\"",
  dot_library_folder,
  "\"",
  ")"
)
opt <- paste0(
  "options(menu.graphics = FALSE)",
  "\n",
  "options(",
  "repos = c(REPO_NAME = ",
  "\"",
  repos,
  "\"",
  ")",
  ")",
  "\n"
)
profile <- readLines(
  root$find_file(
    ".r-set-profile",
    "profile"
  )
)
profile <- c(
  opt,
  lib,
  profile
)
con <- file(
  root$find_file(
    ".Rprofile"
  )
)
writeLines(
  text = profile,
  con = con
)
close(con)
