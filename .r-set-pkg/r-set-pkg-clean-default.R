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
# ====[ clean libraries except for .library]====================================
################################################################################
# force clean the libraries other than `.library`
# this enforces the practice of installing packages only on `.library`
other_libs <- .libPaths()
for (i in seq_along(other_libs)) {
  if (other_libs[i] == dot_library_folder) {
    other_libs[i] <- NA
  }
}
other_libs <- other_libs[complete.cases(other_libs)]
for (i in seq_along(other_libs)) {
  pkgs_i <- installed.packages(
    priority = "NA",
    lib = other_libs[i]
  )
  pkgs_i <- pkgs_i[, "Package"]
  if (length(pkgs_i) != 0) {
    remove.packages(
      pkgs = pkgs_i,
      lib = other_libs[i]
    )
  }
}
