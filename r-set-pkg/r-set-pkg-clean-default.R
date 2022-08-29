################################################################################
# ====[ clean libraries except for .library]====================================
################################################################################
## find root -------------------------------------------------------------------
library(
  "rprojroot",
  lib = ".library"
)
root <- rprojroot::is_rstudio_project
# set library
dot_library_folder <- root$find_file(
  ".library"
)
# process ----------------------------------------------------------------------
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
