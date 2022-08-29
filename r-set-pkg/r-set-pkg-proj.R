################################################################################
# ====[ project packages ]======================================================
################################################################################
pkg <- c(
  "remotes"
)
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
Sys.setenv(
  R_LIBS = dot_library_folder
)
Sys.setenv(
  R_LIBS_USER = dot_library_folder
)
.libPaths(
  dot_library_folder,
  include.site = FALSE
)
# process ----------------------------------------------------------------------
installed <- installed.packages(lib = dot_library_folder)
pkg_installed <- installed[, "Package"]
for (i in seq_along(pkg)) {
  if (!(pkg[i] %in% pkg_installed)) {
    install.packages(
      pkg[i],
      repos = "https://cran.rstudio.com/",
      lib = dot_library_folder,
      dependencies = TRUE
    )
  }
}
