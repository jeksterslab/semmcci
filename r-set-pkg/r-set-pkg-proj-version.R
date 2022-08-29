################################################################################
# ====[ project packages ]======================================================
################################################################################
pkg <- c(
  "lavaan"
)
ver <- c(
  "0.6-12"
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
library(
  "remotes",
  lib.loc = dot_library_folder
)
installed <- installed.packages(lib = dot_library_folder)
pkg_installed <- installed[, "Package"]
if (length(pkg > 0)) {
  if (length(pkg_installed) == 0) {
    for (i in seq_along(pkg)) {
      install_version(
        package = pkg[i],
        version = ver[i],
        repos = "https://cran.rstudio.com/",
        lib = dot_library_folder,
        dependencies = TRUE
      )
    }
  } else {
    for (i in seq_along(pkg)) {
      run <- FALSE
      if (pkg[i] %in% pkg_installed) {
        if (installed[pkg[i], "Version"] != ver[i]) {
          run <- TRUE
        }
      } else {
        run <- TRUE
      }
      if (run) {
        install_version(
          package = pkg[i],
          version = ver[i],
          repos = "https://cran.rstudio.com/",
          lib = dot_library_folder,
          dependencies = TRUE
        )
      }
    }
  }
}
