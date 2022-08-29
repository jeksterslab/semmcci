################################################################################
# ====[ project packages from github ]==========================================
################################################################################
pkg <- c(
  "rstudio/tinytex"
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
for (i in seq_along(pkg)) {
  install_github(
    repo = pkg[i],
    lib = dot_library_folder,
    dependencies = TRUE
  )
}
