#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
################################################################################
# ====[ library ]================================================================
################################################################################
dot_library_folder <- file.path(
  file.path(
    as.character(args[1])
  ),
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
# ====[ tmp ]====================================================================
################################################################################
# The `detritus_folder` is the project's temporary folder.
# The `tmpdir_folder` is the `TMPDIR` R environment variable.
root_folder <- basename(
  dirname(
    root$find_file(
      "project.Rproj"
    )
  )
)
home_folder <- Sys.getenv("HOME")
detritus_folder <- root$find_file(
  "detritus"
)
dir.create(
  detritus_folder,
  showWarnings = FALSE,
  recursive = TRUE
)
# tmpdir_folder <- root$find_file(
#  home_folder,
#  "tmp",
#  root_folder
# )
# dir.create(
#  tmpdir_folder,
#  showWarnings = FALSE,
#  recursive = TRUE
# )
# Sys.setenv(TMPDIR = tmpdir_folder)
# Sys.setenv(TEMP = tmpdir_folder)
# Sys.setenv(TMP = tmpdir_folder)
################################################################################
# ====[ profile ]================================================================
################################################################################
dot_Rprofile_file <- root$find_file(
  ".Rprofile"
)
Sys.setenv(R_PROFILE_USER = dot_Rprofile_file)
################################################################################
# ====[ history ]================================================================
################################################################################
dot_Rhistory_file <- root$find_file(
  ".Rhistory"
)
Sys.setenv(R_HISTFILE = dot_Rhistory_file)
################################################################################
# ====[ write ]==================================================================
################################################################################
key <- c(
  "R_LIBS_USER",
  #  "TMPDIR",
  #  "TMP",
  #  "TEMP",
  "R_PROFILE_USER",
  "R_HISTFILE"
)
value <- c(
  paste0("\"", dot_library_folder, "\""),
  #  paste0("\"", tmpdir_folder, "\""),
  #  paste0("\"", tmpdir_folder, "\""),
  #  paste0("\"", tmpdir_folder, "\""),
  paste0("\"", dot_Rprofile_file, "\""),
  paste0("\"", dot_Rhistory_file, "\"")
)
dot_Renviron_contents <- paste(
  key,
  value,
  sep = "="
)
dot_Renviron_contents <- c(
  dot_Renviron_contents,
  "R_COMPLETION=TRUE"
)
################################################################################
# ====[ secrets ]================================================================
################################################################################
r_env_secrets <- root$find_file(
  ".r-set-env",
  "r-set-env-secrets"
)
source(
  root$find_file(
    ".r-set-env",
    "r-set-env-secrets.R"
  )
)
if (file.exists(r_env_secrets)) {
  dot_Renviron_contents <- c(
    dot_Renviron_contents,
    readLines(r_env_secrets)
  )
}
################################################################################
# ====[ write file ]=============================================================
################################################################################
dot_Renviron_file <- root$find_file(
  ".Renviron"
)
con <- file(dot_Renviron_file)
writeLines(
  text = dot_Renviron_contents,
  con = con
)
close(con)
