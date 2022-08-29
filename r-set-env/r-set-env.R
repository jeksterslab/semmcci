################################################################################
# ====[ library ]================================================================
################################################################################
# The default library path should only have the default packages
# except for `rprojroot` and `remotes` initially.
# All other packages should be installed in `.library`.
library(
  "rprojroot",
  lib = ".library"
)
root <- rprojroot::is_rstudio_project
dot_library_folder <- root$find_file(
  ".library"
)
################################################################################
# ====[ tmp ]====================================================================
################################################################################
# The `detritus_folder` is the project's temporary folder.
# The `tmpdir_folder` is the `TMPDIR` R environment variable.
home_folder <- Sys.getenv("HOME")
detritus_folder <- root$find_file(
  "detritus"
)
dir.create(
  detritus_folder,
  showWarnings = FALSE,
  recursive = TRUE
)
tmpdir_folder <- root$find_file(
  file.path(
    home_folder,
    "tmp"
  )
)
dir.create(
  tmpdir_folder,
  showWarnings = FALSE,
  recursive = TRUE
)
################################################################################
# ====[ profile ]================================================================
################################################################################
dot_Rprofile_file <- root$find_file(
  ".Rprofile"
)
################################################################################
# ====[ history ]================================================================
################################################################################
dot_Rhistory_file <- root$find_file(
  ".Rhistory"
)
################################################################################
# ====[ write ]==================================================================
################################################################################
key <- c(
  "R_LIBS",
  "R_LIBS_USER",
  "TMPDIR",
  "TMP",
  "TEMP",
  "R_PROFILE_USER",
  "R_HISTFILE"
)
value <- c(
  paste0("\"", dot_library_folder, "\""),
  paste0("\"", dot_library_folder, "\""),
  paste0("\"", tmpdir_folder, "\""),
  paste0("\"", tmpdir_folder, "\""),
  paste0("\"", tmpdir_folder, "\""),
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
  "r-set-env",
  "r-set-env-secrets"
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
message(
  paste0(
    "See",
    " ",
    dot_Renviron_file,
    "."
  )
)
