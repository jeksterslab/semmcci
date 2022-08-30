options(menu.graphics = FALSE)
options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/all/__linux__/focal/latest"))

.libPaths("/cloud/project/.library")
.First <- function() {
  if (interactive()) {
    options(
      prompt = paste0(
        "[",
        basename(getwd()),
        "] R> "
      )
    )
    message(
      paste(
        R.version.string,
        Sys.time(),
        getwd(),
        sep = " | "
      )
    )
  }
}
.Last <- function() {
  if (interactive()) {
    try(
      savehistory(
        file = file.path(getwd(), ".Rhistory")
      )
    )
  }
}
