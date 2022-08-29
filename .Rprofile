# Options
options(menu.graphics = FALSE)
# Set repos
local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cran.rstudio.com/"
  options(repos = r)
})
.libPaths(
  ".library",
  include.site = FALSE
)
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
