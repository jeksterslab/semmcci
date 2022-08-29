# find root directory
root <- rprojroot::is_rstudio_project
# root_folder
root_folder <- basename(
  dirname(
    root$find_file(
      "project.Rproj"
    )
  )
)
# source R scripts in r-dependencies and R
invisible(
  lapply(
    X = c(
      "r-misc",
      "r-dependencies",
      "R"
    ),
    FUN = function(x) {
      x <- list.files(
        path = root$find_file(
          x
        ),
        pattern = "\\.R$",
        full.names = TRUE,
        all.files = TRUE,
        recursive = TRUE
      )
      if (length(x) > 0) {
        invisible(
          lapply(
            X = x,
            FUN = source
          )
        )
      }
    }
  )
)
# load all data in data/
data_folder <- root$find_file(
  "data"
)
data_files <- list.files(
  path = data_folder,
  pattern = "\\.rda$",
  full.names = TRUE,
  all.files = TRUE
)
if (length(data_files) > 0) {
  for (i in seq_along(data_files)) {
    load(data_files[i])
  }
  rm(i)
}
rm(
  data_files
)
# other data folders
data_raw_folder <- root$find_file(
  "data-raw"
)
data_process_folder <- root$find_file(
  "data-process"
)
# detritus
detritus_folder <- root$find_file(
  "detritus"
)
# log
log_folder <- root$find_file(
  "log"
)
