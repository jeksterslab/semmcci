# find root directory
rproj <- rprojroot::is_rstudio_project
source(
  file = rproj$find_file(
    ".setup",
    "latex",
    "r-scripts",
    "knitr-options-default.R"
  )
)
source(
  file = rproj$find_file(
    ".setup",
    "latex",
    "r-scripts",
    "knitr-options-custom.R"
  )
)
knitr::opts_chunk$set(
  utils::modifyList(
    x = KnitrOptionsDefault(rproj),
    val = KnitrOptionsCustom()
  )
)
knitr::knit_hooks$set(
  document = function(x) {
    sub(
      pattern = "\\usepackage[]{color}",
      replacement = "\\usepackage{xcolor}",
      x = x,
      fixed = TRUE
    )
  }
)
# load functions
source(
  file = rproj$find_file(
    ".setup",
    "load",
    "load.R"
  )
)
Load(rproj)
# load data
x <- list.files(
  path = rproj$find_file(
    "data"
  ),
  pattern = "\\.rda$",
  full.names = TRUE,
  all.files = TRUE,
  recursive = TRUE
)
x <- c(
  x,
  list.files(
    path = rproj$find_file(
      ".setup",
      "data-dependencies"
    ),
    pattern = "\\.rda$",
    full.names = TRUE,
    all.files = TRUE,
    recursive = TRUE
  )
)
x <- c(
  x,
  list.files(
    path = rproj$find_file(
      "R"
    ),
    pattern = "\\.rda$",
    full.names = TRUE,
    all.files = TRUE,
    recursive = TRUE
  )
)
if (length(x = x) > 0) {
  for (i in seq_along(along.with = x)) {
    load(file = x[i])
  }
  rm(i)
}
rm(x)
rm(
  KnitrOptionsDefault,
  KnitrOptionsCustom,
  rproj
)
