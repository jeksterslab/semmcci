# find root directory
root <- rprojroot::is_rstudio_project
source(
  root$find_file(
    "r-load-all",
    "r-load-all.R"
  )
)
source(
  root$find_file(
    "latex",
    "r-scripts",
    "knitr-options-default.R"
  )
)
source(
  root$find_file(
    "latex",
    "r-scripts",
    "knitr-options-custom.R"
  )
)
knitr::opts_chunk$set(
  modifyList(
    knitr_options_default,
    knitr_options_custom
  )
)
knitr::knit_hooks$set(
  document = function(x) {
    sub(
      "\\usepackage[]{color}",
      "\\usepackage{xcolor}",
      x,
      fixed = TRUE
    )
  }
)
rm(
  knitr_options_default,
  knitr_options_custom
)
