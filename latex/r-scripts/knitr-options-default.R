# See options
# https://yihui.org/knitr/options/
detritus_figures_folder <- file.path(
  detritus_folder, # see r-load-all/r-load-all.R
  paste0(
    "tex-fig-",
    paste0(
      sample(
        letters,
        size = 8,
        replace = TRUE
      ),
      collapse = ""
    )
  )
)
dir.create(detritus_figures_folder)
knitr_options_default <- list(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 6,
  fig.path = detritus_figures_folder,
  out.width = "100%",
  dev = "pdf",
  dev.args = list(
    pdf = list(
      family = "Times"
    )
  )
)
rm(
  detritus_figures_folder
)
