# See options
# https://yihui.org/knitr/options/
tex_figures_folder <- file.path(
  dot_detritus_folder, # see .r-load-all/r-load-all.R
  "tex",
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
dir.create(tex_figures_folder)
knitr_options_default <- list(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 6,
  fig.path = tex_figures_folder,
  out.width = "100%",
  dev = "pdf",
  dev.args = list(
    pdf = list(
      family = "Times"
    )
  )
)
rm(
  tex_figures_folder
)
