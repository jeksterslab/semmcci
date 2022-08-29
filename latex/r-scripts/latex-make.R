LatexMake <- function(clean = TRUE) {
  root <- rprojroot::is_rstudio_project
  # source(
  #  root$find_file(
  #    "latex",
  #    "r-scripts",
  #    "latex-tinytex.R"
  #  )
  # )
  source(
    root$find_file(
      "latex",
      "r-scripts",
      "latex-knit.R"
    )
  )
  source(
    root$find_file(
      "latex",
      "r-scripts",
      "latex-compile.R"
    )
  )
  # try(
  #  LatexTinytex()
  # )
  try(
    LatexKnit()
  )
  try(
    LatexCompile(clean = clean)
  )
}
