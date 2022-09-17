LatexMake <- function(clean = TRUE) {
  root <- rprojroot::is_rstudio_project
  source(
    root$find_file(
      "latexSource",
      "r-scripts",
      "latex-knit.R"
    )
  )
  source(
    root$find_file(
      "latexSource",
      "r-scripts",
      "latex-compile.R"
    )
  )
  try(
    LatexKnit()
  )
  try(
    LatexCompile(clean = clean)
  )
}
