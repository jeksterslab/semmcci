LatexCompile <- function(clean = TRUE) {
  root <- rprojroot::is_rstudio_project
  detritus_folder <- root$find_file(
    "detritus"
  )
  pdf_folder <- root$find_file(
    "latex",
    "pdf"
  )
  try(
    system(
      paste0(
        "latexmk -f -pdf -interaction=nonstopmode -output-directory=",
        pdf_folder,
        " ",
        paste0(
          detritus_folder,
          "/*.tex"
        )
      )
    )
  )
  if (clean) {
    invisible(
      lapply(
        X = c(
          "\\.tex$",
          "\\.xml$",
          "\\.out$",
          "\\.log$",
          "\\.fls$",
          "\\.fdb_latexmk$",
          "\\.blg$",
          "\\.bcf$",
          "\\.bbl$",
          "\\.aux$"
        ),
        FUN = function(x) {
          invisible(
            lapply(
              X = list.files(
                pdf_folder,
                pattern = x,
                full.names = TRUE,
                all.files = TRUE
              ),
              FUN = unlink
            )
          )
        }
      )
    )
    unlink(
      paste0(
        detritus_folder,
        "/tex-fig-*"
      ),
      recursive = TRUE
    )
  }
}
