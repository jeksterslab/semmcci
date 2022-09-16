# find root directory
root <- rprojroot::is_rstudio_project
system(
  paste(
    shQuote(
      file.path(
        R.home("bin"),
        "R"
      )
    ),
    "CMD",
    "Rd2pdf",
    shQuote(
      dirname(
        root$find_file(
          "project.Rproj"
        )
      )
    )
  )
)
