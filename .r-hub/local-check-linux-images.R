# find root directory
root <- rprojroot::is_rstudio_project
# root_folder
root_folder <- dirname(
  root$find_file(
    "project.Rproj"
  )
)
setwd(root_folder)
source("https://install-github.me/r-hub/sysreqs")
imgs <- paste0(
  "rhub",
  "/",
  rhub::local_check_linux_images()$name
)
chk <- vector(mode = "list", length = length(imgs))
for (i in seq_along(imgs)) {
  chk[[i]] <- rhub::local_check_linux(
    path = root_folder,
    image = imgs[i]
  )
}
saveRDS(
  chk,
  file = root$find_file(
    ".r-hub",
    "local-check-linux-images.Rds"
  )
)
