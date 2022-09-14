# source
repos <- "https://packagemanager.rstudio.com/all/latest"
if (sys == "arch") {
  repos <- "https://packagemanager.rstudio.com/all/latest"
}
if (sys == "bionic") {
  repos <- "https://packagemanager.rstudio.com/all/__linux__/bionic/latest"
}
if (sys == "focal") {
  repos <- "https://packagemanager.rstudio.com/all/__linux__/focal/latest"
}
if (sys == "jammy") {
  repos <- "https://packagemanager.rstudio.com/all/__linux__/jammy/latest"
}
if (sys == "centos7") {
  repos <- "https://packagemanager.rstudio.com/all/__linux__/centos7/latest"
}
if (sys == "centos8") {
  repos <- "https://packagemanager.rstudio.com/all/__linux__/centos8/latest"
}
if (sys == "opensuse153") {
  repos <- "https://packagemanager.rstudio.com/all/__linux__/opensuse153/latest"
}
if (sys == "win") {
  repos <- "https://packagemanager.rstudio.com/all/latest/bin/windows"
}
options(menu.graphics = FALSE)
options(repos = c(REPO_NAME = repos))
