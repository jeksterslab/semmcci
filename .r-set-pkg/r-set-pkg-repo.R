# source
src <- "https://packagemanager.rstudio.com/all/latest"
repos <- src

# find root directory
root <- rprojroot::is_rstudio_project

# source functions
sys_tools <- list.files(
  path = root$find_file(
    ".r-misc",
    "sysTools"
  ),
  full.names = TRUE
)
for (i in seq_along(sys_tools)) {
  source(sys_tools[i])
}

# set repos
os <- OS()
if (os == "osx") {
  repos <- src
}
if (os == "windows") {
  repos <- "https://packagemanager.rstudio.com/all/latest/bin/windows"
}
if (os == "linux") {
  distro <- Distro()
  if (is.null(distro)) {
    repos <- src
  } else {
    repos <- src
    if (tolower(distro["distro"]) == "ubuntu") {
      if (distro["codename"] == "bionic") {
        repos <- "https://packagemanager.rstudio.com/all/__linux__/bionic/latest"
      }
      if (distro["codename"] == "focal") {
        repos <- "https://packagemanager.rstudio.com/all/__linux__/focal/latest"
      }
      if (distro["codename"] == "jammy") {
        repos <- "https://packagemanager.rstudio.com/all/__linux__/jammy/latest"
      }
    }
  }
}

# options

options(menu.graphics = FALSE)
options(repos = c(REPO_NAME = repos))




# CentOS
# repo_centos7 <- "https://packagemanager.rstudio.com/all/__linux__/centos7/latest"
# repo_centos8 <- "https://packagemanager.rstudio.com/all/__linux__/centos8/latest"
# repo_centos9 <- repos

# Red Hat Enterprise Linux
# repo_rhel7 <- repo_centos7
# repo_rhel8 <- repo_centos8
# repo_rhel9 <- repo_centos9

# OpenSUSE
# repo_opensuse153 <- "https://packagemanager.rstudio.com/all/__linux__/opensuse153/latest"
# repo_opensuse154 <- repos

# Ubuntu
# repo_bionic <- "https://packagemanager.rstudio.com/all/__linux__/bionic/latest"
# repo_focal <- "https://packagemanager.rstudio.com/all/__linux__/focal/latest"
# repo_jammy <- "https://packagemanager.rstudio.com/all/__linux__/jammy/latest"

# Windows
# repo_win <- "https://packagemanager.rstudio.com/all/latest/bin/windows"

# OSX
# repo_osx <- repos




# repos <- "https://packagemanager.rstudio.com/all/latest"
# if (sys == "arch") {
#  repos <- "https://packagemanager.rstudio.com/all/latest"
# }
# if (sys == "bionic") {
#  repos <- "https://packagemanager.rstudio.com/all/__linux__/bionic/latest"
# }
# if (sys == "focal") {
#  repos <- "https://packagemanager.rstudio.com/all/__linux__/focal/latest"
# }
# if (sys == "jammy") {
#  repos <- "https://packagemanager.rstudio.com/all/__linux__/jammy/latest"
# }
# if (sys == "centos7") {
#  repos <- "https://packagemanager.rstudio.com/all/__linux__/centos7/latest"
# }
# if (sys == "centos8") {
#  repos <- "https://packagemanager.rstudio.com/all/__linux__/centos8/latest"
# }
# if (sys == "opensuse153") {
#  repos <- "https://packagemanager.rstudio.com/all/__linux__/opensuse153/latest"
# }
# if (sys == "win") {
#  repos <- "https://packagemanager.rstudio.com/all/latest/bin/windows"
# }
