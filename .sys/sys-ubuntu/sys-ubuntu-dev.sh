#! /usr/bin/bash

# https://cran.r-project.org/bin/linux/ubuntu/fullREADME.html

echo "deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/" | sudo tee -a /etc/apt/sources.list
sudo awk -i inplace '!seen[$0]++' /etc/apt/sources.list
sudo apt-get -y update && sudo apt-get -y upgrade
sudo apt-get install -y \
  r-base \
  r-base-dev \
  git \
  parallel \
  pandoc \
  vim \
  wget \
  nnn \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libfontconfig1-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libfreetype6-dev \
  libpng-dev \
  libtiff5-dev \
  libjpeg-dev \
  libopenblas-base

wget https://github.com/quarto-dev/quarto-cli/releases/download/v1.0.38/quarto-1.0.38-linux-amd64.deb

sudo dpkg -i quarto-1.0.38-linux-amd64.deb
rm -rf quarto-1.0.38-linux-amd64.deb

wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
sudo awk -i inplace '!seen[$0]++' /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
gpg --show-keys /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
echo "The fingerprint should be 298A3A825C0D65DFD57CBB651716619E084DAB9."
