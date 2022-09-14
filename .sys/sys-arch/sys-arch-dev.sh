#! /usr/bin/bash

# Archlinux dependencies for development

# wget and curl
sudo pacman -Syu --needed --noconfirm wget curl

# version control
sudo pacman -Syu --needed --noconfirm git

# parallel
sudo pacman -Syu --needed --noconfirm parallel

# editor
sudo pacman -Syu --needed --noconfirm vim code

# mail
sudo pacman -Syu --needed --noconfirm mailx

# documents
sudo pacman -Syu --needed --noconfirm pandoc texlive-most texlive-bibtexextra biber

# containers
sudo pacman -Syu --needed --noconfirm docker apptainer

# R
sudo pacman -Syu --needed --noconfirm gcc-fortran openblas xz r

# file manager
sudo pacman -Syu --needed --noconfirm nnn
