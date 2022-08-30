#! /bin/sh

mkdir -p aurpackages
cd aurpackages
sudo pacman -Syu --needed --noconfirm base-devel
git clone https://aur.archlinux.org/quarto-cli-bin.git
cd quarto-cli-bin
makepkg --syncdeps --install --noconfirm --needed --clean
cd ..
rm -rf quarto-cli-bin
cd ..
rm -rf aurpackages
