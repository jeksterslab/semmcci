#! /usr/bin/bash

mkdir -p ~/.julia
cd ~/.julia
wget -c https://julialang-s3.julialang.org/bin/linux/x64/1.7/julia-1.7.3-linux-x86_64.tar.gz -O - | tar -xz
mkdir -p ~/.local/julia
mv ~/.julia/julia-1.7.3/* ~/.local/julia
rm -rf ~/.julia/julia-1.7.3
