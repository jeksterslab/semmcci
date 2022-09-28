#! /usr/bin/bash

mkdir -p ~/.julia
cd ~/.julia
wget -c https://julialang-s3.julialang.org/bin/linux/x64/1.8/julia-1.8.1-linux-x86_64.tar.gz -O - | tar -xz
mkdir -p ~/.local/julia
mv ~/.julia/julia-1.8.1/* ~/.local/julia
rm -rf ~/.julia/julia-1.8.1
