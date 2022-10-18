#!/bin/bash

sudo pacman --noconfirm --needed -Syu \
        make                          \
        tmux                          \
        git                           \
        vim                           \
        apptainer                     \
        docker                        \
        curl                          \
        wget
