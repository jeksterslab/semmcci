#!/bin/bash

git clone git@github.com:jeksterslab/semmcci.git
rm -rf "$PWD.git"
mv semmcci/.git "$PWD"
rm -rf semmcci
