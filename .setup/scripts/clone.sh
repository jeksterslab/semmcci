#!/bin/bash

# stop at the first error: a failed clone must not go on to remove .git
set -Eeuo pipefail

# the SSH host: github-<owner> when ~/.ssh/config defines that alias (for
# example on Roar), else github.com
host="github.com"
if grep -Eqs "^Host[[:space:]]+(.*[[:space:]])?github-jeksterslab([[:space:]]|$)" ~/.ssh/config; then
  host="github-jeksterslab"
fi
git clone "git@${host}:jeksterslab/semmcci.git"
rm -rf "$PWD/.git"
mv "semmcci/.git" "$PWD"
rm -rf "semmcci"
