#!/usr/bin/env bash
# Clone the Overleaf project into a new folder. Clone only: the copy cannot push
# (its push URL is set to "no-push"); upload to Overleaf by hand.
# The Overleaf token is asked once by git and kept by git's credential helper
# for git.overleaf.com only (~/work/.overleaf/git-credentials, mode 600). It is never read,
# printed, or written by this script.
# Usage: bash .setup/scripts/clone-overleaf.sh [PROJECT_NUMBER|URL] [FOLDER]
#   PROJECT_NUMBER  default: .setup/repo/private-overleaf.md of this project
#   FOLDER          default: the repository name in .setup/repo/private-github.md
#                   (required when PROJECT_NUMBER is given)
#   DRYRUN=TRUE prints the git command and stops
set -Eeuo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

arg="${1:-}"
folder="${2:-}"
if [[ -z "${arg}" ]]; then
  file="${here}/.setup/repo/private-overleaf.md"
  [[ -f "${file}" ]] || { echo "No PROJECT_NUMBER given and no ${file}" >&2; exit 1; }
  arg="$(head -n 1 "${file}" | tr -d '[:space:]')"
  if [[ -z "${folder}" && -f "${here}/.setup/repo/private-github.md" ]]; then
    folder="$(head -n 1 "${here}/.setup/repo/private-github.md" | tr -d '[:space:]')"
    folder="${folder##*/}"
  fi
fi
number="${arg##*/project/}"
number="${number%%[/?#]*}"
[[ "${number}" =~ ^[0-9a-f]{24}$ ]] || {
  echo "Not an Overleaf project number (24 hex characters): ${number}" >&2
  exit 1
}
[[ -n "${folder}" ]] || { echo "Give the FOLDER to clone into" >&2; exit 1; }

if [[ -e "${folder}" && -n "$(ls -A "${folder}" 2>/dev/null)" ]]; then
  echo "Refusing: ${folder} exists and is not empty" >&2
  exit 1
fi

echo "git clone https://git@git.overleaf.com/${number} ${folder}"
if [[ "${DRYRUN:-}" == "TRUE" ]]; then
  exit 0
fi

# keep the token for git.overleaf.com only, once, under ~/work/.overleaf (the
# folder of setup-git-github.sh, like the SSH keys); folder 700, file 600.
# Skipped when a helper for git.overleaf.com is already configured.
key="credential.https://git.overleaf.com.helper"
if [[ -z "$(git config --global --get-all "${key}" 2>/dev/null || true)" ]]; then
  umask 077
  mkdir -p "${HOME}/work/.overleaf"
  chmod 700 "${HOME}/work/.overleaf"
  touch "${HOME}/work/.overleaf/git-credentials"
  chmod 600 "${HOME}/work/.overleaf/git-credentials"
  git config --global "${key}" "store --file ${HOME}/work/.overleaf/git-credentials"
fi

git clone "https://git@git.overleaf.com/${number}" "${folder}"
# clone only (never push to Overleaf)
git -C "${folder}" remote set-url --push origin no-push
