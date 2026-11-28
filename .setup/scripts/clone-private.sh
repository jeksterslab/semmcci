#!/usr/bin/env bash
# Clone the private repository (jeksterslab-psu) into a new folder. Clone only:
# nothing is pushed, nothing is removed.
# Usage: bash .setup/scripts/clone-private.sh [OWNER/REPO] [FOLDER]
#   OWNER/REPO  default: .setup/repo/private-github.md of this project
#   FOLDER      default: the repository name, in the current folder
#   DRYRUN=TRUE prints the git command and stops
set -Eeuo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

repo="${1:-}"
if [[ -z "${repo}" ]]; then
  file="${here}/.setup/repo/private-github.md"
  [[ -f "${file}" ]] || { echo "No OWNER/REPO given and no ${file}" >&2; exit 1; }
  repo="$(head -n 1 "${file}" | tr -d '[:space:]')"
fi
[[ "${repo}" =~ ^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$ ]] || {
  echo "Not an OWNER/REPO: ${repo}" >&2
  exit 1
}
owner="${repo%%/*}"
folder="${2:-${repo##*/}}"

# the SSH host: github-<owner> when ~/.ssh/config defines that alias (for
# example on Roar), else github.com
host="github.com"
if grep -Eqs "^Host[[:space:]]+(.*[[:space:]])?github-${owner}([[:space:]]|$)" ~/.ssh/config; then
  host="github-${owner}"
fi

if [[ -e "${folder}" && -n "$(ls -A "${folder}" 2>/dev/null)" ]]; then
  echo "Refusing: ${folder} exists and is not empty" >&2
  exit 1
fi

echo "git clone git@${host}:${repo}.git ${folder}"
if [[ "${DRYRUN:-}" == "TRUE" ]]; then
  exit 0
fi
git clone "git@${host}:${repo}.git" "${folder}"
