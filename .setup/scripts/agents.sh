#!/bin/bash

# make agents: the agents' command line tools (Claude Code, Codex,
# Antigravity, Copilot CLI, gh), on the host (in ~/work/tools, linked, where
# ~/work exists); no R, no Apptainer.
# Installs the missing ones; --update (make agents UPDATE=TRUE) updates them.
# Uses rProject's inst/shell/install-*.sh from RPROJECT_SHELL, the project's
# own library, or an rProject clone next to the project.

set -euo pipefail

update=false
if [[ "${1:-}" == "--update" ]]; then
  update=true
fi

shell_dir=""
for candidate in \
  "${RPROJECT_SHELL:-}" \
  ".library/rProject/shell" \
  "../rProject/inst/shell"; do
  if [[ -n "${candidate}" && -f "${candidate}/install-common.sh" ]]; then
    shell_dir="${candidate}"
    break
  fi
done
if [[ -z "${shell_dir}" ]]; then
  printf 'ERROR: rProject install scripts not found.\n' >&2
  printf 'Set RPROJECT_SHELL, or clone rProject next to this project.\n' >&2
  exit 1
fi

export PATH="${HOME}/.local/bin:${PATH}"
for tool in claude codex antigravity copilot gh; do
  name="${tool}"
  if [[ "${tool}" == "antigravity" ]]; then
    name="agy"
  fi
  if [[ "${update}" == true ]] || ! command -v "${name}" >/dev/null 2>&1; then
    bash "${shell_dir}/install-${tool}.sh"
  else
    printf '==> %s: already installed\n' "${tool}"
  fi
done
