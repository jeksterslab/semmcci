#!/usr/bin/env bash
# Report the private and public repositories of projects: versions, newest
# public tag and commit, and flags. Read-only (gh API only); no push, no clone.
# Usage: bash .setup/scripts/repo-status.sh [SET=<type>] [project ...]
#   (make status SET=<type> PROJECTS="a b" does the same; needs gh, logged in)
#   SET=<type>   every folder next to this project whose .setup/scripts/project.R
#                has type <type> (cran, man, lib, talk, docker, rProject);
#                SET=lib also matches folders named lib-*
#   project ...  folder names (default: the projects named in .setup/ai/handoff.md)
set -Eeuo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
work="$(dirname "${here}")"

set_type="${SET:-}"
projects=()
for arg in "$@"; do
  case "${arg}" in
    SET=*) set_type="${arg#SET=}" ;;
    *) projects+=("${arg}") ;;
  esac
done

proj_type() {
  # type of a project, from its project.R ("" when none)
  local f="${work}/$1/.setup/scripts/project.R"
  [[ -f "${f}" ]] || return 0
  sed -n 's/^type <- "\(.*\)".*/\1/p' "${f}" | head -n 1
}

if [[ -n "${set_type}" ]]; then
  for d in "${work}"/*/; do
    p="$(basename "${d}")"
    t="$(proj_type "${p}")"
    if [[ "${t}" == "${set_type}" ]] ||
      [[ "${set_type}" == "lib" && "${p}" == lib-* ]]; then
      projects+=("${p}")
    fi
  done
elif [[ ${#projects[@]} -eq 0 ]]; then
  handoff="${here}/.setup/ai/handoff.md"
  if [[ -f "${handoff}" ]]; then
    for d in "${work}"/*/; do
      p="$(basename "${d}")"
      [[ -f "${d}.setup/scripts/project.R" ]] || continue
      if grep -qE "(^|[^A-Za-z0-9_-])${p}([^A-Za-z0-9_-]|$)" "${handoff}"; then
        projects+=("${p}")
      fi
    done
  fi
fi

if [[ ${#projects[@]} -eq 0 ]]; then
  echo "no projects selected" >&2
  exit 1
fi

# version in DESCRIPTION of a GitHub repository ("" when none)
gh_version() {
  gh api "repos/$1/contents/DESCRIPTION" --jq .content 2> /dev/null |
    base64 -d 2> /dev/null | sed -n 's/^Version: *//p' | head -n 1 || true
}
# version in a local DESCRIPTION ("" when none)
file_version() {
  [[ -f "$1" ]] || return 0
  sed -n 's/^Version: *//p' "$1" | head -n 1
}
# a > b for versions (sort -V); false when either is empty
ver_gt() {
  [[ -n "$1" && -n "$2" && "$1" != "$2" ]] &&
    [[ "$(printf '%s\n%s\n' "$1" "$2" | sort -V | tail -n 1)" == "$1" ]]
}

printf '%-20s %-9s %-11s %-11s %-11s %-11s %-10s %s\n' \
  project type local .verify private public tag "flags | newest public commit"

for p in "${projects[@]}"; do
  d="${work}/${p}"
  if [[ ! -d "${d}/.setup/repo" ]]; then
    printf '%-20s no .setup/repo\n' "${p}"
    continue
  fi
  t="$(proj_type "${p}")"
  [[ -n "${t}" ]] || t="type?"
  priv=""
  pub=""
  [[ -f "${d}/.setup/repo/private-github.md" ]] &&
    priv="$(tr -d '[:space:]' < "${d}/.setup/repo/private-github.md")"
  [[ -f "${d}/.setup/repo/public-github.md" ]] &&
    pub="$(tr -d '[:space:]' < "${d}/.setup/repo/public-github.md")"
  lv="$(file_version "${d}/DESCRIPTION")"
  vv="$(file_version "${d}/.verify/DESCRIPTION")"
  pv=""
  bv=""
  tag=""
  last=""
  flags=()
  if [[ -n "${priv}" ]]; then pv="$(gh_version "${priv}")"; fi
  if [[ -z "${pub}" ]]; then
    flags+=("no-public")
  elif ! gh api "repos/${pub}" > /dev/null 2>&1; then
    flags+=("no-repo")
  else
    bv="$(gh_version "${pub}")"
    tag="$(gh api "repos/${pub}/tags" --jq '.[0].name' 2> /dev/null || true)"
    last="$(gh api "repos/${pub}/commits" \
      --jq '.[0].commit.committer.date[0:10] + " " + (.[0].commit.message | split("\n")[0])' \
      2> /dev/null || true)"
    if [[ -z "${bv}" && "${t}" == "docker" ]]; then
      : # not a package: no DESCRIPTION to compare
    elif [[ -z "${bv}" ]]; then
      flags+=("unpublished")
    else
      ver_gt "${pv}" "${bv}" && flags+=("public-behind")
      [[ -n "${tag}" && "${tag}" != "v${bv}" ]] && flags+=("tag!=version")
    fi
    [[ -n "${last}" && "${last}" != *"Automated build"* ]] &&
      flags+=("not-auto")
  fi
  ver_gt "${vv}" "${lv}" && flags+=("local-behind")
  ver_gt "${pv}" "${vv}" && flags+=("verify-behind")
  f="${flags[*]:-ok}"
  printf '%-20s %-9s %-11s %-11s %-11s %-11s %-10s %s | %s\n' \
    "${p}" "${t}" "${lv:--}" "${vv:--}" "${pv:--}" "${bv:--}" "${tag:--}" \
    "${f}" "${last:--}"
done
