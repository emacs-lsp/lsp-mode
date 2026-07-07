#!/bin/bash
#
# Validates that every built-in client is documented.
set -euo pipefail

readonly CLIENTS_DIR='clients/'
readonly METADATA_FILE='docs/lsp-clients.json'
readonly SIDEBAR_FILE='mkdocs.yml'
readonly DOCS_URL='https://emacs-lsp.github.io/lsp-mode'

check_jq() {
  command -v jq &>/dev/null || { echo "jq is required" >&2; exit 1; }
}

get_sidebar_items() {
  awk '
    /^  - Languages:/, /^  - [^ ]/ && !/^  - Languages:/ {
      if (/^    - .*: /) { gsub(/^    - |: .*/, ""); print }
    }
  ' "${SIDEBAR_FILE}"
}

sort_lines() {
  python3 -c '
import sys
lines = sys.stdin.read().splitlines()
lines.sort(key=str.lower)
print("\n".join(lines))
'
}

check_rule_1() {
  # Rule 1: Every defgroup in ${CLIENTS_DIR} has a matching name in ${METADATA_FILE}.
  local errors=0
  local names
  names="$(jq -r '.[].name' "${METADATA_FILE}")"
  readonly names
  while read -r client; do
    echo "ERROR: ${client} (from ${CLIENTS_DIR}) missing from ${METADATA_FILE} (\"name\": \"${client}\")" >&2
    ((++errors))
  done < <(comm -23 \
                <(awk '
                    /^\(defgroup lsp-/ { name = $2; sub(/^lsp-/, "", name) }
                    name && /^ *:group '"'"'lsp-mode/ { print name; name = "" }
                    /^\(/ && !/^\(defgroup/ { name = "" }
                  ' ${CLIENTS_DIR}*.el | sort) \
                    <(echo "${names}" | sort))
  return "${errors}"
}

check_rule_2() {
  # Rule 2: ${METADATA_FILE} is sorted by full-name (case-insensitive).
  local names sorted
  names="$(jq -r '.[]."full-name"' "${METADATA_FILE}")"
  sorted="$(echo "${names}" | sort_lines)"
  readonly names sorted
  if [[ "${names}" != "${sorted}" ]]; then
    echo "ERROR: ${METADATA_FILE} is not sorted by full-name" >&2
    git diff --no-index <(echo "${names}") <(echo "${sorted}") >&2 || true
    return 1
  fi
  return 0
}

check_rule_3() {
  # Rule 3: ${SIDEBAR_FILE} Languages section is sorted (case-insensitive).
  local items sorted
  items="$(get_sidebar_items)"
  sorted="$(echo "${items}" | sort_lines)"
  readonly items sorted
  if [[ "${items}" != "${sorted}" ]]; then
    echo "ERROR: ${SIDEBAR_FILE} clients list is not sorted" >&2
    git diff --no-index <(echo "${items}") <(echo "${sorted}") >&2 || true
    return 1
  fi
  return 0
}

check_rule_4() {
  # Rule 4: Every full-name in ${METADATA_FILE} has a matching item in ${SIDEBAR_FILE}.
  local errors=0
  local names
  names="$(jq -r '.[]."full-name"' "${METADATA_FILE}")"
  readonly names
  while read -r name; do
    echo "ERROR: ${name} (from ${METADATA_FILE}) missing from ${SIDEBAR_FILE}" >&2
    ((++errors))
  done < <(comm -23 <(echo "${names}" | sort) <(get_sidebar_items | sort))
  return "${errors}"
}

check_rule_5() {
  # Rule 5: No absolute links to ${DOCS_URL} in ${SIDEBAR_FILE} Languages section.
  local errors=0
  local bad_sidebar
  bad_sidebar="$(awk -v url="${DOCS_URL}" '
    /^  - Languages:/, /^  - [^ ]/ && !/^  - Languages:/ {
      if (index($0, url)) { sub(/:$/, "", $2); print $2 }
    }
  ' "${SIDEBAR_FILE}")"
  readonly bad_sidebar
  if [[ -n "${bad_sidebar}" ]]; then
    while read -r item; do
      echo "ERROR: ${item} in ${SIDEBAR_FILE} has a absolute link" >&2
    done <<< "${bad_sidebar}"
    ((++errors))
  fi
  return "${errors}"
}

check_rule_6() {
  # Rule 6: Every installation in ${METADATA_FILE} only gives manual steps.
  local errors=0
  local bad_metadata
  bad_metadata="$(jq -r \
                    '.[] | select(."installation" | strings | test("(?i)auto[^t]")) | .name' \
                    "${METADATA_FILE}")"
  readonly bad_metadata
  if [[ -n "${bad_metadata}" ]]; then
    while read -r name; do
      echo "ERROR: ${name} installation in ${METADATA_FILE} contains \"auto\", only give manual steps" >&2
    done <<< "${bad_metadata}"
    ((++errors))
  fi
  return "${errors}"
}

main() {
  check_jq
  local errors=0
  check_rule_1; errors=$((errors + $?))
  check_rule_2; errors=$((errors + $?))
  check_rule_3; errors=$((errors + $?))
  check_rule_4; errors=$((errors + $?))
  check_rule_5; errors=$((errors + $?))
  check_rule_6; errors=$((errors + $?))
  exit "${errors}"
}
main "$@"
