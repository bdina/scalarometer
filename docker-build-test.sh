#!/usr/bin/env bash
# Run Gradle tests inside Docker using graalvm-dev:latest (incremental by default).
# Usage: ./docker-build-test.sh [--clean] [extra gradle args...]

set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

tasks=(test)
extra_args=()
for arg in "$@"; do
  if [ "${arg}" = "--clean" ]; then
    tasks=(clean test)
  else
    extra_args+=("${arg}")
  fi
done

exec "${SCRIPT_DIR}/scripts/docker-build" gradle --no-daemon "${tasks[@]}" --warning-mode all ${extra_args[@]+"${extra_args[@]}"}
