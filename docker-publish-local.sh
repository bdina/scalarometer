#!/usr/bin/env bash
# Run tests and publish Scalarometer to host ~/.m2/repository inside Docker using graalvm-dev:latest.
# Usage: ./docker-publish-local.sh [--clean] [--skip-tests] [--version <ver>] [--allow-dirty] [extra gradle args...]

set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

clean_flag=0
run_tests=1
extra_args=()

while [ "$#" -gt 0 ]; do
  case "$1" in
    --clean)
      clean_flag=1
      shift
      ;;
    --skip-tests)
      run_tests=0
      shift
      ;;
    --allow-dirty)
      extra_args+=("-PallowDirtyPublish=true")
      shift
      ;;
    --version)
      if [ "$#" -lt 2 ]; then
        echo "ERROR: --version requires a value" >&2
        exit 2
      fi
      extra_args+=("-PpublishVersion=$2")
      shift 2
      ;;
    --version=*)
      extra_args+=("-PpublishVersion=${1#--version=}")
      shift
      ;;
    *)
      extra_args+=("$1")
      shift
      ;;
  esac
done

tasks=()
if [ "${clean_flag}" -eq 1 ]; then
  tasks+=(clean)
fi
if [ "${run_tests}" -eq 1 ]; then
  tasks+=(test)
fi
tasks+=(publishToMavenLocal)

exec "${SCRIPT_DIR}/scripts/docker-build" gradle --no-daemon "${tasks[@]}" --warning-mode all ${extra_args[@]+"${extra_args[@]}"}
