#!/usr/bin/env bash
# Run Gradle clean and test inside Docker using graalvm-dev:latest.
# Usage: ./docker-build-test.sh

set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

exec "${SCRIPT_DIR}/scripts/docker-build" gradle --no-daemon clean test --warning-mode all
