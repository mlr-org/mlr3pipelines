#!/usr/bin/env bash
# Reuse the isolated container; pass ordinary Rscript arguments verbatim.
set -euo pipefail
cnf_container=cnf-review-r46
if [[ "$(podman inspect --format '{{.State.Running}}' "$cnf_container")" != true ]]; then
  podman start "$cnf_container" >/dev/null
fi
exec podman exec "$cnf_container" Rscript "$@"
