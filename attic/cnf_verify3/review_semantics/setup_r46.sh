#!/usr/bin/env bash
# Recreate the same isolated environment from its pinned cached base image.
set -euo pipefail
cnf_script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
cnf_repo_dir=$(cd -- "$cnf_script_dir/../../.." && pwd)
cnf_container=cnf-review-r46
cnf_image=docker.io/library/r-base@sha256:8917114b5d4657a0ccc33a5df2d0ab40176ac2bbbbef46791b6f89ce062db71b
mkdir -p "$cnf_script_dir/r46-library" "$cnf_script_dir/r46-downloads"
if ! podman container exists "$cnf_container"; then
  podman run -d --name "$cnf_container" \
    -v "$cnf_repo_dir:/work" -w /work \
    -e R_LIBS_USER=/work/attic/cnf_verify3/review_semantics/r46-library:/work/attic/cnf_verify3/representation/r46-library \
    "$cnf_image" sleep infinity
elif [[ "$(podman inspect --format '{{.State.Running}}' "$cnf_container")" != true ]]; then
  podman start "$cnf_container"
fi
podman exec "$cnf_container" apt-get update >"$cnf_script_dir/r46_apt_update.log" 2>&1
podman exec -e DEBIAN_FRONTEND=noninteractive "$cnf_container" \
  apt-get install -y --no-install-recommends r-cran-devtools r-cran-testthat \
  >"$cnf_script_dir/r46_apt_install.log" 2>&1
podman exec "$cnf_container" Rscript \
  attic/cnf_verify3/review_semantics/install_r46_dependencies.R \
  >"$cnf_script_dir/r46_cran_install.log" 2>&1
podman exec "$cnf_container" dpkg-query -W \
  >"$cnf_script_dir/r46_debian_package_versions.tsv"
