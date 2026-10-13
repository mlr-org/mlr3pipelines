# Isolated R 4.6.1 environment for focused CNF tests

Prepared 2026-09-06 in the persistent local podman container
`cnf-review-r46`. The host R installation and package metadata were not changed.

From the repository root, run:

```sh
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  -e 'devtools::test(filter = "Cnf", stop_on_failure = TRUE)'
```

The launcher accepts ordinary `Rscript` arguments, so a narrower test or a
standalone R script can be run through the same environment:

```sh
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  -e 'devtools::test(filter = "CnfFormula_simplify", stop_on_failure = TRUE)'
bash attic/cnf_verify3/review_semantics/run_r46.sh path/to/script.R
```

The equivalent direct command is:

```sh
podman exec cnf-review-r46 Rscript \
  -e 'devtools::test(filter = "Cnf", stop_on_failure = TRUE)'
```

The repository is mounted at `/work`, which is also the container working
directory. Edits made on the host are immediately visible to tests. The
launcher starts the existing container if it has been stopped. To recreate a
missing container and install its dependencies:

```sh
bash attic/cnf_verify3/review_semantics/setup_r46.sh
```

## Versions and isolation

The base image is pinned to:

```
docker.io/library/r-base@sha256:8917114b5d4657a0ccc33a5df2d0ab40176ac2bbbbef46791b6f89ce062db71b
```

The local image ID is
`0b9a80f353372588ef18ff58dbfe04b5e561f630ef6828679df39b8a0e59e9cc`.
It provides R 4.6.1 (2026-06-24), x86_64 Linux, on Debian forky/sid.

| Package | Installed version |
|---|---|
| devtools | 2.4.6 |
| testthat | 3.3.2 |
| mlr3 | 1.8.0 |
| mlr3misc | 0.23.0 |
| paradox | 1.0.1 |
| checkmate | 2.3.4 |
| backports | 1.5.1 |
| data.table | 1.18.6.1 |
| cli | 3.6.6 |
| digest | 0.6.39 |
| lgr | 0.5.2 |
| R6 | 2.6.1 |
| mlbench | 2.1-11 |

All `DESCRIPTION` imports satisfy their declared minimum versions. `mlbench`
is included because the shared package test setup registers a fixture that
uses it. The six CNF test files need no other optional package installations.

`devtools`, `testthat`, and their Debian dependencies are installed only in
the named container. R packages installed from CRAN live in the ignored
directory `review_semantics/r46-library/`. The environment also reads the
pre-existing `representation/r46-library/` to reuse its R 4.6.1 builds of
`checkmate` and `backports`; this setup does not modify that directory.
Source package downloads are retained in the ignored `r46-downloads/`.

The saved complete R package/version/library/build table is
`r46_package_versions.csv`; `r46_session_info.txt` records R session details,
and `r46_debian_package_versions.tsv` records installed OS package versions.
The setup script pins the base image but uses the configured current Debian
repositories and CRAN on a future rebuild. The existing named container and
saved library preserve the environment actually verified here.

## Installation commands and the resolved setup issue

`setup_r46.sh` contains the full executable setup. The OS dependency commands
were:

```sh
podman exec cnf-review-r46 apt-get update
podman exec -e DEBIAN_FRONTEND=noninteractive cnf-review-r46 \
  apt-get install -y --no-install-recommends r-cran-devtools r-cran-testthat
podman exec cnf-review-r46 Rscript \
  attic/cnf_verify3/review_semantics/install_r46_dependencies.R
```

The R installer reads `DESCRIPTION`, installs missing imports plus `mlbench`
with `dependencies = NA`, and verifies that all required namespaces load.
It uses CRAN `https://cloud.r-project.org`, four installation workers, and an
absolute `destdir` under `/work`.

The first attempt passed a relative `destdir`. R's parallel installation
workers changed working directory and then reported downloaded tarballs as
invalid packages. Using `normalizePath()` for `destdir` resolved the issue;
there were no unresolved dependency failures. The original failure is saved
in `r46_cran_relative_destdir_failure.log`, and the successful install is in
`r46_cran_install.log`. OS logs are `r46_apt_update.log` and
`r46_apt_install.log`.

## Verification

The focused validation command is the first command above, with the repository's
normal parallel test configuration and `stop_on_failure = TRUE`. Its output
is saved in `r46_cnf_smoke.log`. It completed successfully in 127.6 seconds:

```
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 3815 ]
```

This includes the current added simplifier regressions in the shared working
tree. `r46_test_source_hashes.sha256` records the six test-file hashes. There
are 146 installed R packages in the combined library paths. No full-package
test run was performed.
