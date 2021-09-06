# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks()

# dependency of NMF
# installing from GH avoids using a static Bioconductor version
get_stage("install") %>%
  add_step(step_install_github("Bioconductor/Biobase"))

if (ci_on_ghactions() && identical(ci_get_env("MLR3"), "devel")) {
  get_stage("install") %>%
    add_step(step_install_github("mlr-org/mlr3"))
}

if (ci_on_ghactions() && ci_has_env("BUILD_PKGDOWN")) {
  # creates pkgdown site and pushes to gh-pages branch
  # only for the runner with the "BUILD_PKGDOWN" env var set
  get_stage("install") %>%
    add_step(step_install_github("mlr-org/mlr3pkgdowntemplate"))
  do_pkgdown()
}

