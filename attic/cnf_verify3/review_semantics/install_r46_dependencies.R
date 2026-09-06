# Run inside the isolated cnf-review-r46 container from /work.
options(repos = c(CRAN = "https://cloud.r-project.org"), timeout = 600)
review_dir = "attic/cnf_verify3/review_semantics"
description = read.dcf("DESCRIPTION")
imports = trimws(strsplit(description[1L, "Imports"], ",", fixed = TRUE)[[1L]])
imports = sub("[[:space:]]*\\(.*\\)$", "", imports)
needed = unique(c(imports, "mlbench"))
missing = setdiff(needed, rownames(installed.packages()))
cat("Library paths:\n")
print(.libPaths())
cat("Missing imports and focused-test support:\n")
print(missing)
if (length(missing)) {
  install.packages(missing, dependencies = NA, Ncpus = 4L,
    destdir = normalizePath(file.path(review_dir, "r46-downloads")))
}
required = unique(c(needed, "devtools", "testthat"))
loaded = vapply(required, requireNamespace, logical(1), quietly = TRUE)
if (!all(loaded)) stop("Missing dependencies: ", paste(required[!loaded], collapse = ", "))
for (package in c("mlr3", "mlr3misc", "paradox")) {
  minimum = c(mlr3 = "0.20.0", mlr3misc = "0.17.0", paradox = "1.0.0")[[package]]
  stopifnot(packageVersion(package) >= package_version(minimum))
}
versions = installed.packages()[, c("Package", "Version", "LibPath", "Built"), drop = FALSE]
write.csv(versions, file.path(review_dir, "r46_package_versions.csv"), row.names = FALSE)
capture.output(sessionInfo(), file = file.path(review_dir, "r46_session_info.txt"))
print(versions[required, , drop = FALSE])
cat("All DESCRIPTION imports, devtools, and testthat load successfully.\n")
