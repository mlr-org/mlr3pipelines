# Run in a fresh R process after prepare.R. No production source() calls occur.
args = commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, args %in% c("baseline", "current"))
stage = args[[1L]]
audit_dir = normalizePath("attic/cnf_fixes/ops_dispatch/installed_check")
runtime = if (getRversion() < "4.0.0") "r36" else "r46"
label = paste(stage, runtime, sep = "_")
package_dir = file.path(audit_dir, "build", stage, "cnfdispatchcheck")
library_dir = file.path(audit_dir, "libraries", label)
dir.create(library_dir, recursive = TRUE, showWarnings = FALSE)
install_log = file.path(audit_dir, "results", paste0(label, "_install.log"))
cat("Runtime:", R.version.string, "\n")
cat("Package scope: isolated six-file CNF package; unchanged production code/registrations\n")
for (package in c("checkmate", "mlr3misc", "digest")) {
  cat("Dependency:", package, as.character(packageVersion(package)), "\n")
}
stopifnot(!"cnfdispatchcheck" %in% loadedNamespaces())
status = system2(file.path(R.home("bin"), "R"),
  c("CMD", "INSTALL", "--byte-compile", paste0("--library=", shQuote(library_dir)), shQuote(package_dir)),
  stdout = install_log, stderr = install_log,
  env = paste0("R_LIBS=", shQuote(paste(.libPaths(), collapse = .Platform$path.sep))))
cat("Install exit status:", status, "\n")
stopifnot(identical(status, 0L))
library("cnfdispatchcheck", lib.loc = library_dir, character.only = TRUE)
ns = asNamespace("cnfdispatchcheck")
stopifnot(identical(normalizePath(getNamespaceInfo(ns, "path")),
  normalizePath(file.path(library_dir, "cnfdispatchcheck"))))
stopifnot(!exists("CnfAtom", envir = globalenv(), inherits = FALSE))
source(file.path(audit_dir, "verify.R"), local = TRUE)
