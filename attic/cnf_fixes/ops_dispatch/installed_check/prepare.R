# Run from the repository root: Rscript <this-file> baseline|current
# This creates an isolated package from unchanged production source files.
args = commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, args %in% c("baseline", "current"))
stage = args[[1L]]
audit_dir = "attic/cnf_fixes/ops_dispatch/installed_check"
package_dir = file.path(audit_dir, "build", stage, "cnfdispatchcheck")
dir.create(file.path(package_dir, "R"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(audit_dir, "results"), showWarnings = FALSE)
baseline_revision = "da252ba5"
files = paste0(c("CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify", "CnfSymbol", "CnfUniverse"), ".R")
paths = c(file.path("R", files), "NAMESPACE")
destination = c(file.path(package_dir, "R", files), file.path(dirname(package_dir), "production_NAMESPACE"))

if (stage == "baseline") {
  revision = system2("git", c("rev-parse", baseline_revision), stdout = TRUE)
  stopifnot(length(revision) == 1L)
  for (i in seq_along(paths)) {
    status = system2("git", c("show", shQuote(paste0(revision, ":", paths[[i]]))), stdout = destination[[i]])
    stopifnot(identical(status, 0L))
  }
} else {
  revision = "working tree; exact source hashes recorded below"
  stopifnot(all(file.copy(paths, destination, overwrite = TRUE)))
}

production_namespace = readLines(tail(destination, 1L), warn = FALSE)
imports = c("import(checkmate)", "import(mlr3misc)", "importFrom(digest,digest)")
namespace = production_namespace[grepl("Cnf", production_namespace, fixed = TRUE) |
  grepl("%among%", production_namespace, fixed = TRUE) | production_namespace %in% imports]
stopifnot(all(imports %in% namespace))
writeLines(namespace, file.path(package_dir, "NAMESPACE"))
writeLines(namespace, file.path(audit_dir, "results", paste0(stage, "_NAMESPACE")))
writeLines(c(
  "Package: cnfdispatchcheck",
  "Type: Package",
  "Title: Isolated Production CNF Namespace Compatibility Check",
  "Version: 0.0.0.9000",
  "Authors@R: person('CNF', 'Audit', role = c('aut', 'cre'), email = 'cnf-audit@example.invalid')",
  "Description: An audit package containing unchanged production CNF source files",
  "    and verbatim relevant generated namespace directives. This is not the full",
  "    mlr3pipelines package and does not use assertion or dependency shims.",
  "License: LGPL-3",
  "Encoding: UTF-8",
  "Depends: R (>= 3.3.0)",
  "Imports: checkmate, mlr3misc, digest",
  "ByteCompile: yes"
), file.path(package_dir, "DESCRIPTION"))

manifest = data.frame(path = c(paths, "isolated/NAMESPACE"),
  sha256 = vapply(c(destination, file.path(package_dir, "NAMESPACE")),
    function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE), character(1L)),
  stringsAsFactors = FALSE)
write.table(manifest, file.path(audit_dir, "results", paste0(stage, "_source_manifest.tsv")),
  sep = "\t", row.names = FALSE, quote = FALSE)
writeLines(revision, file.path(audit_dir, "results", paste0(stage, "_revision.txt")))
cat("Prepared", stage, "isolated package from", revision, "\n")
print(manifest, row.names = FALSE)
