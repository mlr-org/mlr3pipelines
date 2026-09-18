# Preserve complete traces for a compact source-event control bank on both R
# versions. The bank combines hand-built cases with one independently found
# nested unit-merge event, reduced by its event predicate rather than output.
suppressPackageStartupMessages({library(checkmate); library(mlr3misc); library(digest)})
for (file in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(file, ".R")))
}
plain_simplify = simplify_cnf
source("attic/cnf_verify3/set_symmetry/harness.R")
observed = symmetry_instrument(plain_simplify)
out_dir = "attic/cnf_verify3/set_symmetry"
original_checks = readRDS(file.path(out_dir, "checks_r36.rds"))
guard_id = which(vapply(symmetry_sites, function(site) site$kind == "==" &&
  site$expression == "length(unit_domains[[nu]]) == length(unit[[1L]])", FALSE))
stopifnot(length(guard_id) == 1L)
control = original_checks$cases[[original_checks$first_events[[paste0(guard_id, ":FALSE")]]]]
has_guard = function(case) {
  if (!length(case$clauses) || any(!lengths(case$clauses))) return(FALSE)
  if (any(vapply(case$clauses, function(clause) any(!lengths(clause)), FALSE))) return(FALSE)
  result = tryCatch(symmetry_run(case, observed), error = function(e) NULL)
  !is.null(result) && any(vapply(result$trace, function(event) {
    event$id == guard_id && identical(event$value, FALSE)
  }, FALSE))
}
stopifnot(has_guard(control))
repeat {
  previous = control
  for (ci in rev(seq_along(control$clauses))) {
    candidate = control
    candidate$clauses = candidate$clauses[-ci]
    if (has_guard(candidate)) control = candidate
  }
  for (ci in seq_along(control$clauses)) {
    for (si in rev(seq_along(control$clauses[[ci]]))) {
      candidate = control
      candidate$clauses[[ci]] = candidate$clauses[[ci]][-si]
      if (has_guard(candidate)) control = candidate
    }
    for (si in seq_along(control$clauses[[ci]])) {
      for (vi in rev(seq_along(control$clauses[[ci]][[si]]))) {
        candidate = control
        candidate$clauses[[ci]][[si]] = candidate$clauses[[ci]][[si]][-vi]
        if (has_guard(candidate)) control = candidate
      }
    }
  }
  if (identical(control, previous)) break
}
control = symmetry_quotient(control)$case
control$label = "reduced_effective_unit_proper_subset"
stopifnot(has_guard(control))
controls = c(original_checks$cases[seq_len(15L)], list(control))
set.seed(906164L)
traces = lapply(controls, function(case) {
  original = symmetry_run(case, observed)
  fibers = symmetry_fibers(case, split = TRUE)
  # The stronger ordered-vector statement applies to consistent block lifts.
  for (ordered_fibers in list(fibers, symmetry_fibers(case, rename = TRUE))) {
    ordered = symmetry_run(symmetry_transform(case, ordered_fibers), observed)
    expected = symmetry_transform(list(domains = case$domains, clauses = original$output), ordered_fibers)$clauses
    stopifnot(identical(ordered$output, expected), identical(ordered$trace, original$trace))
  }
  domain_reordered = case
  domain_reordered$domains = lapply(domain_reordered$domains, rev)
  domain_result = symmetry_run(domain_reordered, observed)
  stopifnot(identical(domain_result$output, original$output), identical(domain_result$trace, original$trace))
  transformed = symmetry_run(symmetry_transform(case, fibers, reorder = TRUE), observed)
  stopifnot(identical(original$trace, transformed$trace),
    identical(symmetry_project(original$output, symmetry_fibers(case)),
      symmetry_project(transformed$output, fibers)))
  list(case = case, output = original$output, trace = original$trace)
})
suffix = if (getRversion() >= "4.6") "r46" else "r36"
saveRDS(list(version = R.version.string, traces = traces,
  source_sha256 = digest(file = "R/CnfFormula_simplify.R", algo = "sha256")),
  file.path(out_dir, paste0("source_controls_", suffix, ".rds")))
cat("Reduced nested unit-merge control:\n")
dput(control)
cat("PASS:", length(traces), "full original/split-permuted control trace pairs,",
  sum(vapply(traces, function(item) length(item$trace), 0L)), "events per representation\n")
