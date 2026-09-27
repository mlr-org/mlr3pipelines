# Run from the repository root, in either the existing R 3.6 source
# environment or review_semantics/run_r46.sh. No package file is changed.
suppressPackageStartupMessages({
  library(checkmate)
  library(mlr3misc)
  library(digest)
  library(jsonlite)
})
for (file in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(file, ".R")))
}
plain_simplify = simplify_cnf
source("attic/cnf_verify3/set_symmetry/harness.R")
observed = symmetry_instrument(plain_simplify)
production_sites = symmetry_sites
stopifnot(sum(vapply(production_sites, function(site) site$kind == "if", FALSE)) == 108L)
seed = 260906164L
set.seed(seed)
out_dir = "attic/cnf_verify3/set_symmetry"
suffix = if (getRversion() >= "4.6") "r46" else "r36"

domains = list(X = letters[1:5], Y = letters[1:5], Z = letters[1:5], unused = letters[1:6])
make_case = function(label, clauses) list(label = label, domains = domains, clauses = clauses)
cases = list(
  make_case("unit_intersection", list(list(X = c("a", "b", "c")), list(X = c("b", "c", "d")))),
  make_case("unit_conflict", list(list(X = c("a", "b")), list(X = c("c", "d")))),
  make_case("restriction_elimination", list(list(X = c("a", "b")), list(X = c("a", "b", "c"), Y = "a"))),
  make_case("restriction_unchanged", list(list(X = c("a", "b", "c")), list(X = c("a", "b"), Y = "a"))),
  make_case("restriction_shrink", list(list(X = c("a", "b")), list(X = c("b", "c"), Y = "a"))),
  make_case("restriction_empty", list(list(X = "a"), list(X = c("b", "c"), Y = "a"))),
  make_case("generated_unit", list(list(X = "a", Y = c("a", "b")), list(X = "b", Y = "a"))),
  make_case("three_range_comparison", list(list(X = c("a", "b"), Y = "a"),
    list(X = c("a", "b"), Y = c("b", "c")), list(X = "a", Y = c("c", "d")))),
  make_case("crossing_ranges", list(list(X = c("a", "b"), Y = c("a", "d")),
    list(X = c("b", "c"), Y = c("b", "d")), list(X = c("c", "d"), Y = c("a", "b")))),
  make_case("resolution_unit", list(list(X = "a", Y = "a"), list(X = "b", Y = "b"),
    list(Y = c("a", "b")))),
  make_case("hidden_chain", list(list(X = c("a", "b"), Y = c("a", "b")),
    list(X = "a", Z = c("a", "b")), list(X = "b", Z = c("b", "c")),
    list(Y = c("a", "b"), Z = c("a", "c")))),
  make_case("unit_hla", list(list(X = c("a", "b")),
    list(X = "a", Y = c("a", "b")), list(X = "b", Y = c("c", "d")))),
  make_case("symbol_order", list(list(Z = c("a", "b"), X = "b", Y = c("a", "c")),
    list(Y = c("b", "d"), X = c("b", "c")), list(Z = "b", Y = "c"))),
  make_case("duplicate_clauses", list(list(Y = "b", X = c("a", "b")),
    list(Y = "b", X = c("a", "b")))),
  make_case("identity_range_order", list(list(X = c("b", "a", "c")))))

# This bank is independently generated; it imports no earlier campaign cases.
# Domain sizes are deliberately larger than the number of values separated by
# some inputs, so quotienting can merge actual membership cells.
n_random = as.integer(Sys.getenv("CNF_SYMMETRY_RANDOM", "600"))
for (trial in seq_len(n_random)) {
  n_symbols = sample.int(5L, 1L) + 1L
  syms = paste0("V", seq_len(n_symbols))
  doms = setNames(lapply(syms, function(s) letters[seq_len(sample.int(4L, 1L) + 2L)]), syms)
  n_clauses = sample.int(12L, 1L)
  clauses = lapply(seq_len(n_clauses), function(ci) {
    width = if (runif(1) < 0.15) 1L else sample.int(min(n_symbols, 4L) - 1L, 1L) + 1L
    support = syms[sample.int(n_symbols, width)]
    setNames(lapply(support, function(s) {
      size = sample.int(length(doms[[s]]) - 1L, 1L)
      doms[[s]][sample.int(length(doms[[s]]), size)]
    }), support)
  })
  cases[[length(cases) + 1L]] = list(label = paste0("random:", trial), domains = doms, clauses = clauses)
}

summary = list(version = R.version.string, seed = seed, random_cases = n_random,
  source_sha256 = digest(file = "R/CnfFormula_simplify.R", algo = "sha256"),
  cases = length(cases), production_sites = length(production_sites),
  transformations = c(rename = 0L, refine = 0L, reorder = 0L, refine_reorder = 0L, quotient = 0L),
  baseline_events = 0L, paired_events = 0L, quotient_cells_merged = 0L)
site_values = lapply(production_sites, function(site) character())
first_events = list()
for (ci in seq_along(cases)) {
  case = cases[[ci]]
  original = symmetry_run(case, observed)
  summary$baseline_events = summary$baseline_events + length(original$trace)
  for (event in original$trace) {
    value = paste(capture.output(dput(event$value)), collapse = " ")
    site_values[[event$id]] = unique(c(site_values[[event$id]], value))
    key = paste(event$id, value, sep = ":")
    if (is.null(first_events[[key]])) first_events[[key]] = ci
  }
  identity_fibers = symmetry_fibers(case)
  expected = symmetry_project(original$output, identity_fibers)
  for (variant in c("rename", "refine", "reorder", "refine_reorder")) {
    fibers = symmetry_fibers(case, split = variant %in% c("refine", "refine_reorder"), rename = variant == "rename")
    transformed_case = symmetry_transform(case, fibers, reorder = variant %in% c("reorder", "refine_reorder"))
    transformed = symmetry_run(transformed_case, observed)
    if (!identical(original$trace, transformed$trace) ||
        !identical(expected, symmetry_project(transformed$output, fibers))) {
      saveRDS(list(case = case, transformed_case = transformed_case, fibers = fibers,
        original = original, transformed = transformed, variant = variant),
        file.path(out_dir, paste0("unexpected_difference_", suffix, ".rds")))
      stop("Symmetry mismatch: ", case$label, " / ", variant)
    }
    summary$transformations[[variant]] = summary$transformations[[variant]] + 1L
    summary$paired_events = summary$paired_events + length(transformed$trace)
  }
  quotient = symmetry_quotient(case)
  collapsed = symmetry_run(quotient$case, observed)
  stopifnot(identical(original$trace, collapsed$trace),
    identical(symmetry_project(original$output, quotient$fibers),
      symmetry_project(collapsed$output, symmetry_fibers(quotient$case))))
  summary$transformations[["quotient"]] = summary$transformations[["quotient"]] + 1L
  summary$paired_events = summary$paired_events + length(collapsed$trace)
  summary$quotient_cells_merged = summary$quotient_cells_merged +
    sum(lengths(case$domains) - lengths(quotient$case$domains))
  if (ci %% 100L == 0L) cat("checked", ci, "cases and", sum(summary$transformations), "transformations\n")
}

# Constants and the empty conjunction are included separately: there is no
# nontrivial value quotient, but the kernel has dedicated early-return paths.
for (constant in list(TRUE, FALSE, list())) {
  case = list(domains = list(), clauses = constant)
  result = symmetry_run(case, observed)
  stopifnot(identical(result$output, if (!length(constant)) TRUE else constant))
}
summary$constant_controls = 3L

# Deliberate controls operate on in-memory source copies and leave output
# unchanged. A result-only metamorphic check could not detect any of them.
insert_probe = function(statement) {
  probe = plain_simplify
  body(probe) = as.call(c(list(as.name("{"), statement), as.list(body(probe))[-1L]))
  symmetry_instrument(probe)
}
control_case = make_case("observer_control", list(list(X = c("a", "b"), Y = "a"),
  list(X = c("b", "c"), Y = "b")))
controls = list(
  cardinality = list(probe = quote(if (length(entries[[1L]][[1L]]) == length(entries[[2L]][[1L]])) invisible(NULL)),
    fibers = symmetry_fibers(control_case, split = TRUE), reorder = FALSE),
  short_circuit_label = list(probe = quote(if (("a" %in% entries[[1L]][[1L]]) || TRUE) invisible(NULL)),
    fibers = symmetry_fibers(control_case, rename = TRUE), reorder = FALSE),
  first_value_order = list(probe = quote(if (entries[[1L]][[1L]][[1L]] == "a") invisible(NULL)),
    fibers = symmetry_fibers(control_case), reorder = TRUE))
summary$observer_controls = list()
for (name in names(controls)) {
  control = controls[[name]]
  probe = insert_probe(control$probe)
  transformed_case = symmetry_transform(control_case, control$fibers)
  if (control$reorder) {
    transformed_case$domains = lapply(transformed_case$domains, rev)
    transformed_case$clauses = lapply(transformed_case$clauses, function(clause) lapply(clause, rev))
  }
  one = symmetry_run(control_case, probe)
  two = symmetry_run(transformed_case, probe)
  stopifnot(identical(symmetry_project(one$output, symmetry_fibers(control_case)),
    symmetry_project(two$output, control$fibers)), !identical(one$trace, two$trace))
  at = symmetry_trace_difference(one$trace, two$trace)
  summary$observer_controls[[name]] = list(first_difference = at,
    site = symmetry_sites[[one$trace[[at]]$id]],
    original = one$trace[[at]]$value, transformed = two$trace[[at]]$value)
}

# A projection alone can conceal a partial fiber. This deliberate bad result
# must be rejected even though q(a_1) would equal the expected quotient value.
partial_map = list(X = list(a = c("a_1", "a_2"), b = "b_1"))
partial = tryCatch(symmetry_project(list(list(X = "a_1")), partial_map), error = identity)
stopifnot(inherits(partial, "error"))
summary$partial_fiber_control = TRUE

# Collect exactly the inspected value-cardinality and HLA decisions, including
# the guarded equality inside the initial comparison (which is not an if).
value_predicates = c("!length(unit_isct)",
  "length(unit_domains[[nu]]) == length(unit[[1L]])",
  "length(clause[[symbol_idx]]) == length(restringent)",
  "length(clause[[symbol_idx]]) == clause_symbol_length_before",
  "!length(clause[[symbol_idx]])",
  "length(range_outer) == length(range_inner)",
  "length(range_new) == length(universe[[symbol]])")
predicate_sites = which(vapply(production_sites, function(site) {
  site$expression %in% value_predicates && site$kind %in% c("if", "==")
}, FALSE))
summary$value_predicates = lapply(predicate_sites, function(i) {
  c(production_sites[[i]], list(observed = site_values[[i]]))
})
summary$site_kind_counts = as.list(table(vapply(production_sites, function(site) site$kind, "")))
summary$success = TRUE
json_summary = summary
json_summary$transformations = as.list(summary$transformations)
write_json(json_summary, file.path(out_dir, paste0("results_", suffix, ".json")), pretty = TRUE, auto_unbox = TRUE)
saveRDS(list(summary = summary, sites = production_sites, observed_values = site_values,
  cases = cases, first_events = first_events), file.path(out_dir, paste0("checks_", suffix, ".rds")))
cat("PASS", suffix, ":", length(cases), "cases,", sum(summary$transformations),
  "paired transformations,", summary$baseline_events + summary$paired_events,
  "source events; all three deliberate schedule controls detected\n")
