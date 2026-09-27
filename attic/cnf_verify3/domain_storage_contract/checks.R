source("attic/cnf_verify3/domain_storage_contract/harness.R")
suppressPackageStartupMessages({ library(digest); library(jsonlite) })
version_tag = if (getRversion() < "4.0.0") "r36" else "r46"
seed = 860601L
set.seed(seed)

instrumented = domain_instrument(domain_plain)
observed = instrumented$fun
sites = instrumented$sites
site_kinds = vapply(sites, `[[`, "", "kind")
parsed = getParseData(parse("R/CnfFormula_simplify.R", keep.source = TRUE))
stopifnot(sum(site_kinds == "if") == sum(parsed$token == "IF"),
  sum(site_kinds == "for_sequence") == sum(parsed$token == "FOR"),
  sum(site_kinds == "repeat_iteration") == sum(parsed$token == "REPEAT"),
  sum(site_kinds == "&&_left") == sum(parsed$token == "AND2"),
  sum(site_kinds == "||_left") == sum(parsed$token == "OR2"))
write.table(do.call(rbind, lapply(sites, as.data.frame)),
  file.path(domain_here, "sites.tsv"), sep = "\t", quote = TRUE, row.names = FALSE)

# Reuse concrete inputs, independently rechecking all premises and outcomes.
# Their generation or previous instrumentation is not used by this observer.
saved = list()
for (path in c("attic/cnf_verify3/root/branch_obligations.rds",
    "attic/cnf_verify3/root/branch_dense/branch_obligations.rds")) {
  for (case in Filter(Negate(is.null), readRDS(path)$first_inputs)) {
    key = digest(list(case$domains, case$clauses), algo = "sha256")
    saved[[key]] = case
  }
}
stopifnot(length(saved) == 57L)

basic = list(
  list(label = "constant_false", domains = list(X = c("a", "b")), clauses = FALSE),
  list(label = "constant_true", domains = list(X = c("a", "b")), clauses = TRUE),
  list(label = "empty", domains = list(X = c("a", "b")), clauses = list()),
  list(label = "unit", domains = list(X = c("a", "b", "c")), clauses = list(list(X = c("b", "a")))),
  list(label = "contradictory_units", domains = list(X = c("a", "b", "c")),
    clauses = list(list(X = c("a", "b")), list(X = "c"))),
  list(label = "nonunit_hla_repetitions", domains = list(X = c("a", "b", "c"), Y = c("a", "b", "c")),
    clauses = list(list(X = c("a", "b"), Y = c("a", "b")), list(X = c("b", "c"), Y = "a"))),
  list(label = "unit_hla_repetitions", domains = list(X = c("a", "b", "c"), Y = c("a", "b", "c")),
    clauses = list(list(X = c("a", "b")), list(X = "a", Y = "a"), list(X = "b", Y = "b"))))

random_count = as.integer(Sys.getenv("DOMAIN_RANDOM_CASES", "200"))
generated = lapply(seq_len(random_count), function(i) {
  n = sample.int(5L, 1L) + 1L
  domains = setNames(lapply(seq_len(n), function(j) {
    # Include an empty string as a legal value, plus nonsyntactic values.
    c("", "a.b", "[c]", "d e", "z")[seq_len(sample.int(4L, 1L) + 1L)]
  }), c("X", "Y", "not syntactic", "a.b", "[", "names") [seq_len(n)])
  clauses = lapply(seq_len(sample.int(18L, 1L)), function(j) {
    width = if (i %% 2L) sample.int(min(n, 4L), 1L) else sample.int(min(n, 4L) - 1L, 1L) + 1L
    symbols = names(domains)[sample.int(n, width)]
    setNames(lapply(symbols, function(s) domains[[s]][sample.int(length(domains[[s]]),
      sample.int(length(domains[[s]]) - 1L, 1L))]), symbols)
  })
  list(label = paste0("independent_random_", i), domains = domains, clauses = clauses)
})
cases = c(basic, unname(saved), generated)
counts = c(cases = length(cases), runs = 0, paired_trace_checks = 0, events = 0,
  actual_write_events = 0, pair_rows = 0, unit_containments = 0,
  nonunit_hla = 0, unit_hla = 0, repeated_virtual_ranges = 0, named_virtual_ranges = 0,
  truth_assignments = 0, virtual_support_comparisons = 0)
if_seen = matrix(FALSE, length(sites), 2L)
first = list()
case_results = list()

for (ci in seq_along(cases)) {
  case = cases[[ci]]
  variants = domain_variants(case$domains)
  baseline = NULL
  for (kind in names(variants)) {
    result = domain_run(case, variants[[kind]], observed)
    if (is.null(baseline)) {
      baseline = result
      # Independent semantic enumeration once per input, followed by exact
      # output equality for every storage variant.
      assignments = expand.grid(lapply(case$domains, domain_support), stringsAsFactors = FALSE)
      for (ai in seq_len(nrow(assignments))) {
        assignment = as.list(assignments[ai, , drop = FALSE])
        stopifnot(identical(domain_truth(case$clauses, assignment), domain_truth(result$output, assignment)))
      }
      counts[["truth_assignments"]] = counts[["truth_assignments"]] + nrow(assignments)
      case_results[[ci]] = list(label = case$label, output = result$output,
        trace_sha256 = digest(result$trace, algo = "sha256"), events = length(result$trace), hla = length(result$hla))
    } else {
      stopifnot(identical(result$output, baseline$output), identical(result$trace, baseline$trace),
        identical(result$entry, baseline$entry), length(result$hla) == length(baseline$hla))
      for (hi in seq_along(result$hla)) {
        one = result$hla[[hi]]
        other = baseline$hla[[hi]]
        stopifnot(identical(one[c("kind", "clause_idx", "donor_idx", "symbol", "donor", "missing")],
          other[c("kind", "clause_idx", "donor_idx", "symbol", "donor", "missing")]),
          setequal(domain_support(one$old), domain_support(other$old)),
          setequal(domain_support(one$new), domain_support(other$new)))
        counts[["virtual_support_comparisons"]] = counts[["virtual_support_comparisons"]] + 1L
      }
      counts[["paired_trace_checks"]] = counts[["paired_trace_checks"]] + 1L
    }
    counts[["runs"]] = counts[["runs"]] + 1L
    counts[["events"]] = counts[["events"]] + length(result$trace)
    ids = vapply(result$trace, `[[`, 0L, 1L)
    counts[["actual_write_events"]] = counts[["actual_write_events"]] + sum(site_kinds[ids] == "actual_write")
    for (event in result$trace[site_kinds[ids] == "if"]) {
      if_seen[event[[1L]], as.integer(as.logical(event[[2L]])) + 1L] = TRUE
    }
    if (!is.null(result$entry)) counts[names(result$entry)] = counts[names(result$entry)] + result$entry
    for (hla in result$hla) {
      count_name = paste0(hla$kind, "_hla")
      counts[[count_name]] = counts[[count_name]] + 1L
      counts[["repeated_virtual_ranges"]] = counts[["repeated_virtual_ranges"]] + hla$duplicate
      counts[["named_virtual_ranges"]] = counts[["named_virtual_ranges"]] + hla$named
      first_name = paste(hla$kind, kind, sep = "_")
      if (is.null(first[[first_name]]) && (hla$duplicate || hla$named)) first[[first_name]] = list(case = case, observation = hla)
    }
  }
  if (ci %% 10L == 0L) cat("Completed", ci, "of", length(cases), "inputs;", counts[["events"]], "events\n")
}
stopifnot(sum(if_seen) == 212L, counts[["repeated_virtual_ranges"]] > 0,
  counts[["named_virtual_ranges"]] > 0, counts[["unit_hla"]] > 0, counts[["nonunit_hla"]] > 0)

# Actual-source controls, made only in in-memory functions. One adds a harmless
# explicit branch; one reverses domain difference into intersection. The first
# must preserve outputs but change event streams; the second must be caught by
# the independently recomputed selected-donor/multiplicity checks.
replace_expression = function(expr, target, replacement) {
  if (identical(expr, target)) return(replacement)
  if (is.call(expr)) for (i in seq_along(expr)[-1L]) {
    if (is.call(expr[[i]])) expr[[i]] = replace_expression(expr[[i]], target, replacement)
  }
  expr
}
control_case = first[["nonunit_repeated_named"]]$case
control_domains = domain_variants(control_case$domains)$repeated_named
control_built = domain_build(control_case$clauses, control_domains)
unmutated = domain_run(control_case, control_domains, observed)
changed_schedule = domain_plain
body(changed_schedule) = replace_expression(body(changed_schedule), quote(x[!x %in% y]),
  quote({ if (FALSE) stop("unreachable calibration"); x[!x %in% y] }))
schedule_instrumented = domain_instrument(changed_schedule)
schedule_observed = schedule_instrumented$fun
domain_reset()
schedule_output = schedule_observed(control_case$clauses, control_built$universe)
stopifnot(identical(domain_bare(schedule_output), unmutated$output),
  !identical(domain_take_trace(), unmutated$trace))
calibration_sites = which(vapply(schedule_instrumented$sites, function(s) {
  s$kind == "if" && s$expression == "FALSE" && s$owner == "char_setdiff"
}, FALSE))
stopifnot(length(calibration_sites) == 1L,
  any(vapply(domain_take_trace(), function(e) e[[1L]] == calibration_sites, FALSE)))
changed_difference = domain_plain
body(changed_difference) = replace_expression(body(changed_difference), quote(x[!x %in% y]), quote(x[x %in% y]))
difference_observed = domain_instrument(changed_difference)$fun
domain_reset()
difference_error = tryCatch({ difference_observed(control_case$clauses, control_built$universe); NULL },
  error = function(e) conditionMessage(e))
stopifnot(is.character(difference_error), grepl("missing|multiplicity|length\\(new\\)", difference_error))

# The tempting standalone cardinality iff is false when the required donor
# exception is absent. This is a local algebra control, not a kernel input.
u = c("a", "a", "b")
c_old = "a"
d = character()
c_new = c(c_old, u[!u %in% c(c_old, d)])
standalone_false_negative = list(domain = u, old = c_old, donor = d, new = c_new,
  covers = all(u %in% c_new), length_equal = length(c_new) == length(u))
stopifnot(standalone_false_negative$covers, !standalone_false_negative$length_equal)

report = list(source_sha256 = digest(file = "R/CnfFormula_simplify.R", algo = "sha256"),
  version = R.version.string, checkmate_version = as.character(packageVersion("checkmate")),
  seed = seed, saved_cases = length(saved), basic_cases = length(basic), generated_cases = random_count,
  variants = names(domain_variants(basic[[1L]]$domains)), counts = as.list(counts),
  sites = as.list(table(site_kinds)), feasible_if_outcomes = sum(if_seen),
  controls = list(schedule_only_detected = TRUE, difference_mutation_detected = difference_error,
    standalone_full_support_length_equivalence_refuted = standalone_false_negative))
write_json(report, file.path(domain_here, paste0("results_", version_tag, ".json")), pretty = TRUE, auto_unbox = TRUE)
saveRDS(list(report = report, cases = cases, first = first, if_seen = if_seen, results = case_results),
  file.path(domain_here, paste0("checks_", version_tag, ".rds")))
cat(toJSON(report, pretty = TRUE, auto_unbox = TRUE), "\n")
