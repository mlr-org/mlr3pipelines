# Independent source-copy instrumentation for frozen unit-birth certificates.
# Only base R is required. No production file is modified, no earlier harness
# or random generator is imported, and the actual simplifier also runs without
# hooks so that instrumentation-induced output changes are detected.
#
# Run from the repository root:
#   CNF_REVIEW_TRIALS=5000 Rscript attic/cnf_verify3/review_semantics/check_source_births.R

review_dir = "attic/cnf_verify3/review_semantics"
source_path = "R/CnfFormula_simplify.R"
source(source_path)
plain_simplify = simplify_cnf
audit = new.env(parent = emptyenv())
audit$counts = list()
audit$first_deferred_skip = NULL

bump = function(name, amount = 1L) {
  previous = audit$counts[[name]]
  if (is.null(previous)) previous = 0L
  audit$counts[[name]] = previous + amount
}

set_equal = function(a, b) all(a %in% b) && all(b %in% a)

models = function(clauses, assignments) {
  if (is.logical(clauses)) return(rep(clauses, nrow(assignments)))
  answer = rep(TRUE, nrow(assignments))
  for (clause in clauses) {
    satisfied = rep(FALSE, nrow(assignments))
    for (symbol in names(clause)) {
      satisfied = satisfied | assignments[[symbol]] %in% clause[[symbol]]
    }
    answer = answer & satisfied
  }
  answer
}

check = function(condition, description) {
  if (!condition) {
    saveRDS(list(description = description, input = audit$input,
      births = audit$births, counts = audit$counts),
      file.path(review_dir, "source_failure.rds"))
    stop(description, call. = FALSE)
  }
}

review_semantic_hook = function(state) {
  check(identical(models(state$entries[!state$eliminated], audit$assignments),
    audit$initial_models), "A committed semantic mutation changed the model set")
  bump("semantic_mutations")
}

review_birth_hook = function(state, idx, symbol, unit) {
  key = as.character(idx)
  check(is.null(audit$births[[key]]), "A clause index registered twice")
  older = state$unit_domains[[symbol]]
  if (is.null(older)) older = state$universe[[symbol]]
  ghost_older = state$universe[[symbol]]
  for (birth in audit$births) {
    if (identical(birth$symbol, symbol)) ghost_older = intersect(ghost_older, birth$range)
  }
  check(set_equal(older, ghost_older), "Stored ambient domain differs from older births")
  birth = list(symbol = symbol, range = unit[[1L]], earlier = older,
    rank = length(audit$births) + 1L)
  audit$births[[key]] = birth
  bump("unit_births")

  if (is.null(state$is_not_subset_of)) return(invisible(NULL))
  idx_meta = state$available_inverse[[idx]]
  if (idx_meta > state$meta_idx_outer) return(invisible(NULL))
  for (source_meta in seq_along(state$available)) {
    source_idx = state$available[[source_meta]]
    if (source_idx == idx || state$eliminated[[source_idx]] || state$is_unit[[source_idx]]) next
    if (is.na(state$not_subset_count[source_meta, idx_meta])) next
    source_matrix = state$is_not_subset_of[[source_meta]]
    if (!symbol %in% colnames(source_matrix)) next
    if (source_matrix[idx_meta, symbol]) next
    values = state$entries[[source_idx]][[symbol]]
    check(all(values[values %in% birth$earlier] %in% birth$range),
      "Incoming FALSE comparison lacks a pre-birth certificate")
    bump("incoming_false_birth_certificates")
  }
}

review_skip_hook = function(state, idx, target, symbol) {
  birth = audit$births[[as.character(idx)]]
  check(!is.null(birth), "Skip used an index without a birth event")
  values = state$entries[[target]][[symbol]]
  check(all(values[values %in% birth$earlier] %in% birth$range),
    "A skip certificate needs a constraint born at or after its own birth")
  bump("frozen_birth_skips")
  if (!all(values %in% state$unit_domains[[symbol]])) {
    bump("physically_deferred_skips")
    if (is.null(audit$first_deferred_skip)) {
      audit$first_deferred_skip = list(input = audit$input, birth = birth,
        values = values, current_domain = state$unit_domains[[symbol]],
        target = target, idx = idx)
    }
  }
}

review_hla_boundary_hook = function(state) {
  live = which(!state$eliminated & !state$is_unit)
  for (birth in audit$births) for (idx in live) {
    values = state$entries[[idx]][[birth$symbol]]
    check(all(values %in% birth$range),
      "A live clause is outside a birth constraint at HLA entry")
    bump("final_birth_containment")
  }
  bump("hla_boundaries")
}

review_unit_hla_hook = function(state) {
  for (i in seq_along(state$remaining_nonunit_entries)) {
    donor = state$entries[[state$remaining_nonunit_entries[[i]]]]
    direct = vapply(names(donor), function(s) !all(donor[[s]] %in% state$clause[[s]]), logical(1))
    row = state$is_not_subset_of_unit[[i]]
    if (is.null(row)) {
      row = names(donor) != state$unitsymbol
      bump("unallocated_unit_hla_rows")
    }
    check(identical(unname(row), unname(direct)),
      "Lazy unit-HLA row does not describe the current virtual clause")
    check(state$not_subset_count[[i]] == sum(direct),
      "Unit-HLA count does not describe the current virtual clause")
    bump("unit_hla_rows")
  }
  bump("unit_hla_iterations")
}

replace_once = function(text, old, new) {
  matches = gregexpr(old, text, fixed = TRUE)[[1L]]
  stopifnot(length(matches) == 1L, matches[[1L]] > 0L)
  sub(old, new, text, fixed = TRUE)
}

src = paste(readLines(source_path), collapse = "\n")
src = replace_once(src, "    nu = names(unit)", paste(
  "    nu = names(unit)",
  "    review_birth_hook(parent.env(environment()), unit_idx, nu, unit)", sep = "\n"))
old = "        if (s_clause_idx_meta <= meta_idx_outer && inso_column[[s_clause_idx_meta]] && !is_not_subset_of[[s_clause_idx_meta]][unit_idx_meta, nu]) next"
new = paste(
  "        if (s_clause_idx_meta <= meta_idx_outer && inso_column[[s_clause_idx_meta]] && !is_not_subset_of[[s_clause_idx_meta]][unit_idx_meta, nu]) {",
  "          review_skip_hook(parent.env(environment()), unit_idx, s_clause_idx, nu)",
  "          next",
  "        }", sep = "\n")
src = replace_once(src, old, new)
src = replace_once(src, "  remaining_entries = which(!eliminated)[order(lengths(entries[!eliminated]), decreasing = TRUE)]",
  paste("  review_hla_boundary_hook(environment())",
    "  remaining_entries = which(!eliminated)[order(lengths(entries[!eliminated]), decreasing = TRUE)]", sep = "\n"))
src = replace_once(src, "      hla_clause_idx = match(TRUE, not_subset_count == 1L & !was_used)",
  paste("      review_unit_hla_hook(environment())",
    "      hla_clause_idx = match(TRUE, not_subset_count == 1L & !was_used)", sep = "\n"))

# All actual entries or eliminated mutations preserve the interpreted formula,
# including the intermediate state between writing the merged representative
# and marking its candidate eliminated. The two HLA writes occur at top level.
lines = strsplit(src, "\n", fixed = TRUE)[[1L]]
is_semantic_write = grepl("entries\\[\\[.*<<-|eliminated\\[\\[.*(<<-|= TRUE)", lines)
is_top_level = grepl("eliminated\\[\\[clause_idx\\]\\] = TRUE", lines)
output = character()
for (i in seq_along(lines)) {
  output = c(output, lines[[i]])
  if (is_semantic_write[[i]]) {
    output = c(output, if (is_top_level[[i]])
      "        review_semantic_hook(environment())" else
      "    review_semantic_hook(parent.env(environment()))")
  }
}
eval(parse(text = output))
instrumented_simplify = simplify_cnf

run_case = function(clauses, universe, label) {
  audit$input = list(clauses = clauses, universe = universe, label = label)
  audit$births = list()
  audit$assignments = expand.grid(universe, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  audit$initial_models = models(clauses, audit$assignments)
  result = instrumented_simplify(clauses, universe)
  uninstrumented = plain_simplify(clauses, universe)
  check(identical(result, uninstrumented), "Instrumentation changed returned storage")
  check(identical(models(result, audit$assignments), audit$initial_models),
    "Returned formula changed the model set")
  bump("source_cases")
  if (any(audit$initial_models)) bump("satisfiable_cases")
  invisible(result)
}

# A known deeply deferred canonical witness, copied as data only. It has a
# skip whose raw source is disjoint from the current effective unit. The new
# obligation being checked here is its *frozen pre-birth* certificate.
run_case(list(
  list(X0 = c("2", "6"), X1 = "6"),
  list(X0 = "2", X1 = c("6", "3")),
  list(X0 = "5", X1 = c("3", "0")),
  list(X1 = "3", X0 = c("5", "2")),
  list(X0 = "1", X1 = c("6", "5"))
), list(X0 = c("1", "2", "5", "6"), X1 = c("0", "3", "5", "6")), "deferred")

# Restricting donors under U can make U removable. This is sequential
# equivalence, although the original unrestricted donors do not imply U.
run_case(list(list(x = c("0", "1")),
  list(x = c("1", "2"), y = "0"),
  list(x = c("1", "2"), y = "1")),
  list(x = c("0", "1", "2"), y = c("0", "1")), "unit_hla_context")

seed = as.integer(Sys.getenv("CNF_REVIEW_SEED", "6137"))
trials = as.integer(Sys.getenv("CNF_REVIEW_TRIALS", "5000"))
set.seed(seed)
for (trial in seq_len(trials)) {
  n = sample(2:5, 1L)
  universe = setNames(lapply(seq_len(n), function(i) as.character(seq_len(sample(2:4, 1L)))),
    paste0("s", seq_len(n)))
  planted = if (trial %% 2L == 0L) lapply(universe, function(domain) sample(domain, 1L)) else NULL
  clauses = lapply(seq_len(sample(3:16, 1L)), function(i) {
    # Most cases start without units, so recursion has to create them.
    size_choices = seq.int(if (trial %% 5L == 0L) 1L else 2L, n)
    size = size_choices[[sample.int(length(size_choices), 1L)]]
    chosen = sample(names(universe), size)
    clause = setNames(lapply(chosen, function(s) {
      domain = universe[[s]]
      sample(domain, sample(seq_len(length(domain)), 1L))
    }), chosen)
    if (!is.null(planted)) {
      s = sample(chosen, 1L)
      clause[[s]] = union(clause[[s]], planted[[s]])
    }
    clause
  })
  run_case(clauses, universe, paste0("random_", seed, "_", trial))
  if (trial %% 250L == 0L) {
    cat("trials:", trial, "births:", audit$counts$unit_births,
      "frozen skips:", audit$counts$frozen_birth_skips, "\n")
  }
}
result = list(seed = seed, trials = trials, source_md5 = unname(tools::md5sum(source_path)),
  R_version = R.version.string, counts = audit$counts,
  first_deferred_skip = audit$first_deferred_skip)
saveRDS(result, file.path(review_dir, paste0("source_results_seed", seed, ".rds")))
print(result[c("seed", "trials", "source_md5", "R_version", "counts")])
cat("All source-copy and model checks passed.\n")
