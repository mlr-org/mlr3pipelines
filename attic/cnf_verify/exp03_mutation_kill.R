# Mutation testing of simplify_cnf: seed deliberate bugs ("mutants") into
# R/CnfFormula_simplify.R, then run the *same* fuzzer distribution as
# exp02_fuzz_baseline.R against each mutant.
#
# Purpose: measure the *detector power* of the verification approach. A
# soundness mutant that survives thousands of trials means the fuzzers explore
# that code region too weakly -- and a hypothetical real bug there could have
# been missed by all previous fuzzing campaigns. Control mutants (disabling
# elimination passes) must NOT be flagged, validating that the oracle does not
# produce false positives (all elimination passes are redundancy-only).
#
# Parameters (env vars):
#   CNF_MUT_BUDGET (default 4000)  max fuzz trials per mutant
#   CNF_CORES      (default 16)
#   CNF_SEED       (default 1)
#   CNF_TIMEOUT    (default 20)    per-trial timeout seconds (mutants can hang)
#
# Run: Rscript attic/cnf_verify/exp03_mutation_kill.R

Sys.setenv(CNF_TIMEOUT = Sys.getenv("CNF_TIMEOUT", "20"))
source(file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "harness.R"))
source_cnf()

simplify_src_file = file.path(CNF_SRC_DIR, "CnfFormula_simplify.R")
simplify_src = paste(readLines(simplify_src_file), collapse = "\n")

# each mutant: id, expect ("kill": soundness bug, fuzzer must find it;
# "survive": completeness-only change, fuzzer must NOT flag it;
# "info": interesting either way -- violates internal invariants, outcome informative),
# pattern (exact unique substring), replacement
mutants = list(
  list(id = "M01_unit_merge_union", expect = "kill",
    pattern = "unit_isct = prev_unit[prev_unit %in% unit[[1L]]]",
    replacement = "unit_isct = unique(c(prev_unit, unit[[1L]]))"),
  list(id = "M02_drop_unit_contradiction", expect = "kill",
    pattern = "if (!length(unit_isct)) return(TRUE)  # signal that we have a contradiction and can exit",
    replacement = "# contradiction check removed (mutant)"),
  list(id = "M03_subsume_also_subset", expect = "kill",
    pattern = "if (length(clause[[symbol_idx]]) == length(restringent)) {\n      # If the lengths match",
    replacement = "if (length(clause[[symbol_idx]]) == length(restringent) || length(clause[[symbol_idx]]) == clause_symbol_length_before) {\n      # If the lengths match"),
  list(id = "M04_subset_direction_flip", expect = "kill",
    pattern = "        inner_subset_of_outer = all(range_inner %in% range_outer)",
    replacement = "        inner_subset_of_outer = all(range_outer %in% range_inner)"),
  list(id = "M05_sse_restrict_swap", expect = "kill",
    pattern = "      adr = apply_domain_restriction(available[[meta_idx_other]], symbol_to_restrict, entries[[available[[meta_idx]]]][[symbol_to_restrict]], FALSE)",
    replacement = "      adr = apply_domain_restriction(available[[meta_idx]], symbol_to_restrict, entries[[available[[meta_idx_other]]]][[symbol_to_restrict]], FALSE)"),
  list(id = "M06_hla_literal_not_complement", expect = "kill",
    pattern = "      range_new = c(range_old, char_setdiff(universe[[symbol]], c(range_old, entries[[clause_idx_other]][[symbol]])))",
    replacement = "      range_new = unique(c(range_old, entries[[clause_idx_other]][[symbol]]))"),
  list(id = "M07_hla_taut_eager_nonunit", expect = "kill",
    pattern = "if (length(range_new) == length(universe[[symbol]])) {\n        # hidden tautology elimination\n\n        # We still need the symbol registry",
    replacement = "if (length(range_new) >= length(universe[[symbol]]) - 1L) {\n        # hidden tautology elimination\n\n        # We still need the symbol registry"),
  list(id = "M08_hla_taut_eager_unit", expect = "kill",
    pattern = "if (length(range_new) == length(universe[[symbol]])) {\n        # hidden tautology elimination\n        # no more need to update the unit registry",
    replacement = "if (length(range_new) >= length(universe[[symbol]]) - 1L) {\n        # hidden tautology elimination\n        # no more need to update the unit registry"),
  list(id = "M09_2nd_order_disjoint_drop", expect = "kill",
    pattern = "    if (any(clause_oneend_symbol_intersect %in% clause_twoends[[symbol_intersect]] &\n        !clause_oneend_symbol_intersect %in% entries[[idx_target]][[symbol_intersect]])) {\n      return(FALSE)\n    }",
    replacement = "    # disjointness check removed (mutant)"),
  list(id = "M10_restrict_union", expect = "kill",
    pattern = "    clause[[symbol_idx]] = char_intersect(clause[[symbol_idx]], restringent)",
    replacement = "    clause[[symbol_idx]] = char_union(clause[[symbol_idx]], restringent)"),
  list(id = "M11_subsume_eliminate_self", expect = "kill",
    pattern = "    if (rowsum == 0) {\n      eliminate_clause_update_sr(available[[meta_idx_other]])\n      return(NULL)\n    }",
    replacement = "    if (rowsum == 0) {\n      eliminate_clause_update_sr(available[[meta_idx]])\n      return(NULL)\n    }"),
  list(id = "M12_stale_symbol_registry", expect = "info",
    pattern = "    # remove from symbol registry of the symbol that went to 0\n    sr = symbol_registry[[symbol]]\n    symbol_registry[[symbol]] = sr[sr != clause_idx]",
    replacement = "    # (mutant: symbol registry not cleaned up)\n    sr = symbol_registry[[symbol]]"),
  list(id = "M13_unit_hla_count_no_adjust", expect = "info",
    pattern = "    not_subset_count = lengths(entries[remaining_nonunit_entries]) - (remaining_nonunit_entries %in% symbol_registry[[unitsymbol]])",
    replacement = "    not_subset_count = lengths(entries[remaining_nonunit_entries])"),
  list(id = "M14_use_inso_skip_all", expect = "info",
    pattern = "        s_clause_idx_meta = available_inverse[[s_clause_idx]]\n        if (s_clause_idx_meta <= meta_idx_outer && inso_column[[s_clause_idx_meta]] && !is_not_subset_of[[s_clause_idx_meta]][unit_idx_meta, nu]) next",
    replacement = "        s_clause_idx_meta = available_inverse[[s_clause_idx]]\n        if (s_clause_idx_meta <= meta_idx_outer) next"),
  list(id = "M15_ctrl_no_hla_nonunit", expect = "survive",
    pattern = "  for (clause_idx in remaining_nonunit_entries) {\n    clause = entries[[clause_idx]]\n\n    # index inside `is_not_subset_of`",
    replacement = "  for (clause_idx in head(remaining_nonunit_entries, 0L)) {\n    clause = entries[[clause_idx]]\n\n    # index inside `is_not_subset_of`"),
  list(id = "M16_ctrl_no_hla_unit", expect = "survive",
    pattern = "  for (clause_idx in remaining_unit_entries) {\n    clause = entries[[clause_idx]]\n    unitsymbol = names(clause)",
    replacement = "  for (clause_idx in head(remaining_unit_entries, 0L)) {\n    clause = entries[[clause_idx]]\n    unitsymbol = names(clause)"),
  list(id = "M17_ctrl_no_2nd_order_trigger", expect = "survive",
    pattern = "  sse_to_trigger = which(!second_order_enabled_matrix, arr.ind = TRUE)",
    replacement = "  sse_to_trigger = which(matrix(FALSE, 0L, 0L), arr.ind = TRUE)")
)

apply_mutant = function(src, m) {
  n_occ = length(gregexpr(m$pattern, src, fixed = TRUE)[[1]])
  if (n_occ != 1 || !grepl(m$pattern, src, fixed = TRUE)) {
    stop(sprintf("mutant %s: pattern occurs %d times (must be exactly 1)", m$id, n_occ))
  }
  sub(m$pattern, m$replacement, src, fixed = TRUE)
}

# verify all patterns match before running anything
for (m in mutants) invisible(apply_mutant(simplify_src, m))
cat(sprintf("all %d mutant patterns match uniquely\n", length(mutants)))

budget = as.integer(Sys.getenv("CNF_MUT_BUDGET", "4000"))

run_mutant = function(m) {
  # runs in a forked child: redefining simplify_cnf is process-local
  mutated = apply_mutant(simplify_src, m)
  eval(parse(text = mutated), envir = globalenv())
  first_fail = NULL
  trials_run = 0L
  for (i in seq_len(budget)) {
    trials_run = i
    trial = gen_standard_trial(i)
    rec = tryCatch(
      check_simplify(trial$clauses, trial$uinfo, extra_info = list(trial = i, gen = trial$gen)),
      error = function(e) list(kind = "harness_error", message = conditionMessage(e))
    )
    if (!is.null(rec)) {
      first_fail = rec
      break
    }
  }
  list(id = m$id, expect = m$expect, trials = trials_run,
    caught = !is.null(first_fail),
    kind = if (!is.null(first_fail)) first_fail$kind else NA_character_,
    message = if (!is.null(first_fail)) first_fail$message else NA_character_,
    fail = first_fail)
}

cat(sprintf("running %d mutants x up to %d trials each\n", length(mutants), budget))
results = parallel::mclapply(mutants, run_mutant, mc.cores = min(n_cores(), length(mutants)))

verdict_ok = TRUE
cat(sprintf("%-32s %-8s %-8s %-7s %-10s %s\n", "mutant", "expect", "result", "trials", "kind", "note"))
for (r in results) {
  if (inherits(r, "try-error") || is.null(r$id)) { cat("worker failure\n"); verdict_ok = FALSE; next }
  result = if (r$caught) "KILLED" else "SURVIVED"
  bad = (r$expect == "kill" && !r$caught) || (r$expect == "survive" && r$caught)
  if (bad) verdict_ok = FALSE
  cat(sprintf("%-32s %-8s %-8s %-7d %-10s %s%s\n", r$id, r$expect, result, r$trials,
    if (is.na(r$kind)) "" else r$kind,
    if (is.na(r$message)) "" else substr(r$message, 1, 60),
    if (bad) "   <-- UNEXPECTED" else ""))
}
saveRDS(results, file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "results",
  sprintf("exp03_mutants_seed%d.rds", base_seed())))
cat("\n", if (verdict_ok) "MUTATION TESTING: all expectations met" else "MUTATION TESTING: UNEXPECTED OUTCOMES (see above)", "\n", sep = "")
