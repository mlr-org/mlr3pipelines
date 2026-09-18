# In-memory single-defect source controls for the indexing detector.
# Base R only. Run after/beside check_source_shapes.R from the repository root.

setup_lines = readLines("attic/cnf_verify3/index_contract/check_source_shapes.R")
setup_end = which(startsWith(setup_lines, "binary = list("))[[1L]] - 1L
eval(parse(text = setup_lines[seq_len(setup_end)]))
exact_source = paste(readLines(source_path), collapse = "\n")
original_observed = observed_simplify
control_results = list()

replace_one = function(text, old, new) {
  hits = gregexpr(old, text, fixed = TRUE)[[1L]]
  check(length(hits) == 1L && hits[[1L]] > 0L, "Control anchor was not unique")
  sub(old, new, text, fixed = TRUE)
}

run_control = function(label, source, entries, universe, expected) {
  observed_simplify <<- original_observed
  run_case(entries, universe, paste0(label, "_original"))
  audit$sites = list()
  location_used <<- new.env(parent = emptyenv())
  tree = annotate(parse(text = source)[[1L]])
  audit$site_counts = integer(length(audit$sites))
  env = new.env(parent = globalenv())
  eval(instrument(tree), env)
  audit$birth_indices = character()
  audit$input = list(entries = entries, universe = universe, label = label)
  err = tryCatch({ env$simplify_cnf(entries, universe); NULL }, error = identity)
  check(inherits(err, "error") && grepl(expected, conditionMessage(err), fixed = TRUE),
    paste0("Source control ", label, " did not fail as expected: ",
      if (is.null(err)) "no error" else conditionMessage(err)))
  control_results[[label]] <<- list(message = conditionMessage(err),
    entries = entries, universe = universe)
  cat(sprintf("REJECTED %s: %s\n", label, conditionMessage(err)))
  # The original instrumented closure refers to this table by site ID.
  audit$sites = original_sites
  audit$site_counts = integer(length(audit$sites))
}
original_sites = audit$sites

binary = list(X = c("0", "1"), Y = c("0", "1"), Z = c("0", "1"))
one_nonunit = list(list(X = "0", Y = "0"))

# The same guarded match occurs twice; intentionally remove only the first.
guard = "      if (is.na(hla_clause_idx)) break  # no more clauses to consider: all are either used or have no_subset_count > 1"
check(sum(gregexpr(guard, exact_source, fixed = TRUE)[[1L]] > 0L) == 2L,
  "Expected two HLA match guards")
run_control("remove_nonunit_match_guard", sub(guard, "", exact_source, fixed = TRUE),
  one_nonunit, binary, "missing numeric subscript")

unit_count = "    not_subset_count = lengths(entries[remaining_nonunit_entries]) - (remaining_nonunit_entries %in% symbol_registry[[unitsymbol]])"
run_control("corrupt_lazy_unit_count", replace_one(exact_source, unit_count,
  "    not_subset_count = rep(1L, length(remaining_nonunit_entries))"),
  list(list(X = "0"), list(Y = "0", Z = "0")), binary, "pivot-count bookkeeping failed")

# Generated independently by this stream, seed 19073, trial 42. The first
# observed future-matrix restriction needs the guard before a matrix lookup.
future_entries = list(
  list(s2 = "3", s1 = "1", s3 = "1"),
  list(s3 = "1", s4 = c("3", "1"), s1 = c("3", "1")),
  list(s2 = c("3", "1"), s4 = "3", s1 = c("1", "2"), s3 = "1"),
  list(s2 = "1", s3 = "1", s4 = "1", s1 = c("3", "1")),
  list(s2 = c("3", "2"), s4 = "2"), list(s2 = "2", s4 = "1"),
  list(s2 = c("3", "2"), s4 = "3", s3 = "1", s1 = c("3", "1")))
future_universe = list(s1 = c("1", "2", "3"), s2 = c("1", "2", "3"),
  s3 = c("1", "2"), s4 = c("1", "2", "3"))
run_control("remove_future_matrix_guards", gsub("    if (meta_idx > meta_idx_outer) return(FALSE)",
  "", exact_source, fixed = TRUE), future_entries, future_universe,
  "did not receive a matrix")

run_control("lookup_deleted_symbol_in_current_clause", gsub(
  "is_not_subset_of_col = match(symbol, colnames(is_not_subset_of[[meta_idx]]))",
  "is_not_subset_of_col = match(symbol, names(entries[[clause_idx]]))", exact_source, fixed = TRUE),
  list(list(X = "0", Y = "0", Z = "0"), list(X = "1", Y = "0", Z = "0")),
  binary, "missing numeric subscript")

allocation = "  not_subset_count = matrix(NA_integer_, nrow = length(available), ncol = length(available))"
run_control("drop_count_matrix_dimensions", replace_one(exact_source, allocation,
  paste(allocation, "  not_subset_count = as.vector(not_subset_count)", sep = "\n")),
  one_nonunit, binary, "did not receive a matrix")

run_control("give_lazy_inverse_wrong_domain", replace_one(exact_source,
  'delayedAssign("roe_inverse", match(seq_along(entries), remaining_nonunit_entries))',
  'delayedAssign("roe_inverse", match(seq_along(entries), remaining_nonunit_entries[0L]))'),
  list(list(X = c("0", "1")), list(X = "0", Y = "0")),
  list(X = c("0", "1", "2"), Y = c("0", "1")), "missing numeric subscript")

run_control("allow_nonunit_hla_self_decrement", replace_one(exact_source,
  "    is_not_subset_of[[meta_idx_outer]][meta_idx_outer, ] = FALSE",
  "    is_not_subset_of[[meta_idx_outer]][meta_idx_outer, ] = TRUE"),
  list(list(X = "0", Y = c("0", "1")), list(X = c("0", "1"), Y = "0")),
  list(X = c("0", "1", "2"), Y = c("0", "1", "2")), "missing numeric subscript")

tag = Sys.getenv("CNF_SHAPE_TAG", "r36")
saveRDS(list(controls = control_results, source_md5 = tools::md5sum(source_path),
  R_version = R.version.string), file.path(audit_dir, paste0("source_controls_", tag, ".rds")))
cat(sprintf("PASS: %i canonical originals accepted; all %i corrupted source copies rejected.\n",
  length(control_results), length(control_results)))
