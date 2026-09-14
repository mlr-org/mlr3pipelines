record = readRDS("attic/cnf_verify3/structural_review/checks.rds")
evaluate = function(formula, rows) {
  out = rep(TRUE, nrow(rows))
  for (clause in formula) {
    cl_out = rep(FALSE, nrow(rows))
    for (s in names(clause)) cl_out = cl_out | rows[[s]] %in% clause[[s]]
    out = out & cl_out
  }
  out
}
single = record$counterexamples$single_cycle
rows = expand.grid(single$domains, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
before = evaluate(single$clauses, rows)
reduced = single$clauses
reduced[[1L]][["a1"]] = NULL
stopifnot(identical(evaluate(reduced, rows), before), sum(before) == 4L)

# The same forcing cycle is renamable Horn: interpret x=0 as the positive
# literal of x and ai=1 as the positive literal of each internal symbol.
positive = c(x = "0", a1 = "1", a2 = "1", a3 = "1")
stopifnot(all(vapply(single$clauses, function(cl) sum(vapply(names(cl), function(s)
  identical(cl[[s]], positive[[s]]), logical(1))) <= 1L, logical(1))))
record$counterexamples$single_cycle$redundant_value = list(clause = 1L, symbol = "a1", value = "1")
record$counterexamples$single_cycle$renamable_horn_positive_values = positive

# The opposed construction has two cycles in one connected component, so its
# unsatisfiability does not challenge the pseudoforest recognition theorem.
opposed = record$counterexamples$opposed_cycles
stopifnot(identical(opposed$incidence_ranks, 2L), opposed$models == 0L)
for (i in seq_along(opposed$clauses)) {
  grid = expand.grid(opposed$domains, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  stopifnot(any(evaluate(opposed$clauses[-i], grid)))
}

# Preserve explicitly named JSON statistics; remove an unused reserved count
# from the first run's RDS record without rerunning any production call.
record$stats = lapply(record$stats, function(x) x[names(x) != "nonunit_unit_overlap_outputs"])
saveRDS(record, "attic/cnf_verify3/structural_review/checks.rds")
record$stats = lapply(record$stats, as.list)
jsonlite::write_json(record, "attic/cnf_verify3/structural_review/checks.json", pretty = TRUE, auto_unbox = TRUE)
cat("The single incidence cycle has four models, all with x=1, and remains true-equivalent after removing a1=1 from clause 1.\n")
cat("The single cycle is also renamable Horn in the recorded polarities.\n")
cat("All eight opposed-cycle clause deletions restore satisfiability.\n")
