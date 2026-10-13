source("attic/cnf_verify3/boolean_occurrences/common.R")
install_instrumentation()
context = boolean_context(6L)
cases = list()
add_case = function(words, label) cases[[length(cases) + 1L]] <<- list(words = words, label = label)

# A hidden implication around a cycle: the long clause contains the first
# and last positive literals, while donors propagate falsity through the
# middle. Rotate the repeated position through donor and target clauses.
for (n in 3:6) {
  cycle = lapply(seq_len(n - 1L), function(i) c(-i, i + 1L))
  cycle[[length(cycle) + 1L]] = c(-n, 1L)
  for (extra in list(c(1L, -2L, 3L), c(1L, 2L, 3L), c(-1L, 2L, -3L),
    c(1L, n), c(-1L, -n))) {
    base = c(cycle, list(extra))
    for (ci in seq_along(base)) for (p in seq_along(base[[ci]])) for (copies in 1:3) {
      words = base
      words[[ci]] = c(words[[ci]], rep(words[[ci]][[p]], copies))
      add_case(words, paste("hidden_cycle", n, ci, p, copies))
      add_case(lapply(rev(words), rev), paste("hidden_cycle_reversed", n, ci, p, copies))
    }
  }
}

# Covers every literal sign combination in a three-step HLA ladder. Each
# donor has one added duplicate position; the target keeps one or two
# duplicates so source-column and target-range representations both vary.
for (a in c(-1L, 1L)) for (b in c(-2L, 2L)) for (c in c(-3L, 3L)) {
  for (d in c(-4L, 4L)) for (e in c(-5L, 5L)) {
    base = list(c(a, b, c), c(-c, d), c(-d, e), c(-e, a), c(-b, -a))
    for (duplicate_clause in seq_along(base)) for (copies in 1:3) {
      words = base
      words[[duplicate_clause]] = c(words[[duplicate_clause]], rep(words[[duplicate_clause]][[1L]], copies))
      add_case(words, "hla_ladder")
    }
  }
}

started = proc.time()[[3L]]
for (i in seq_along(cases)) {
  one_case(cases[[i]]$words, context, cases[[i]]$label)
  if (i %% 100L == 0L) {
    cat("checked", i, "of", length(cases), "elapsed", proc.time()[[3L]] - started, "\n")
    flush.console()
  }
}
result = list(R = R.version.string, cases = length(cases), totals = audit$totals,
  examples = audit$examples, seconds = proc.time()[[3L]] - started)
saveRDS(result, file.path(audit_dir, paste0("hla", Sys.getenv("CNF_BOOLEAN_SUFFIX"), ".rds")))
capture.output(str(result[c("R", "cases", "totals", "seconds")]),
  file = file.path(audit_dir, paste0("hla", Sys.getenv("CNF_BOOLEAN_SUFFIX"), ".txt")))
print(result[c("R", "cases", "totals", "seconds")])
