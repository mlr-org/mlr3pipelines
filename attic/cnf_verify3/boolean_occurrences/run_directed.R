source("attic/cnf_verify3/boolean_occurrences/common.R")
install_instrumentation()
context = boolean_context(4L)
cases = list()
add_case = function(words, label) {
  cases[[length(cases) + 1L]] <<- list(words = words, label = label)
}

# First-copy deletion with an additional literal so the reverse first-order
# call need not immediately produce a unit. Nearby clauses vary each sign,
# both symbols, and ordering; their truth tables are evaluated positionally.
binary = list()
for (a in 1:3) for (b in seq.int(a, 3L)) {
  if (a == b) next
  for (sa in c(-1L, 1L)) for (sb in c(-1L, 1L)) binary[[length(binary) + 1L]] = c(sa * a, sb * b)
}
stopifnot(length(binary) == 12L)
for (copies in 2:4) for (a in seq_along(binary)) for (b in seq_along(binary)) {
  core = list(c(-1L, 2L), c(rep(1L, copies), 2L, 3L))
  add_case(c(core, binary[c(a, b)]), paste("deletion_neighborhood", copies, a, b))
  if (copies == 2L) add_case(c(binary[c(b, a)], lapply(rev(core), rev)), paste("deletion_reordered", a, b))
}

# Count-two rows whose exceptional columns have the SAME symbol name.
# The last donor may have an unrelated exceptional symbol; repeated match()
# must not be treated as certifying that it lacks such an exception.
for (s1 in c(-1L, 1L)) for (s2 in c(-1L, 1L)) for (s3 in c(-1L, 1L)) {
  core = list(c(s1, s1, s3 * 3L), c(-s1, -s1, s3 * 3L), c(s2, 2L))
  for (ordering in list(1:3, c(2L, 1L, 3L), c(3L, 2L, 1L))) {
    add_case(core[ordering], paste("same_name_exceptions", s1, s2, s3, paste(ordering, collapse = "")))
    for (extra in binary) add_case(c(core[ordering], list(extra)), "same_name_with_neighbor")
  }
}

# Genuine second-order resolution, with padding applied independently to
# either occurrence of each of the three essential clauses. The canonical
# instance derives S2 from (S1|S2), (!S1|S3), (S2|!S3).
for (first in 0:3) for (second in 0:3) for (third in 0:3) {
  for (positions in list(c(1L, 1L, 1L), c(1L, 2L, 1L), c(2L, 1L, 2L), c(2L, 2L, 2L))) {
    base = list(c(1L, 2L), c(-1L, 3L), c(2L, -3L))
    padded = Map(function(word, copies, p) c(word, rep(word[[p]], copies)), base,
      c(first, second, third), positions)
    add_case(padded, paste("resolution_padding", first, second, third, paste(positions, collapse = "")))
    add_case(c(padded, list(c(-2L, 4L), c(-2L, -4L))), "resolution_then_contradiction")
  }
}

# Longer implication cycles and unit creation exercise eventual deletion of
# orphan-bearing clauses and nested propagation. Every repetition word is
# intentionally retained, even when it is semantically idempotent.
for (copies in 1:4) for (selected in 1:4) for (sign in c(-1L, 1L)) {
  cycle = list(c(-1L, 2L), c(-2L, 3L), c(-3L, 4L), c(-4L, 1L))
  cycle[[selected]] = c(cycle[[selected]], rep(cycle[[selected]][[1L]], copies))
  add_case(c(cycle, list(sign)), "cycle_with_initial_unit")
  add_case(c(cycle, list(c(1L, 2L), c(1L, -2L))), "cycle_with_derived_unit")
  add_case(c(cycle, list(c(1L, 1L), c(-1L, -1L))), "cycle_with_opposite_pseudounits")
}

# Widths are not capped at three: one- and two-symbol clauses with long
# occurrence words exercise the permanent-column argument and all-copy UP.
for (copies in c(2L, 3L, 4L, 7L, 12L, 24L)) {
  add_case(list(rep(1L, copies), -1L), "long_pseudounit_contradiction")
  add_case(list(c(rep(1L, copies), rep(2L, copies)), -1L), "long_initial_propagation")
  add_case(list(c(-1L, 2L), c(rep(1L, copies), 2L, 3L), c(-2L, -3L)), "long_orphan_birth")
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
saveRDS(result, file.path(audit_dir, paste0("directed", Sys.getenv("CNF_BOOLEAN_SUFFIX"), ".rds")))
capture.output(str(result[c("R", "cases", "totals", "seconds")]),
  file = file.path(audit_dir, paste0("directed", Sys.getenv("CNF_BOOLEAN_SUFFIX"), ".txt")))
print(result[c("R", "cases", "totals", "seconds")])
