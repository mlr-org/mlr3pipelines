# Independent construction and complete small-instance truth/witness checks.
# Uses an actual installed package, not the repeated-pass Python generator.
.libPaths(c(normalizePath("attic/cnf_verify3/execution_modes/library"), .libPaths()))
suppressPackageStartupMessages(library(mlr3pipelines))
save_dir = "attic/cnf_verify3/quotient_review"

make_family = function(n, forward = FALSE) {
  domains = c(list(G = paste0("g", 0:n), T = c("0", "1")),
    setNames(rep(list(c("0", "1")), n + 2L), paste0("S", 0:(n + 1L))))
  clauses = list(A0 = list(S0 = "0", S1 = "0"))
  for (i in 0:(n - 1L)) clauses[[paste0("B", i)]] = setNames(list("1", "1", paste0("g", i)), c(paste0("S", i), "T", "G"))
  indices = if (forward) seq_len(n) else n:1L
  for (i in indices) clauses[[paste0("A", i)]] = setNames(list("0", "0", "0", paste0("g", 0:(i - 1L))),
    c(paste0("S", i), paste0("S", i + 1L), "T", "G"))
  list(domains = domains, clauses = clauses)
}
truth = function(clauses, assignments) {
  if (is.logical(clauses)) return(rep(as.vector(clauses), nrow(assignments)))
  result = rep(TRUE, nrow(assignments))
  for (clause in clauses) {
    value = rep(FALSE, nrow(assignments))
    for (i in seq_along(clause)) value = value | assignments[[names(clause)[[i]]]] %in% clause[[i]]
    result = result & value
  }
  result
}
holds = function(clause, point) any(vapply(names(clause), function(s) point[[s]] %in% clause[[s]], logical(1)))
mass = function(clauses) sum(vapply(clauses, function(cl) sum(lengths(cl)), integer(1)))
key = function(clause) paste(vapply(sort(names(clause)), function(s) paste(s, paste(sort(clause[[s]]), collapse = ","), sep = "="), character(1)), collapse = ";")
keyset = function(clauses) sort(vapply(clauses, key, character(1)))
clause_id = function(clause) {
  selectors = names(clause)[startsWith(names(clause), "S")]
  indices = as.integer(sub("S", "", selectors))
  if (length(indices) == 1L) {
    stopifnot(identical(clause[[selectors]], "1"))
    paste0("B", indices)
  } else {
    stopifnot(length(indices) == 2L, diff(sort(indices)) == 1L,
      all(vapply(selectors, function(s) identical(clause[[s]], "0"), logical(1))))
    paste0("A", min(indices))
  }
}
objects = function(family) {
  universe = CnfUniverse()
  symbols = lapply(names(family$domains), function(s) CnfSymbol(universe, s, family$domains[[s]]))
  names(symbols) = names(family$domains)
  lapply(family$clauses, function(cl) CnfClause(lapply(names(cl), function(s) CnfAtom(symbols[[s]], cl[[s]]))))
}
selectors = function(n, default = "0") setNames(rep(default, n + 2L), paste0("S", 0:(n + 1L)))
clause_witness = function(n, name) {
  i = as.integer(sub("^[AB]", "", name))
  if (startsWith(name, "A")) {
    point = c(G = paste0("g", n), T = "1", selectors(n))
    point[paste0("S", c(i, i + 1L))] = "1"
  } else {
    point = c(G = if (i == 0L) "g1" else "g0", T = "0", selectors(n, "1"))
    point[paste0("S", if (i == 0L) c(0L, 1L) else c(0L, i))] = "0"
  }
  point
}
literal_witness = function(n, name, symbol, value) {
  i = as.integer(sub("^[AB]", "", name))
  if (startsWith(name, "A")) {
    point = c(G = paste0("g", n), T = "1", selectors(n))
    if (symbol == "G") {
      point[["G"]] = value
      point[paste0("S", c(i, i + 1L))] = "1"
    } else {
      stopifnot(startsWith(symbol, "S"))
      point[paste0("S", c(i, i + 1L))] = "1"
      point[[symbol]] = "0"
    }
  } else if (symbol == "T") {
    point = c(G = paste0("g", n), T = "1", selectors(n))
  } else if (startsWith(symbol, "S")) {
    point = clause_witness(n, name)
    point[[symbol]] = "1"
  } else {
    stopifnot(symbol == "G", i <= 1L)
    point = c(G = value, T = "0", selectors(n, "1"))
    point[[paste0("S", i)]] = "0"
  }
  point
}

stats = c(frontier_states = 0L, clause_witnesses = 0L, literal_witnesses = 0L,
  public_reverse_passes = 0L, public_forward_passes = 0L, checked_truth_rows = 0L)
records = list()
for (n in 1:6) {
  family = make_family(n)
  assignments = expand.grid(family$domains, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  expected = truth(family$clauses, assignments)
  stopifnot(any(expected), mass(family$clauses) == 2L + 6L * n + n * (n + 1L) / 2L)
  current = objects(family)
  current_plain = family$clauses
  reverse_masses = mass(current_plain)
  for (k in 0:n) {
    frontier = family$clauses
    if (k > 0L) for (i in seq_len(k)) frontier[[paste0("A", i)]][["T"]] = NULL
    for (name in names(frontier)) {
      point = clause_witness(n, name)
      stopifnot(all(vapply(names(family$domains), function(s) point[[s]] %in% family$domains[[s]], logical(1))))
      stopifnot(!holds(frontier[[name]], point), all(vapply(frontier[names(frontier) != name], holds, logical(1), point)))
      stats[["clause_witnesses"]] = stats[["clause_witnesses"]] + 1L
      for (s in names(frontier[[name]])) for (value in frontier[[name]][[s]]) {
        if (startsWith(name, "A") && s == "T") next
        if (startsWith(name, "B") && s == "G" && as.integer(sub("B", "", name)) >= 2L) next
        point = literal_witness(n, name, s, value)
        stopifnot(all(vapply(frontier, holds, logical(1), point)))
        reduced = frontier[[name]]
        reduced[[s]] = setdiff(reduced[[s]], value)
        stopifnot(!holds(reduced, point))
        stats[["literal_witnesses"]] = stats[["literal_witnesses"]] + 1L
      }
    }
    stats[["frontier_states"]] = stats[["frontier_states"]] + 1L
    output = CnfFormula(current)
    bare = c(output)
    if (k == n) stopifnot(identical(bare, current_plain))
    productive_count = min(k + 1L, n)
    expected_frontier = family$clauses
    for (i in seq_len(productive_count)) expected_frontier[[paste0("A", i)]][["T"]] = NULL
    stopifnot(identical(keyset(bare), unname(keyset(expected_frontier))))
    ids_before = vapply(current_plain, clause_id, character(1))
    ids_after = vapply(bare, clause_id, character(1))
    stopifnot(identical(unname(ids_after), unname(ids_before[order(lengths(current_plain))])))
    stopifnot(identical(truth(bare, assignments), expected))
    stats[["checked_truth_rows"]] = stats[["checked_truth_rows"]] + nrow(assignments)
    stats[["public_reverse_passes"]] = stats[["public_reverse_passes"]] + 1L
    reverse_masses = c(reverse_masses, mass(bare))
    current = as.list(output)
    current_plain = bare
  }
  stopifnot(identical(diff(reverse_masses), c(rep(-1L, n), 0L)))
  forward_family = make_family(n, forward = TRUE)
  forward = CnfFormula(objects(forward_family))
  forward_check = CnfFormula(as.list(forward))
  stopifnot(identical(c(forward), c(forward_check)), identical(keyset(c(forward)), keyset(current_plain)),
    identical(truth(c(forward), assignments), expected))
  stats[["public_forward_passes"]] = stats[["public_forward_passes"]] + 2L
  stats[["checked_truth_rows"]] = stats[["checked_truth_rows"]] + 2L * nrow(assignments)
  records[[as.character(n)]] = list(n = n, reverse_masses = reverse_masses,
    forward_masses = c(mass(forward_family$clauses), mass(c(forward)), mass(c(forward_check))),
    input_models = sum(expected), worlds = nrow(assignments))
  cat("n=", n, ": reverse mass ", paste(reverse_masses, collapse = " -> "),
    "; forward deletes ", mass(forward_family$clauses) - mass(c(forward)), " values; ",
    nrow(assignments), " complete assignments.\n", sep = "")
}

# Direct independent check of R's manual-queue matrix enumeration order.
for (rows in 1:15) for (cols in 1:15) {
  m = matrix(seq_len(rows * cols) %% 3L == 0L, nrow = rows)
  positions = which(m, arr.ind = TRUE)
  stopifnot(!is.unsorted(positions[, 2L]))
}

# The minimized member separately exercises a final sort-only pass, which the
# main equal-length family does not need after its last productive pass.
saved = jsonlite::fromJSON("attic/cnf_verify3/repeated_passes/minimized_three_pass.json", simplifyVector = FALSE)
minimal = list(domains = lapply(saved$domains, unlist, use.names = FALSE),
  clauses = lapply(saved$clauses, function(cl) lapply(cl, unlist, use.names = FALSE)))
assignments = expand.grid(minimal$domains, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
expected = truth(minimal$clauses, assignments)
current = objects(minimal)
previous_plain = unname(minimal$clauses)
minimal_masses = mass(previous_plain)
storage_changes = logical()
for (pass in 1:5) {
  output = CnfFormula(current)
  plain = c(output)
  minimal_masses = c(minimal_masses, mass(plain))
  storage_changes = c(storage_changes, !identical(plain, previous_plain))
  stopifnot(identical(truth(plain, assignments), expected))
  previous_plain = plain
  current = as.list(output)
}
stopifnot(identical(minimal_masses, c(22L, 21L, 20L, 19L, 19L, 19L)),
  identical(storage_changes, c(TRUE, TRUE, TRUE, TRUE, FALSE)))
cat("Minimized member: mass", paste(minimal_masses, collapse = " -> "),
  "; fourth call sorts only; fifth call is identical;", nrow(assignments), "complete assignments.\n")
cat("R:", R.version.string, "; package:", as.character(packageVersion("mlr3pipelines")), "\n")
print(stats)
saveRDS(list(stats = stats, records = records, R = R.version.string,
  package = as.character(packageVersion("mlr3pipelines")), minimized = list(mass = minimal_masses,
    storage_changes = storage_changes, worlds = nrow(assignments))), file.path(save_dir, "repeated_checks.rds"))
cat("All independent public-call, full-truth, storage-order, and witness checks passed.\n")
