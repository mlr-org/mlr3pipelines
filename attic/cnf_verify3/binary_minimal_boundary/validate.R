source("attic/cnf_verify3/operator_proof/bootstrap.R")

parity_cycle = function(n) {
  clauses = list()
  for (i in seq_len(n - 1L)) {
    clauses = c(clauses, list(c(-i, i + 1L), c(i, -(i + 1L))))
  }
  c(clauses, list(c(-1L, -n), c(1L, n)))
}

to_raw = function(clauses, symbols) {
  lapply(clauses, function(clause) {
    setNames(as.list(ifelse(clause > 0L, "1", "0")), symbols[abs(clause)])
  })
}

all_permutations = function(values) {
  if (length(values) == 1L) return(list(values))
  unlist(lapply(seq_along(values), function(i) {
    lapply(all_permutations(values[-i]), function(rest) c(values[[i]], rest))
  }), recursive = FALSE)
}

complement_distances = function(clauses, n) {
  literal_vertex = function(literal) 2L * (abs(literal) - 1L) + ifelse(literal > 0L, 2L, 1L)
  adjacency = matrix(FALSE, 2L * n, 2L * n)
  for (clause in clauses) {
    adjacency[literal_vertex(-clause[[1L]]), literal_vertex(clause[[2L]])] = TRUE
    adjacency[literal_vertex(-clause[[2L]]), literal_vertex(clause[[1L]])] = TRUE
  }
  distance = matrix(Inf, 2L * n, 2L * n)
  diag(distance) = 0
  distance[adjacency] = 1
  for (pivot in seq_len(2L * n)) {
    for (from in seq_len(2L * n)) for (to in seq_len(2L * n)) {
      distance[from, to] = min(distance[from, to], distance[from, pivot] + distance[pivot, to])
    }
  }
  stopifnot(all(is.finite(distance)))
  distance[cbind(seq_len(2L * n), rep(seq_len(n) * 2L, each = 2L) - rep(c(0L, 1L), n))]
}

for (n in 2:4) {
  symbols = paste0("x", seq_len(n))
  universe = make_universe(setNames(rep(list(c("0", "1")), n), symbols))
  clauses = parity_cycle(n)
  raw = to_raw(clauses, symbols)
  assignments = expand.grid(mget(symbols, envir = universe), KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE)
  distances = complement_distances(clauses, n)
  stopifnot(nrow(assignments) == 2^n, !any(evaluate_formula(raw, assignments)),
    identical(distances, rep(as.double(n), 2L * n)))
  deletion_models = vapply(seq_along(raw), function(i) {
    sum(evaluate_formula(raw[-i], assignments))
  }, integer(1))
  stopifnot(all(deletion_models == 1L))
  direct = simplify_cnf(raw, universe)
  public = CnfFormula(lapply(raw, make_clause, universe = universe))
  if (n < 4L) {
    stopifnot(isFALSE(c(direct)), isFALSE(c(public)))
  } else {
    stopifnot(identical(canonical_formula(direct), canonical_formula(raw)),
      identical(canonical_formula(public), canonical_formula(raw)))
  }
  cat("Parity cycle:", n, "variables,", length(raw), "clauses, complement distances",
    paste(distances, collapse = ","), "; production FALSE:", isFALSE(c(direct)), "\n")
  cat("  Models after each single-clause deletion:", paste(deletion_models, collapse = ","), "\n")
}

# The unchanged four-variable input remains unchanged under all 16 variable
# sign choices and 24 variable renamings. Each also gets a deterministic
# shuffled clause order and within-clause orientation.
set.seed(20260906)
symbols = paste0("x", 1:4)
universe = make_universe(setNames(rep(list(c("0", "1")), 4L), symbols))
base = parity_cycle(4L)
rename_checks = 0L
for (renaming in all_permutations(1:4)) for (signs in asplit(
    as.matrix(expand.grid(rep(list(c(-1L, 1L)), 4L))), 1L)) {
  clauses = lapply(base, function(clause) {
    sign(clause) * signs[abs(clause)] * renaming[abs(clause)]
  })
  raw = to_raw(clauses, symbols)
  raw = raw[sample.int(length(raw))]
  raw = lapply(raw, function(clause) clause[sample.int(2L)])
  direct = simplify_cnf(raw, universe)
  public = CnfFormula(lapply(raw, make_clause, universe = universe))
  stopifnot(identical(canonical_formula(direct), canonical_formula(raw)),
    identical(canonical_formula(public), canonical_formula(raw)))
  rename_checks = rename_checks + 1L
}
stopifnot(rename_checks == 384L)
cat("Variable-sign, variable-name, and shuffled input checks:", rename_checks, "\n")

# Separately exhaust every within-clause symbol ordering, so dictionary-order
# coverage does not depend on the random choice in the preceding loop.
orientation_checks = 0L
for (mask in 0:255) {
  raw = to_raw(base, symbols)
  for (i in seq_along(raw)) if (bitwAnd(mask, bitwShiftL(1L, i - 1L))) raw[[i]] = rev(raw[[i]])
  result = simplify_cnf(raw, universe)
  stopifnot(identical(canonical_formula(result), canonical_formula(raw)))
  orientation_checks = orientation_checks + 1L
}
stopifnot(orientation_checks == 256L)
cat("All within-clause symbol orderings:", orientation_checks, "\n")
cat("Unmodified production source MD5:", unname(tools::md5sum("R/CnfFormula_simplify.R")), "\n")
cat("R:", R.version.string, "\n")
