#!/usr/bin/env Rscript
# Test formulas where clauses share no symbols, or have minimal sharing.
# When clauses share no symbols, simplification should just return them as-is
# (no subsumption/SSE/HLA possible). Also test mixing disjoint + shared.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Fully disjoint symbols per clause ===
cat("=== Fully disjoint ===\n")
set.seed(257001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  # 9 variables, grouped into 3 disjoint sets of 3
  all_syms = paste0("V", 1:9)
  syms = list()
  for (s in all_syms) syms[[s]] = CnfSymbol(u, s, dom)

  # Each clause uses symbols from a different group
  groups = list(all_syms[1:3], all_syms[4:6], all_syms[7:9])
  clauses = lapply(groups, function(group) {
    n_sym = sample(1:3, 1)
    chosen = sample(group, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [disjoint-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [disjoint-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Fully disjoint: %d tests, %d failures\n", n_tests, n_failures))

# === Disjoint clauses combined with AND/OR ===
cat("\n=== Disjoint operations ===\n")
set.seed(257002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  all_syms = paste0("V", 1:6)
  syms = list()
  for (s in all_syms) syms[[s]] = CnfSymbol(u, s, dom)

  # f1 uses V1-V3, f2 uses V4-V6
  make_f = function(group) {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(group, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(all_syms[1:3])
  f2 = make_f(all_syms[4:6])
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)

  op = sample(1:3, 1)
  if (op == 1) {
    r = tryCatch(f1 & f2, error = function(e) NULL)
    expected = t1 & t2
  } else if (op == 2) {
    r = tryCatch(f1 | f2, error = function(e) NULL)
    expected = t1 | t2
  } else {
    r = tryCatch(!f1, error = function(e) NULL)
    expected = !t1
  }
  if (!is.null(r) && !all(evaluate_formula(r, u) == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [disjop-%d]: op=%d mismatch\n", trial, op))
  }
}
cat(sprintf("  Disjoint operations: %d tests, %d failures\n", n_tests, n_failures))

# === One shared symbol, rest disjoint ===
cat("\n=== One shared symbol ===\n")
set.seed(257003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  # V1 is shared, V2-V4 in group 1, V5-V7 in group 2
  all_syms = paste0("V", 1:7)
  syms = list()
  for (s in all_syms) syms[[s]] = CnfSymbol(u, s, dom)

  n_cl = sample(4:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    # Each clause includes V1 and some from one group
    group = if (j %% 2 == 0) all_syms[2:4] else all_syms[5:7]
    n_sym = sample(1:2, 1)
    chosen = c("V1", sample(group, n_sym))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [oneshared-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [oneshared-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  One shared symbol: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
