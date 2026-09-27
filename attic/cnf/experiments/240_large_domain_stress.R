#!/usr/bin/env Rscript
# Large domain stress testing:
# Symbols with 10-20 values in their domain.
# Tests that range operations work correctly with larger sets.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

evaluate_raw_clauses = function(clauses, universe) {
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in clauses) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }
  raw_truth
}

# === Pattern 1: Two variables with large domains ===
cat("=== Two vars, large domains ===\n")
set.seed(240001)

for (trial in 1:500) {
  u = CnfUniverse()
  d_size = sample(10:15, 1)
  dom = paste0("v", 1:d_size)
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  n_cl = sample(3:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:max(1, d_size-2), 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [2vlg-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [2vlg-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Two vars large: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Three variables, mixed large domains ===
cat("\n=== Three vars, mixed large domains ===\n")
set.seed(240002)

for (trial in 1:500) {
  u = CnfUniverse()
  d_sizes = c(sample(8:12, 1), sample(5:8, 1), sample(3:5, 1))
  sym_names = paste0("V", 1:3)
  doms = list()
  syms = list()
  for (i in 1:3) {
    d = paste0("v", 1:d_sizes[i])
    doms[[sym_names[i]]] = d
    syms[[sym_names[i]]] = CnfSymbol(u, sym_names[i], d)
  }

  n_cl = sample(4:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) {
      d = doms[[s]]
      syms[[s]] %among% sample(d, sample(1:max(1, length(d)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [3vmixed-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [3vmixed-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Three vars mixed: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Large domains with narrow ranges (lots of subsetting) ===
cat("\n=== Large domains, narrow ranges ===\n")
set.seed(240003)

for (trial in 1:500) {
  u = CnfUniverse()
  d_size = sample(12:20, 1)
  dom = paste0("v", 1:d_size)
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Use narrow ranges (1-3 values out of large domain)
  n_cl = sample(4:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [lgnarrow-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [lgnarrow-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Large narrow: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Large domains with operations ===
cat("\n=== Large domains with operations ===\n")
set.seed(240004)

for (trial in 1:200) {
  u = CnfUniverse()
  d_size = sample(8:12, 1)
  dom = paste0("v", 1:d_size)
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)

  make_f = function() {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      atoms = list()
      if (runif(1) > 0.3) atoms[[length(atoms) + 1]] = A %among% sample(dom, sample(1:max(1, d_size-2), 1))
      if (runif(1) > 0.3) atoms[[length(atoms) + 1]] = B %among% sample(dom, sample(1:max(1, d_size-2), 1))
      if (length(atoms) == 0) atoms[[1]] = A %among% sample(dom, sample(1:max(1, d_size-2), 1))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1

  # Test all operations
  for (op_name in c("AND", "OR", "NOT")) {
    result = tryCatch({
      if (op_name == "AND") f1 & f2
      else if (op_name == "OR") f1 | f2
      else !f1
    }, error = function(e) e)

    if (inherits(result, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [lgop-%s-%d]: %s\n", op_name, trial, result$message)); next
    }

    t1 = evaluate_formula(f1, u)
    t2 = evaluate_formula(f2, u)
    expected = if (op_name == "AND") t1 & t2 else if (op_name == "OR") t1 | t2 else !t1
    actual = evaluate_formula(result, u)

    if (!all(actual == expected)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [lgop-%s-%d]: mismatch\n", op_name, trial))
    }
  }
}
cat(sprintf("  Large domain ops: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
