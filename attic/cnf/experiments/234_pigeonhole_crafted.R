#!/usr/bin/env Rscript
# Crafted formulas testing specific algorithmic scenarios:
# 1. Pigeonhole-like formulas (unsatisfiable by combinatorial argument)
# 2. Formulas that require all phases of simplification
# 3. Formulas where every clause interacts with every other clause
# 4. Cyclic dependency structures
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

# === Pattern 1: At-most-one constraint (AMO) ===
cat("=== At-most-one constraint ===\n")
set.seed(234001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("T", "F")
  n_vars = sample(4:7, 1)
  sym_names = paste0("V", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # At most one variable is TRUE: for each pair, NOT(Vi=T AND Vj=T) = (Vi=F OR Vj=F)
  clauses = list()
  for (i in 1:(n_vars-1)) {
    for (j in (i+1):n_vars) {
      cl = as.CnfClause(syms[[sym_names[i]]] %among% "F" | syms[[sym_names[j]]] %among% "F")
      if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
    }
  }
  # Add "at least one TRUE" constraint sometimes
  if (runif(1) < 0.5) {
    atoms = lapply(sym_names, function(s) syms[[s]] %among% "T")
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  # Add some random clauses
  for (j in 1:sample(1:3, 1)) {
    n_sym = sample(2:min(4, n_vars), 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, 1))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [amo-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [amo-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  At-most-one: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Graph coloring formulas ===
cat("\n=== Graph coloring ===\n")
set.seed(234002)

for (trial in 1:500) {
  u = CnfUniverse()
  n_colors = sample(2:4, 1)
  dom = paste0("c", 1:n_colors)
  n_vars = sample(4:6, 1)
  sym_names = paste0("V", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Random edges: adjacent vertices must have different colors
  n_edges = sample(n_vars:(n_vars*(n_vars-1)/2), 1)
  all_edges = combn(sym_names, 2, simplify = FALSE)
  edges = all_edges[sample(length(all_edges), min(n_edges, length(all_edges)))]

  clauses = list()
  for (edge in edges) {
    v1 = edge[1]; v2 = edge[2]
    # For each color: NOT(v1=c AND v2=c) = (v1!=c OR v2!=c)
    for (color in dom) {
      other_colors = setdiff(dom, color)
      cl = as.CnfClause(syms[[v1]] %among% other_colors | syms[[v2]] %among% other_colors)
      if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
    }
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [color-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [color-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Graph coloring: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Implication chains with random extras ===
cat("\n=== Implication chains ===\n")
set.seed(234003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  n_vars = sample(4:6, 1)
  sym_names = paste0("V", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Chain: V1 in S1 -> V2 in S2 -> V3 in S3 -> ...
  # In CNF: (V1 not in S1 | V2 in S2), (V2 not in S2 | V3 in S3), ...
  clauses = list()
  chain_vals = lapply(1:n_vars, function(i) sample(dom, sample(1:2, 1)))

  for (i in 1:(n_vars-1)) {
    anti = setdiff(dom, chain_vals[[i]])
    if (length(anti) == 0) next  # skip if chain_vals covers entire domain
    cl = as.CnfClause(syms[[sym_names[i]]] %among% anti | syms[[sym_names[i+1]]] %among% chain_vals[[i+1]])
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }

  # Starting unit
  clauses[[length(clauses) + 1]] = as.CnfClause(syms[[sym_names[1]]] %among% chain_vals[[1]])

  # Random extras
  for (j in 1:sample(2:5, 1)) {
    n_sym = sample(2:min(3, n_vars), 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [chain-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chain-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Implication chains: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Bi-implications (equivalence constraints) ===
cat("\n=== Bi-implication constraints ===\n")
set.seed(234004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  n_vars = sample(3:5, 1)
  sym_names = paste0("V", 1:n_vars)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  clauses = list()
  # Some bi-implications: Vi=Si <-> Vj=Sj
  # In CNF: (Vi not in Si | Vj in Sj) AND (Vj not in Sj | Vi in Si)
  for (k in 1:sample(2:4, 1)) {
    pair = sample(sym_names, 2)
    s1 = sample(dom, sample(1:2, 1))
    s2 = sample(dom, sample(1:2, 1))
    anti_s1 = setdiff(dom, s1)
    anti_s2 = setdiff(dom, s2)
    if (length(anti_s1) > 0) {
      cl = as.CnfClause(syms[[pair[1]]] %among% anti_s1 | syms[[pair[2]]] %among% s2)
      if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
    }
    if (length(anti_s2) > 0) {
      cl = as.CnfClause(syms[[pair[2]]] %among% anti_s2 | syms[[pair[1]]] %among% s1)
      if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
    }
  }

  # Random extras
  for (j in 1:sample(2:4, 1)) {
    n_sym = sample(1:min(3, n_vars), 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 4) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [biimpl-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [biimpl-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Bi-implications: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
