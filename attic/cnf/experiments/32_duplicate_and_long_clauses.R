#!/usr/bin/env Rscript
# Test edge cases with duplicate clauses, very long clauses, and unusual patterns
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

check_fc = function(formula, raw_clauses, universe, label) {
  n_tests <<- n_tests + 1
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in raw_clauses) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }

  simplified_truth = evaluate_formula(formula, universe)
  mismatches = which(raw_truth != simplified_truth)
  if (length(mismatches)) {
    n_failures <<- n_failures + 1
    idx = mismatches[1]
    cat(sprintf("FAIL [%s]: row %d (raw=%s, simp=%s)\n",
      label, idx, raw_truth[idx], simplified_truth[idx]))
    cat(sprintf("  Assignment: %s\n",
      paste(colnames(assignments), "=", assignments[idx, ], collapse = ", ")))
    return(FALSE)
  }
  TRUE
}

set.seed(13579)

cat("=== Duplicate clauses ===\n")
# Same clause appearing 2-5 times
for (trial in 1:200) {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e"))
  Z = CnfSymbol(u, "Z", c("f", "g", "h"))
  syms = list(X = X, Y = Y, Z = Z)

  # Create some unique clauses
  n_unique = sample(2:4, 1)
  unique_clauses = lapply(1:n_unique, function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  # Add duplicates
  clause_list = unique_clauses
  for (i in 1:sample(1:4, 1)) {
    clause_list[[length(clause_list) + 1]] = unique_clauses[[sample(n_unique, 1)]]
  }

  f = tryCatch(CnfFormula(clause_list), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [dup trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clause_list, u, sprintf("dup-%d", trial))
}
cat(sprintf("  Duplicate clauses: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Long clauses (all symbols) ===\n")
# Clauses that mention all variables
for (trial in 1:200) {
  u = CnfUniverse()
  n_vars = sample(4:6, 1)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:sample(2:3, 1)))
  }

  n_clauses = sample(3:7, 1)
  clauses = lapply(1:n_clauses, function(j) {
    # Each clause mentions ALL variables
    atoms = lapply(names(syms), function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [long trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("long-%d", trial))
}
cat(sprintf("  Long clauses: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Mix of unit clauses and very long clauses ===\n")
for (trial in 1:200) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
  B = CnfSymbol(u, "B", c("b1", "b2", "b3"))
  C = CnfSymbol(u, "C", c("c1", "c2"))
  D = CnfSymbol(u, "D", c("d1", "d2"))
  E = CnfSymbol(u, "E", c("e1", "e2", "e3"))
  syms = list(A = A, B = B, C = C, D = D, E = E)

  clauses = list()
  # 2-3 units
  for (i in 1:sample(2:3, 1)) {
    s = sample(names(syms), 1)
    dom = u[[s]]
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1)))
  }
  # 2-3 clauses with 4-5 symbols
  for (i in 1:sample(2:3, 1)) {
    n = sample(4:5, 1)
    chosen = sample(names(syms), n)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [mix trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("mix-%d", trial))
}
cat(sprintf("  Mix unit+long: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Cross-type operator chains ===\n")
# Build formulas using chains of different operator types
for (trial in 1:200) {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e"))
  Z = CnfSymbol(u, "Z", c("f", "g"))

  # Build using Atom & Atom, then | Clause, etc.
  atom1 = X %among% sample(c("a", "b", "c"), sample(1:2, 1))
  atom2 = Y %among% sample(c("d", "e"), 1)
  atom3 = Z %among% sample(c("f", "g"), 1)

  # Various chain patterns
  pattern = sample(1:6, 1)
  f = tryCatch({
    switch(pattern,
      # Pattern 1: (atom & atom) & atom
      (atom1 & atom2) & atom3,
      # Pattern 2: atom & (atom | atom)
      atom1 & (atom2 | atom3),
      # Pattern 3: (atom | atom) & (atom | atom)
      (atom1 | atom2) & (atom2 | atom3),
      # Pattern 4: !atom & atom
      (!atom1) & atom2,
      # Pattern 5: !(atom | atom) & atom
      (!(atom1 | atom2)) & atom3,
      # Pattern 6: (atom & atom) | atom
      (atom1 & atom2) | atom3
    )
  }, error = function(e) e)

  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [chain trial %d pattern %d]: %s\n", trial, pattern, f$message)); next
  }

  # Evaluate the formula
  n_tests <<- n_tests + 1
  simplified_truth = evaluate_formula(f, u)

  # Compute expected manually
  varnames = ls(u)
  domains = lapply(varnames, function(v) get(v, u))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  a1_vals = unclass(atom1)$values
  a2_vals = unclass(atom2)$values
  a3_vals = unclass(atom3)$values

  expected = switch(pattern,
    (assignments$X %in% a1_vals) & (assignments$Y %in% a2_vals) & (assignments$Z %in% a3_vals),
    (assignments$X %in% a1_vals) & ((assignments$Y %in% a2_vals) | (assignments$Z %in% a3_vals)),
    ((assignments$X %in% a1_vals) | (assignments$Y %in% a2_vals)) & ((assignments$Y %in% a2_vals) | (assignments$Z %in% a3_vals)),
    (!(assignments$X %in% a1_vals)) & (assignments$Y %in% a2_vals),
    (!((assignments$X %in% a1_vals) | (assignments$Y %in% a2_vals))) & (assignments$Z %in% a3_vals),
    ((assignments$X %in% a1_vals) & (assignments$Y %in% a2_vals)) | (assignments$Z %in% a3_vals)
  )

  mismatches = which(simplified_truth != expected)
  if (length(mismatches)) {
    n_failures <<- n_failures + 1
    idx = mismatches[1]
    cat(sprintf("FAIL [chain trial %d, pattern %d]: row %d\n", trial, pattern, idx))
  }
}
cat(sprintf("  Cross-type chains: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== CnfFormula constructor with mixed CnfClause and CnfFormula ===\n")
for (trial in 1:200) {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b"))
  Y = CnfSymbol(u, "Y", c("c", "d"))
  Z = CnfSymbol(u, "Z", c("e", "f"))
  syms = list(X = X, Y = Y, Z = Z)

  # Build a formula from a mix of CnfClause and CnfFormula objects
  cl1 = as.CnfClause(X %among% sample(c("a", "b"), 1) | Y %among% sample(c("c", "d"), 1))
  cl2 = as.CnfClause(Z %among% sample(c("e", "f"), 1))
  f_inner = tryCatch(cl1 & cl2, error = function(e) NULL)
  if (is.null(f_inner)) next

  cl3 = as.CnfClause(Y %among% sample(c("c", "d"), 1) | Z %among% sample(c("e", "f"), 1))

  # CnfFormula constructor with mixed list
  f_mixed = tryCatch(CnfFormula(list(f_inner, cl3)), error = function(e) e)
  if (inherits(f_mixed, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [mixed ctor trial %d]: %s\n", trial, f_mixed$message)); next
  }

  # Compare with explicit AND
  f_explicit = tryCatch(f_inner & as.CnfFormula(cl3), error = function(e) NULL)
  if (is.null(f_explicit)) next

  n_tests = n_tests + 1
  t_mixed = evaluate_formula(f_mixed, u)
  t_explicit = evaluate_formula(f_explicit, u)
  if (!all(t_mixed == t_explicit)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mixed ctor trial %d]: mixed constructor differs from explicit AND\n", trial))
  }
}
cat(sprintf("  Mixed constructor: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
