#!/usr/bin/env Rscript
# Super fuzzer: scale up testing with varied configurations
# Larger domains, more variables, more clauses
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

set.seed(271828)

# Config 1: 4 vars with domains 4-5 (256-625 assignments), heavy clause count
cat("=== Config 1: 4 vars, large domains, many clauses ===\n")
for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:4) {
    vname = paste0("V", v)
    ds = sample(3:5, 1)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:ds))
  }

  n_clauses = sample(6:15, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_atoms = sample(1:4, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [C1 trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("C1-%d", trial))
}
cat(sprintf("  Config 1: %d tests, %d failures\n", n_tests, n_failures))

# Config 2: 2 vars with domains 5-7 (25-49 assignments), many clauses with large ranges
cat("\n=== Config 2: 2 vars, very large domains ===\n")
for (trial in 1:200) {
  u = CnfUniverse()
  ds1 = sample(4:7, 1); ds2 = sample(4:7, 1)
  X = CnfSymbol(u, "X", paste0("x", 1:ds1))
  Y = CnfSymbol(u, "Y", paste0("y", 1:ds2))
  syms = list(X = X, Y = Y)

  n_clauses = sample(4:10, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_atoms = sample(1:2, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      n_vals = sample(1:(length(dom)-1), 1)
      syms[[s]] %among% sample(dom, n_vals)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [C2 trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("C2-%d", trial))
}
cat(sprintf("  Config 2: %d tests, %d failures\n", n_tests, n_failures))

# Config 3: 6 vars binary, 15-20 clauses (SAT-like)
cat("\n=== Config 3: 6 binary vars, many clauses (SAT-like) ===\n")
for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:6) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1"))
  }

  n_clauses = sample(10:20, 1)
  clauses = lapply(1:n_clauses, function(j) {
    # 3-SAT like: 3 literals per clause
    n_atoms = sample(2:4, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [C3 trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("C3-%d", trial))
}
cat(sprintf("  Config 3: %d tests, %d failures\n", n_tests, n_failures))

# Config 4: Combine operations: build formulas with &, |, and !
cat("\n=== Config 4: Combined operations ===\n")
for (trial in 1:200) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
  B = CnfSymbol(u, "B", c("b1", "b2"))
  C = CnfSymbol(u, "C", c("c1", "c2"))
  syms = list(A = A, B = B, C = C)

  mk_clause = function() {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  }

  # Build 2-3 sub-formulas
  f_parts = lapply(1:sample(2:3, 1), function(i) {
    cls = lapply(1:sample(1:3, 1), function(j) mk_clause())
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  })
  f_parts = f_parts[!vapply(f_parts, is.null, logical(1))]
  if (length(f_parts) < 2) next

  # Randomly combine with & or |
  op = sample(c("and", "or", "negate_and", "negate_or"), 1)
  f = tryCatch({
    switch(op,
      "and" = f_parts[[1]] & f_parts[[2]],
      "or" = f_parts[[1]] | f_parts[[2]],
      "negate_and" = (!f_parts[[1]]) & f_parts[[2]],
      "negate_or" = (!f_parts[[1]]) | f_parts[[2]]
    )
  }, error = function(e) NULL)
  if (is.null(f)) next

  # Verify semantics by truth table
  n_tests = n_tests + 1
  varnames = ls(u)
  domains = lapply(varnames, function(v) get(v, u))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  t_f = evaluate_formula(f, u)

  # Compute expected
  t_parts = lapply(f_parts, function(fp) evaluate_formula(fp, u))
  expected = switch(op,
    "and" = t_parts[[1]] & t_parts[[2]],
    "or" = t_parts[[1]] | t_parts[[2]],
    "negate_and" = (!t_parts[[1]]) & t_parts[[2]],
    "negate_or" = (!t_parts[[1]]) | t_parts[[2]]
  )

  if (!all(t_f == expected)) {
    n_failures = n_failures + 1
    idx = which(t_f != expected)[1]
    cat(sprintf("FAIL [C4 trial %d, op=%s]: row %d\n", trial, op, idx))
  }
}
cat(sprintf("  Config 4: %d tests, %d failures\n", n_tests, n_failures))

# Config 5: Stress with 5 vars, mixed domain sizes, units + long clauses
cat("\n=== Config 5: 5 vars mixed, stress ===\n")
for (trial in 1:300) {
  u = CnfUniverse()
  dom_sizes = sample(2:4, 5, replace = TRUE)
  syms = list()
  for (v in 1:5) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:dom_sizes[v]))
  }

  clauses = list()
  # Add 1-2 units
  for (i in 1:sample(1:2, 1)) {
    s = sample(names(syms), 1)
    dom = u[[s]]
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1)))
  }
  # Add 5-10 random clauses
  for (i in 1:sample(5:10, 1)) {
    n_atoms = sample(1:4, 1)
    chosen = sample(names(syms), min(n_atoms, 5))
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [C5 trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("C5-%d", trial))
}
cat(sprintf("  Config 5: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== SUPER TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
