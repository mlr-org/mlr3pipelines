#!/usr/bin/env Rscript
# Adversarial patterns designed to stress the simplifier's internal bookkeeping
# Focus on patterns that create cascading updates to is_not_subset_of matrix
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

# === "Almost subset" patterns ===
# Create many clauses that are almost subsets of each other, differing in exactly one symbol
# This maximizes the work in the is_not_subset_of matrix
cat("=== Almost-subset patterns ===\n")
set.seed(41001)

for (trial in 1:200) {
  u = CnfUniverse()
  n_vars = sample(3:4, 1)
  dom_sizes = sample(2:3, n_vars, replace = TRUE)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:dom_sizes[v]))
  }

  # Create a "base" clause
  base_vals = lapply(names(syms), function(s) {
    dom = u[[s]]
    sample(dom, sample(1:max(1, length(dom)-1), 1))
  })
  names(base_vals) = names(syms)

  clauses = list()
  # Create variations that differ from base in exactly one symbol
  for (v in names(syms)) {
    dom = u[[v]]
    # Variant: change just this symbol to different values
    for (k in 1:min(2, length(dom))) {
      variant = base_vals
      variant[[v]] = sample(dom, sample(1:max(1, length(dom)-1), 1))
      atoms = lapply(names(syms), function(s) {
        syms[[s]] %among% variant[[s]]
      })
      clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
    }
  }

  # Add the base clause itself
  atoms = lapply(names(syms), function(s) {
    syms[[s]] %among% base_vals[[s]]
  })
  clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [almost trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("almost-%d", trial))
}
cat(sprintf("  Almost-subset: %d tests, %d failures\n", n_tests, n_failures))

# === Clauses that become identical after simplification ===
# Create formulas where SSE or unit propagation makes two clauses identical
cat("\n=== Post-simplification duplicates ===\n")
set.seed(41002)

for (trial in 1:200) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
  B = CnfSymbol(u, "B", c("b1", "b2"))
  C = CnfSymbol(u, "C", c("c1", "c2"))

  # Unit: A in {a1, a2}
  # Clause 1: (A in {a1, a2} | B in {b1})  -- after unit prop, becomes A ⊆ unit → eliminated
  # Clause 2: (A in {a1, a3} | B in {b1})  -- after unit prop, A becomes {a1} → (A in {a1} | B in {b1})
  # Clause 3: (A in {a2, a3} | B in {b1})  -- after unit prop, A becomes {a2} → (A in {a2} | B in {b1})
  # Clause 4: (A in {a1} | C in {c1})
  # Clause 5: (A in {a2} | C in {c1})
  # After unit prop, clauses 2&3 are different. But SSE between 2,3 might trigger.

  dom_a = sample(c("a1", "a2", "a3"), sample(2:3, 1))
  clauses = list(
    as.CnfClause(A %among% dom_a),
    as.CnfClause(A %among% sample(c("a1", "a2", "a3"), 2) | B %among% sample(c("b1", "b2"), 1)),
    as.CnfClause(A %among% sample(c("a1", "a2", "a3"), 2) | C %among% sample(c("c1", "c2"), 1)),
    as.CnfClause(B %among% sample(c("b1", "b2"), 1) | C %among% sample(c("c1", "c2"), 1))
  )

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [postsimp trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("postsimp-%d", trial))
}
cat(sprintf("  Post-simplification duplicates: %d tests, %d failures\n", n_tests, n_failures))

# === Clauses where unit propagation eliminates different symbols from different clauses ===
# This tests the interplay between unit propagation and the is_not_subset_of matrix
cat("\n=== Multi-unit propagation interaction ===\n")
set.seed(41003)

for (trial in 1:300) {
  u = CnfUniverse()
  n_vars = sample(4:6, 1)
  dom_sizes = sample(2:3, n_vars, replace = TRUE)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0(vname, "_", 1:dom_sizes[v]))
  }

  clauses = list()
  # Add 2-3 units
  unit_vars = sample(names(syms), min(3, n_vars))
  for (s in unit_vars) {
    dom = u[[s]]
    clauses[[length(clauses) + 1]] = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1)))
  }

  # Add multi-symbol clauses that overlap with multiple units
  for (i in 1:sample(4:8, 1)) {
    n_atoms = sample(2:min(n_vars, 4), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    clauses[[length(clauses) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [multiunit trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("multiunit-%d", trial))
}
cat(sprintf("  Multi-unit propagation: %d tests, %d failures\n", n_tests, n_failures))

# === Formulas with many near-contradictions ===
# Where clauses combine to create very restricted solutions
cat("\n=== Near-contradiction patterns ===\n")
set.seed(41004)

for (trial in 1:200) {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("x1", "x2", "x3"))
  Y = CnfSymbol(u, "Y", c("y1", "y2", "y3"))
  Z = CnfSymbol(u, "Z", c("z1", "z2"))

  syms = list(X = X, Y = Y, Z = Z)

  # Create clauses that together leave very few satisfying assignments
  # Use complementary patterns
  clauses = list()
  n_clauses = sample(4:8, 1)
  for (i in 1:n_clauses) {
    n_atoms = sample(1:3, 1)
    chosen = sample(c("X", "Y", "Z"), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      # Bias toward smaller ranges to create more constraints
      n_vals = if (runif(1) > 0.5) 1 else sample(1:max(1, length(dom)-1), 1)
      syms[[s]] %among% sample(dom, n_vals)
    })
    clauses[[i]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [nearcontra trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("nearcontra-%d", trial))
}
cat(sprintf("  Near-contradiction: %d tests, %d failures\n", n_tests, n_failures))

# === Formulas constructed via CnfFormula(list of CnfFormulas) ===
# Test the flattening path in the constructor
cat("\n=== Formula-of-formulas construction ===\n")
set.seed(41005)

for (trial in 1:100) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
  B = CnfSymbol(u, "B", c("b1", "b2"))
  C = CnfSymbol(u, "C", c("c1", "c2"))

  syms = list(A = A, B = B, C = C)

  mk_clause = function() {
    n_atoms = sample(1:3, 1)
    chosen = sample(c("A", "B", "C"), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  }

  # Build sub-formulas
  sf1 = tryCatch(CnfFormula(list(mk_clause(), mk_clause())), error = function(e) NULL)
  sf2 = tryCatch(CnfFormula(list(mk_clause(), mk_clause())), error = function(e) NULL)
  sf3 = tryCatch(CnfFormula(list(mk_clause())), error = function(e) NULL)
  if (is.null(sf1) || is.null(sf2) || is.null(sf3)) next

  # Construct formula from sub-formulas
  f_combined = tryCatch(CnfFormula(list(sf1, sf2, sf3)), error = function(e) NULL)
  # Also construct from individual clauses
  all_clauses = c(as.list(sf1), as.list(sf2), as.list(sf3))
  f_flat = tryCatch(CnfFormula(all_clauses), error = function(e) NULL)

  if (!is.null(f_combined) && !is.null(f_flat)) {
    n_tests = n_tests + 1
    t_comb = evaluate_formula(f_combined, u)
    t_flat = evaluate_formula(f_flat, u)
    if (!all(t_comb == t_flat)) {
      n_failures = n_failures + 1
      idx = which(t_comb != t_flat)[1]
      cat(sprintf("FAIL [formula-of-formula trial %d]: row %d\n", trial, idx))
    }
  }
}
cat(sprintf("  Formula-of-formulas: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
