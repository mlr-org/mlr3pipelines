source("experiments/test_harness.R")

# Stress test HLA (Hidden Literal Addition) and cascading simplification
# These tests target:
# 1. HLA that requires many rounds to detect hidden tautology/subsumption
# 2. Cascading: unit propagation creates new units which create more SSE
# 3. Large formulas that push limits of the bookkeeping

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
    cat(sprintf("  Clauses (%d):\n", length(raw_clauses)))
    for (ci in seq_along(raw_clauses)) {
      cl = unclass(raw_clauses[[ci]])
      if (is.logical(cl)) {
        cat(sprintf("    %d: %s\n", ci, cl))
      } else {
        cat(sprintf("    %d: %s\n", ci, paste(names(cl), "=", sapply(cl, paste, collapse = ","), collapse = " | ")))
      }
    }
    return(FALSE)
  }
  TRUE
}

cat("=== HLA Stress: Chain of nearly-subsuming clauses ===\n")
# Create a chain where clause i almost subsumes clause i+1,
# so HLA needs to propagate through the chain to detect elimination

for (trial in 1:100) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2", "a3", "a4"))
  B = CnfSymbol(u, "B", c("b1", "b2", "b3", "b4"))
  C = CnfSymbol(u, "C", c("c1", "c2", "c3", "c4"))

  syms = list(A = A, B = B, C = C)

  # Build a chain of clauses
  n_clauses = sample(4:8, 1)
  clauses = vector("list", n_clauses)

  for (i in 1:n_clauses) {
    chosen = sample(names(syms), sample(2:3, 1))
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:3, 1))
    })
    clauses[[i]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [chain trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("chain-%d", trial))
}
cat(sprintf("  Chain done: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== HLA Stress: Hidden tautology patterns ===\n")
# Patterns designed so that after HLA, a clause becomes a tautology
# This requires that the HLA-expanded range covers the full domain

for (trial in 1:100) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
  B = CnfSymbol(u, "B", c("b1", "b2", "b3"))
  C = CnfSymbol(u, "C", c("c1", "c2", "c3"))

  # Target clause: A in {a1} | B in {b1} | C in {c1}
  cl_target = as.CnfClause(A %among% sample(c("a1", "a2", "a3"), sample(1:2, 1)) |
    B %among% sample(c("b1", "b2", "b3"), sample(1:2, 1)) |
    C %among% sample(c("c1", "c2", "c3"), sample(1:2, 1)))

  # Helper clauses that together could make target a hidden tautology
  helpers = lapply(1:sample(3:6, 1), function(i) {
    syms = list(A = A, B = B, C = C)
    chosen = sample(c("A", "B", "C"), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(u[[s]], sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  all_clauses = c(list(cl_target), helpers)
  f = tryCatch(CnfFormula(all_clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [hidden_taut trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, all_clauses, u, sprintf("hidden_taut-%d", trial))
}
cat(sprintf("  Hidden taut done: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Cascading: Unit propagation creating new units ===\n")
# Create formulas where unit propagation of one clause triggers another

for (trial in 1:100) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2"))
  B = CnfSymbol(u, "B", c("b1", "b2"))
  C = CnfSymbol(u, "C", c("c1", "c2"))
  D = CnfSymbol(u, "D", c("d1", "d2"))
  E = CnfSymbol(u, "E", c("e1", "e2"))

  # Chain of implications via units:
  # A in {a1} (unit) -> B's range restricted -> C's range restricted -> ...
  clauses = list(
    as.CnfClause(A %among% sample(c("a1", "a2"), 1)),
    as.CnfClause(A %among% sample(c("a1", "a2"), 1) | B %among% sample(c("b1", "b2"), 1)),
    as.CnfClause(B %among% sample(c("b1", "b2"), 1) | C %among% sample(c("c1", "c2"), 1)),
    as.CnfClause(C %among% sample(c("c1", "c2"), 1) | D %among% sample(c("d1", "d2"), 1)),
    as.CnfClause(D %among% sample(c("d1", "d2"), 1) | E %among% sample(c("e1", "e2"), 1))
  )

  # Add some extra clauses
  extra = lapply(1:sample(0:3, 1), function(i) {
    syms = list(A = A, B = B, C = C, D = D, E = E)
    sym_names = c("A", "B", "C", "D", "E")
    chosen = sample(sym_names, sample(1:3, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(u[[s]], sample(1:length(u[[s]]), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  all_clauses = c(clauses, extra)
  f = tryCatch(CnfFormula(all_clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [cascade trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, all_clauses, u, sprintf("cascade-%d", trial))
}
cat(sprintf("  Cascade done: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Large formulas: many clauses, many variables ===\n")
# Push the simplifier with larger inputs

for (trial in 1:50) {
  u = CnfUniverse()
  n_vars = sample(4:6, 1)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    dom_size = sample(2:4, 1)
    dom = paste0(tolower(vname), "_", 1:dom_size)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_clauses = sample(8:20, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_syms = sample(1:min(4, n_vars), 1)
    chosen = sample(names(syms), n_syms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [large trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("large-%d", trial))
}
cat(sprintf("  Large done: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Duplicate clauses ===\n")
# Test with many duplicate/near-duplicate clauses

for (trial in 1:100) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
  B = CnfSymbol(u, "B", c("b1", "b2", "b3"))

  base_clause = as.CnfClause(
    A %among% sample(c("a1", "a2", "a3"), sample(1:2, 1)) |
    B %among% sample(c("b1", "b2", "b3"), sample(1:2, 1))
  )

  clauses = rep(list(base_clause), sample(2:5, 1))

  # Add some variations
  for (i in 1:sample(1:3, 1)) {
    clauses[[length(clauses) + 1]] = as.CnfClause(
      A %among% sample(c("a1", "a2", "a3"), sample(1:2, 1)) |
      B %among% sample(c("b1", "b2", "b3"), sample(1:2, 1))
    )
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [dup trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("dup-%d", trial))
}
cat(sprintf("  Dup done: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
