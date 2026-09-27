#!/usr/bin/env Rscript
# "Final boss" comprehensive test combining everything we've learned
# Uses the most challenging patterns across all strategies
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
    return(FALSE)
  }
  TRUE
}

check_formula_truth = function(formula, expected_truth, universe, label) {
  n_tests <<- n_tests + 1
  result_truth = evaluate_formula(formula, universe)
  mismatches = which(result_truth != expected_truth)
  if (length(mismatches)) {
    n_failures <<- n_failures + 1
    idx = mismatches[1]
    cat(sprintf("FAIL [%s]: row %d (expected=%s, got=%s)\n",
      label, idx, expected_truth[idx], result_truth[idx]))
    return(FALSE)
  }
  TRUE
}

# === Extreme SAT: 9 binary vars, many clauses ===
cat("=== Extreme SAT ===\n")
set.seed(59001)

for (trial in 1:100) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:9) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1"))
  }

  n_cl = sample(15:30, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(2:4, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [extreme-sat %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("extreme-sat-%d", trial))
}
cat(sprintf("  Extreme SAT: %d tests, %d failures\n", n_tests, n_failures))

# === Large domain mixed: 3 vars, domains 2-6 ===
cat("\n=== Large domain mixed ===\n")
set.seed(59002)

for (trial in 1:200) {
  u = CnfUniverse()
  dom_sizes = c(sample(2:3, 1), sample(3:4, 1), sample(5:6, 1))
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, paste0("d", 1:dom_sizes[v]))
  }

  n_cl = sample(5:12, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [large-dom %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("large-dom-%d", trial))
}
cat(sprintf("  Large domain mixed: %d tests, %d failures\n", n_tests, n_failures))

# === Combined operations gauntlet ===
cat("\n=== Combined operations gauntlet ===\n")
set.seed(59003)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c"))
  }

  mk_formula = function() {
    cls = lapply(1:sample(1:3, 1), function(j) {
      chosen = sample(names(syms), sample(1:2, 1))
      atoms = lapply(chosen, function(s) {
        syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = mk_formula(); f2 = mk_formula(); f3 = mk_formula()
  if (is.null(f1) || is.null(f2) || is.null(f3)) next

  t1 = evaluate_formula(f1, u); t2 = evaluate_formula(f2, u); t3 = evaluate_formula(f3, u)

  # Test various combinations
  op = sample(1:6, 1)
  result = tryCatch(switch(op,
    `1` = { expected = (t1 & t2) | t3; (f1 & f2) | f3 },
    `2` = { expected = t1 | (t2 & t3); f1 | (f2 & f3) },
    `3` = { expected = !(t1 & t2) & t3; !(f1 & f2) & f3 },
    `4` = { expected = (t1 | !t2) & (t2 | !t3); (f1 | !f2) & (f2 | !f3) },
    `5` = { expected = !(!t1 | !t2); !(!f1 | !f2) },  # should equal t1 & t2
    `6` = { expected = (t1 & !t1) | t2; (f1 & !f1) | f2 }  # should equal t2
  ), error = function(e) NULL)
  if (is.null(result)) next

  check_formula_truth(result, expected, u, sprintf("gauntlet-%d-op%d", trial, op))
}
cat(sprintf("  Combined gauntlet: %d tests, %d failures\n", n_tests, n_failures))

# === Units + operations chain ===
cat("\n=== Units + operations ===\n")
set.seed(59004)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:4) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1", "2"))
  }

  # Build a formula with units
  clauses1 = list(
    as.CnfClause(syms[["V1"]] %among% sample(c("0", "1", "2"), sample(1:2, 1)))
  )
  for (i in 1:sample(2:4, 1)) {
    n_atoms = sample(2:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1", "2"), sample(1:2, 1))
    })
    clauses1[[length(clauses1) + 1]] = as.CnfClause(Reduce(`|`, atoms))
  }
  f1 = tryCatch(CnfFormula(clauses1), error = function(e) NULL)
  if (is.null(f1)) next

  # Build another formula
  clauses2 = lapply(1:sample(2:4, 1), function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1", "2"), sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  f2 = tryCatch(CnfFormula(clauses2), error = function(e) NULL)
  if (is.null(f2)) next

  t1 = evaluate_formula(f1, u); t2 = evaluate_formula(f2, u)

  # Chain: f1 & f2, then OR with !f1
  f12 = tryCatch(f1 & f2, error = function(e) NULL)
  if (is.null(f12)) next
  not_f1 = tryCatch(!f1, error = function(e) NULL)
  if (is.null(not_f1)) next
  result = tryCatch(f12 | not_f1, error = function(e) NULL)
  if (is.null(result)) next

  expected = (t1 & t2) | !t1
  check_formula_truth(result, expected, u, sprintf("unit-chain-%d", trial))
}
cat(sprintf("  Units + operations: %d tests, %d failures\n", n_tests, n_failures))

# === Stress test with 5 vars, domain 4, many clauses ===
cat("\n=== Heavy stress ===\n")
set.seed(59005)

for (trial in 1:100) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:5) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c", "d"))
  }

  n_cl = sample(10:20, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(1:4, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c", "d"), sample(1:3, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [heavy-stress %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("heavy-stress-%d", trial))
}
cat(sprintf("  Heavy stress: %d tests, %d failures\n", n_tests, n_failures))

# === Randomized everything ===
cat("\n=== Randomized everything ===\n")
set.seed(59006)

for (trial in 1:500) {
  u = CnfUniverse()
  n_vars = sample(2:6, 1)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    dom_size = sample(2:5, 1)
    syms[[vname]] = CnfSymbol(u, vname, paste0("v", v, "_", 1:dom_size))
  }

  n_cl = sample(3:15, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(1:min(n_vars, 4), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [rng %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("rng-%d", trial))
}
cat(sprintf("  Randomized everything: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
