#!/usr/bin/env Rscript
# Test edge cases with domain sizes: singleton domains, large domains, mixed
# Also tests with special characters in domain values
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

# === Singleton domain variables ===
cat("=== Singleton domain variables ===\n")
set.seed(52001)

# Variables with domain size 1 are always TRUE when unconstrained.
# A unit clause for them either forces them to their only value (always satisfied) or to nothing (contradiction).
for (trial in 1:100) {
  u = CnfUniverse()
  # Mix of singleton and multi-value domains
  A = CnfSymbol(u, "A", c("only"))
  B = CnfSymbol(u, "B", c("x", "y"))
  C = CnfSymbol(u, "C", c("p", "q", "r"))

  clauses = list()
  # Sometimes include singleton in clause
  n_cl = sample(2:5, 1)
  for (i in 1:n_cl) {
    syms_avail = list(A = A, B = B, C = C)
    doms_avail = list(A = "only", B = c("x", "y"), C = c("p", "q", "r"))
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms_avail), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms_avail[[s]] %among% sample(doms_avail[[s]], sample(1:length(doms_avail[[s]]), 1))
    })
    clauses[[i]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [singleton %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("singleton-%d", trial))
}
cat(sprintf("  Singleton domain: %d tests, %d failures\n", n_tests, n_failures))

# === Multiple singleton domains ===
cat("\n=== Multiple singleton domains ===\n")
set.seed(52002)

for (trial in 1:100) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", "only_a")
  B = CnfSymbol(u, "B", "only_b")
  C = CnfSymbol(u, "C", c("c1", "c2"))

  clauses = list()
  n_cl = sample(2:4, 1)
  syms_avail = list(A = A, B = B, C = C)
  doms_avail = list(A = "only_a", B = "only_b", C = c("c1", "c2"))
  for (i in 1:n_cl) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms_avail), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms_avail[[s]] %among% sample(doms_avail[[s]], sample(1:length(doms_avail[[s]]), 1))
    })
    clauses[[i]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [multi-single %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("multi-single-%d", trial))
}
cat(sprintf("  Multi singleton: %d tests, %d failures\n", n_tests, n_failures))

# === Domain size 2 (binary) everywhere ===
cat("\n=== All binary domains ===\n")
set.seed(52003)

for (trial in 1:200) {
  u = CnfUniverse()
  n_vars = sample(3:6, 1)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("T", "F"))
  }

  n_cl = sample(3:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(1:min(3, n_vars), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("T", "F"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [binary %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("binary-%d", trial))
}
cat(sprintf("  All binary: %d tests, %d failures\n", n_tests, n_failures))

# === Asymmetric domain sizes ===
cat("\n=== Asymmetric domain sizes ===\n")
set.seed(52004)

for (trial in 1:200) {
  u = CnfUniverse()
  # One variable with very large domain, others small
  A = CnfSymbol(u, "A", paste0("a", 1:8))
  B = CnfSymbol(u, "B", c("0", "1"))
  C = CnfSymbol(u, "C", c("x", "y"))

  syms = list(A = A, B = B, C = C)
  doms = list(A = paste0("a", 1:8), B = c("0", "1"), C = c("x", "y"))

  n_cl = sample(3:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      d = doms[[s]]
      syms[[s]] %among% sample(d, sample(1:max(1, length(d)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [asym %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("asym-%d", trial))
}
cat(sprintf("  Asymmetric domains: %d tests, %d failures\n", n_tests, n_failures))

# === Special characters in domain values ===
cat("\n=== Special characters in values ===\n")
set.seed(52005)

for (trial in 1:100) {
  u = CnfUniverse()
  # Domains with special characters
  A = CnfSymbol(u, "A", c("hello world", "foo bar", "baz qux"))
  B = CnfSymbol(u, "B", c("a.b", "c-d", "e_f"))
  C = CnfSymbol(u, "C", c("TRUE", "FALSE", "NA"))

  syms = list(A = A, B = B, C = C)
  doms = list(
    A = c("hello world", "foo bar", "baz qux"),
    B = c("a.b", "c-d", "e_f"),
    C = c("TRUE", "FALSE", "NA")
  )

  n_cl = sample(2:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      d = doms[[s]]
      syms[[s]] %among% sample(d, sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [special %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("special-%d", trial))
}
cat(sprintf("  Special characters: %d tests, %d failures\n", n_tests, n_failures))

# === Empty string in domain ===
cat("\n=== Empty string in domain ===\n")
set.seed(52006)

for (trial in 1:100) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("", "notempty", "alsonotempty"))
  B = CnfSymbol(u, "B", c("x", "y"))

  syms = list(A = A, B = B)
  doms = list(A = c("", "notempty", "alsonotempty"), B = c("x", "y"))

  n_cl = sample(2:4, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(1:2, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      d = doms[[s]]
      syms[[s]] %among% sample(d, sample(1:max(1, length(d)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests = n_tests + 1; n_failures = n_failures + 1
    cat(sprintf("ERROR [empty-str %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u, sprintf("empty-str-%d", trial))
}
cat(sprintf("  Empty string: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
