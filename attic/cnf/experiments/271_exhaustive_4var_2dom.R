#!/usr/bin/env Rscript
# Exhaustive test over 4 binary variables, 3 clauses.
# Binary domain: 2^4 = 16 assignments per evaluation.
# This is a very thorough but computationally feasible exhaustive test.
# Each clause is a disjunction of atoms, each atom is V_i in {0} or V_i in {1}.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== Exhaustive 4-var binary 3-clause ===\n")
set.seed(271001)

u = CnfUniverse()
dom = c("0", "1")
sym_names = paste0("V", 1:4)
syms = list()
for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

# Generate all possible non-tautological atoms: (V_i, {0}), (V_i, {1})
# That's 8 atoms.
all_atoms = list()
for (s in sym_names) {
  for (v in dom) {
    all_atoms[[length(all_atoms) + 1]] = list(sym = s, range = v)
  }
}

# A clause is a set of 1-4 atoms (using OR)
# We sample random 3-clause formulas from this pool
n_total_trials = 50000  # 50K random samples

for (trial in 1:n_total_trials) {
  # Each clause: pick 1-3 random atoms
  clauses = lapply(1:3, function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(length(all_atoms), n_atoms)
    atoms = lapply(chosen, function(k) {
      a = all_atoms[[k]]
      syms[[a$sym]] %among% a$range
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [exh-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [exh-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  4-var binary 3-clause: %d tests, %d failures\n", n_tests, n_failures))

# === Same but with 4 clauses ===
cat("\n=== 4-var binary 4-clause ===\n")
set.seed(271002)

for (trial in 1:30000) {
  clauses = lapply(1:4, function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(length(all_atoms), n_atoms)
    atoms = lapply(chosen, function(k) {
      a = all_atoms[[k]]
      syms[[a$sym]] %among% a$range
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [exh4-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_formula(make_raw_formula(lapply(clauses, unclass), u), u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [exh4-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  4-var binary 4-clause: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
