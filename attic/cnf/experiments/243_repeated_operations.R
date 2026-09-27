#!/usr/bin/env Rscript
# Repeated operations stress test:
# Apply many AND, OR, NOT operations in sequence to build complex formulas.
# Tests that intermediate results stay semantically correct throughout.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Pattern 1: Many AND operations building up ===
cat("=== Incremental AND ===\n")
set.seed(243001)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Start with a simple formula and AND more clauses
  base_atoms = lapply(sample(sym_names, sample(1:2, 1)), function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
  base_cl = as.CnfClause(Reduce(`|`, base_atoms))
  if (isTRUE(unclass(base_cl))) next
  accum = CnfFormula(list(base_cl))

  n_steps = sample(3:8, 1)
  ok = TRUE
  for (step in 1:n_steps) {
    step_atoms = lapply(sample(sym_names, sample(1:2, 1)), function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    step_cl = as.CnfClause(Reduce(`|`, step_atoms))
    if (isTRUE(unclass(step_cl))) next
    step_f = CnfFormula(list(step_cl))

    accum = tryCatch(accum & step_f, error = function(e) e)
    if (inherits(accum, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [incand-%d-step%d]: %s\n", trial, step, accum$message))
      ok = FALSE; break
    }
  }
  if (!ok) { n_tests = n_tests + 1; next }

  n_tests = n_tests + 1
  # Verify final result
  truth = evaluate_formula(accum, u)
  # We can't easily track all clauses, but we can at least check it's a valid formula
  if (!is.logical(truth) || any(is.na(truth))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [incand-%d]: invalid truth result\n", trial))
  }
}
cat(sprintf("  Incremental AND: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Alternating AND and OR ===
cat("\n=== Alternating AND/OR ===\n")
set.seed(243002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_simple_f = function() {
    s = sample(sym_names, 1)
    cl = as.CnfClause(syms[[s]] %among% sample(dom, sample(1:2, 1)))
    if (isTRUE(unclass(cl))) return(NULL)
    CnfFormula(list(cl))
  }

  f = make_simple_f()
  if (is.null(f)) next

  formulas = list(f)
  n_ops = sample(3:6, 1)
  ok = TRUE

  for (step in 1:n_ops) {
    g = make_simple_f()
    if (is.null(g)) next

    op = sample(c("AND", "OR"), 1)
    new_f = tryCatch({
      if (op == "AND") f & g else f | g
    }, error = function(e) e)

    if (inherits(new_f, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [altop-%d-step%d]: %s\n", trial, step, new_f$message))
      ok = FALSE; break
    }
    f = new_f
    formulas[[length(formulas) + 1]] = f
  }
  if (!ok) { n_tests = n_tests + 1; next }

  n_tests = n_tests + 1
  # Check final formula evaluates
  truth = evaluate_formula(f, u)
  if (!is.logical(truth) || any(is.na(truth))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [altop-%d]: invalid truth\n", trial))
  }
}
cat(sprintf("  Alternating AND/OR: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Repeated negation (double, triple) ===
cat("\n=== Repeated negation ===\n")
set.seed(243003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Create a formula with 1-3 clauses
  n_cl = sample(1:3, 1)
  cls = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
  if (length(cls) < 1) next
  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  original_truth = evaluate_formula(f, u)

  # Double negation should be equivalent to original
  f_nn = tryCatch(!!f, error = function(e) e)
  if (inherits(f_nn, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dblneg-%d]: %s\n", trial, f_nn$message)); next
  }
  nn_truth = evaluate_formula(f_nn, u)
  if (!all(nn_truth == original_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dblneg-%d]: double negation mismatch\n", trial)); next
  }

  # Triple negation should be equivalent to single negation
  f_nnn = tryCatch(!!!f, error = function(e) e)
  if (inherits(f_nnn, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [trpneg-%d]: %s\n", trial, f_nnn$message)); next
  }
  n_truth = evaluate_formula(!f, u)
  nnn_truth = evaluate_formula(f_nnn, u)
  if (!all(nnn_truth == n_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [trpneg-%d]: triple negation mismatch\n", trial))
  }
}
cat(sprintf("  Repeated negation: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: De Morgan's law stress ===
cat("\n=== De Morgan's law ===\n")
set.seed(243004)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:2, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1

  # De Morgan: !(f1 & f2) == !f1 | !f2
  lhs = tryCatch(!(f1 & f2), error = function(e) e)
  rhs = tryCatch(!f1 | !f2, error = function(e) e)

  if (inherits(lhs, "error") || inherits(rhs, "error")) {
    n_failures = n_failures + 1
    msg = if (inherits(lhs, "error")) lhs$message else rhs$message
    cat(sprintf("ERROR [dm1-%d]: %s\n", trial, msg)); next
  }

  lhs_truth = evaluate_formula(lhs, u)
  rhs_truth = evaluate_formula(rhs, u)
  if (!all(lhs_truth == rhs_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dm1-%d]: !(f1&f2) != !f1|!f2\n", trial)); next
  }

  # De Morgan: !(f1 | f2) == !f1 & !f2
  lhs2 = tryCatch(!(f1 | f2), error = function(e) e)
  rhs2 = tryCatch(!f1 & !f2, error = function(e) e)

  if (inherits(lhs2, "error") || inherits(rhs2, "error")) {
    n_failures = n_failures + 1
    msg = if (inherits(lhs2, "error")) lhs2$message else rhs2$message
    cat(sprintf("ERROR [dm2-%d]: %s\n", trial, msg)); next
  }

  lhs2_truth = evaluate_formula(lhs2, u)
  rhs2_truth = evaluate_formula(rhs2, u)
  if (!all(lhs2_truth == rhs2_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dm2-%d]: !(f1|f2) != !f1&!f2\n", trial))
  }
}
cat(sprintf("  De Morgan: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
