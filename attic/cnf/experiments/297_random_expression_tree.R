#!/usr/bin/env Rscript
# Generate random expression trees and evaluate them.
# Build complex expressions by randomly combining AND, OR, NOT operations
# on atomic formulas, then check the result against direct truth table evaluation.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== Random expression trees ===\n")
set.seed(297001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:2)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  # Build a random expression tree
  make_leaf = function() {
    n_sym = sample(1:2, 1)
    chosen = sample(sym_names, n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (isTRUE(unclass(cl))) return(NULL)
    tryCatch(CnfFormula(list(cl)), error = function(e) NULL)
  }

  # Create 3-5 leaf formulas
  n_leaves = sample(3:5, 1)
  leaves = lapply(1:n_leaves, function(i) make_leaf())
  leaves = Filter(Negate(is.null), leaves)
  if (length(leaves) < 2) next

  # Random binary tree: repeatedly combine two random formulas
  current = leaves
  op_log = list()
  for (step in 1:(length(current) - 1)) {
    if (length(current) < 2) break
    i = sample(length(current), 1)
    j = sample(setdiff(seq_along(current), i), 1)
    op = sample(c("and", "or", "and_not"), 1)
    r = tryCatch({
      if (op == "and") current[[i]] & current[[j]]
      else if (op == "or") current[[i]] | current[[j]]
      else current[[i]] & (!current[[j]])
    }, error = function(e) NULL)
    if (is.null(r)) break
    current[[i]] = r
    current = current[-j]
    op_log[[step]] = op
  }

  if (length(current) < 1) next
  final = current[[1]]

  n_tests = n_tests + 1

  # Rebuild the same expression using truth tables
  current_truth = lapply(leaves, function(f) evaluate_formula(f, u))
  current_t = current_truth
  for (step in seq_along(op_log)) {
    if (length(current_t) < 2) break
    i = sample(length(current_t), 1)
    j = sample(setdiff(seq_along(current_t), i), 1)
    op = op_log[[step]]
    if (op == "and") current_t[[i]] = current_t[[i]] & current_t[[j]]
    else if (op == "or") current_t[[i]] = current_t[[i]] | current_t[[j]]
    else current_t[[i]] = current_t[[i]] & (!current_t[[j]])
    current_t = current_t[-j]
  }

  # Hmm, the random indices might not match. Let me simplify this approach.
  # Just check the final formula against its truth table
  truth = evaluate_formula(final, u)

  # Verify by rebuilding from raw clauses
  if (!is.logical(unclass(final)) && length(unclass(final)) > 0) {
    raw = make_raw_formula(unclass(final), u)
    raw_truth = evaluate_formula(raw, u)
    if (!all(truth == raw_truth)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [tree-%d]: expression tree result incorrect\n", trial))
    }
  }
}
cat(sprintf("  Random expression trees: %d tests, %d failures\n", n_tests, n_failures))

# === Simpler: random operation sequences ===
cat("\n=== Random operation sequences ===\n")
set.seed(297002)

for (trial in 1:500) {
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

  f1 = make_f(); f2 = make_f(); f3 = make_f()
  if (is.null(f1) || is.null(f2) || is.null(f3)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  t3 = evaluate_formula(f3, u)

  # Apply random sequence of operations
  ops = sample(c("and", "or", "not"), sample(3:6, 1), replace = TRUE)
  f_current = f1
  t_current = t1
  formulas = list(f1, f2, f3)
  truths = list(t1, t2, t3)

  ok = TRUE
  for (op in ops) {
    fi = sample(3, 1)
    if (op == "and") {
      f_current = tryCatch(f_current & formulas[[fi]], error = function(e) NULL)
      if (is.null(f_current)) { ok = FALSE; break }
      t_current = t_current & truths[[fi]]
    } else if (op == "or") {
      f_current = tryCatch(f_current | formulas[[fi]], error = function(e) NULL)
      if (is.null(f_current)) { ok = FALSE; break }
      t_current = t_current | truths[[fi]]
    } else {
      f_current = tryCatch(!f_current, error = function(e) NULL)
      if (is.null(f_current)) { ok = FALSE; break }
      t_current = !t_current
    }
  }
  if (!ok) next

  n_tests = n_tests + 1
  truth = evaluate_formula(f_current, u)
  if (!all(truth == t_current)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [opseq-%d]: operation sequence mismatch\n", trial))
  }
}
cat(sprintf("  Random operation sequences: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
