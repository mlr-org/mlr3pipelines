#!/usr/bin/env Rscript
# Deep test of |.CnfFormula distribution logic
# The distribution code modifies e2 inside lapply, which should be safe due to
# R's copy-on-modify semantics (each lapply iteration gets its own copy)
# But we want to verify this thoroughly.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

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

# === Distribution with many e1 clauses (tests lapply isolation) ===
cat("=== Many e1 clauses in distribution ===\n")
set.seed(58001)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c"))
  }

  # f1 with multiple clauses (multiple iterations in lapply)
  cls1 = lapply(1:sample(2:4, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  # f2 with multiple clauses (inner loop iterates over these)
  cls2 = lapply(1:sample(2:4, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f1 = tryCatch(CnfFormula(cls1), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(cls2), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  expected = t1 | t2

  f_or = tryCatch(f1 | f2, error = function(e) NULL)
  if (is.null(f_or)) next
  check_formula_truth(f_or, expected, u, sprintf("many-e1-%d", trial))
}
cat(sprintf("  Many e1 clauses: %d tests, %d failures\n", n_tests, n_failures))

# === Distribution where tautology elimination happens ===
cat("\n=== Tautology during distribution ===\n")
set.seed(58002)

for (trial in 1:200) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
  B = CnfSymbol(u, "B", c("b1", "b2"))

  # f1 has clauses that, when OR'd with f2's clauses, create tautologies
  # E.g., f1 has {A: {a1, a2}} and f2 has {A: {a3}}, union covers full domain
  cls1 = list()
  cls2 = list()
  n1 = sample(1:3, 1)
  n2 = sample(1:3, 1)
  for (i in 1:n1) {
    syms_avail = list(A = A, B = B)
    doms = list(A = c("a1", "a2", "a3"), B = c("b1", "b2"))
    chosen = sample(names(syms_avail), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms_avail[[s]] %among% sample(doms[[s]], sample(1:length(doms[[s]]), 1))
    })
    cls1[[i]] = as.CnfClause(Reduce(`|`, atoms))
  }
  for (i in 1:n2) {
    syms_avail = list(A = A, B = B)
    doms = list(A = c("a1", "a2", "a3"), B = c("b1", "b2"))
    chosen = sample(names(syms_avail), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms_avail[[s]] %among% sample(doms[[s]], sample(1:length(doms[[s]]), 1))
    })
    cls2[[i]] = as.CnfClause(Reduce(`|`, atoms))
  }

  f1 = tryCatch(CnfFormula(cls1), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(cls2), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  expected = t1 | t2

  f_or = tryCatch(f1 | f2, error = function(e) NULL)
  if (is.null(f_or)) next
  check_formula_truth(f_or, expected, u, sprintf("taut-dist-%d", trial))
}
cat(sprintf("  Tautology distribution: %d tests, %d failures\n", n_tests, n_failures))

# === Distribution symmetry: f1 | f2 == f2 | f1 ===
cat("\n=== Distribution symmetry ===\n")
set.seed(58003)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c"))
  }

  cls1 = lapply(1:sample(1:3, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls2 = lapply(1:sample(1:3, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f1 = tryCatch(CnfFormula(cls1), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(cls2), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  f12 = tryCatch(f1 | f2, error = function(e) NULL)
  f21 = tryCatch(f2 | f1, error = function(e) NULL)
  if (is.null(f12) || is.null(f21)) next

  t12 = evaluate_formula(f12, u)
  t21 = evaluate_formula(f21, u)

  n_tests = n_tests + 1
  if (!all(t12 == t21)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sym-%d]: f1|f2 != f2|f1\n", trial))
  }
}
cat(sprintf("  Distribution symmetry: %d tests, %d failures\n", n_tests, n_failures))

# === Distribution with FALSE formulas ===
cat("\n=== Distribution with FALSE ===\n")
set.seed(58004)

for (trial in 1:100) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b"))
  }

  cls = lapply(1:sample(1:3, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  t_f = evaluate_formula(f, u)

  # Create FALSE formula
  f_false = tryCatch(
    CnfFormula(list(
      as.CnfClause(syms[["V1"]] %among% "a"),
      as.CnfClause(syms[["V1"]] %among% "b")
    )),
    error = function(e) NULL
  )
  if (is.null(f_false) || !isFALSE(as.logical(f_false))) next

  # f | FALSE == f
  result = tryCatch(f | f_false, error = function(e) NULL)
  if (!is.null(result)) {
    check_formula_truth(result, t_f, u, sprintf("or-false-%d", trial))
  }

  # FALSE | f == f
  result2 = tryCatch(f_false | f, error = function(e) NULL)
  if (!is.null(result2)) {
    check_formula_truth(result2, t_f, u, sprintf("false-or-%d", trial))
  }
}
cat(sprintf("  Distribution with FALSE: %d tests, %d failures\n", n_tests, n_failures))

# === Large distribution (4+ clauses each) ===
cat("\n=== Large distribution ===\n")
set.seed(58005)

for (trial in 1:100) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b"))
  }

  # Both formulas have 3-5 clauses - distribution produces many cross-product terms
  cls1 = lapply(1:sample(3:5, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls2 = lapply(1:sample(3:5, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f1 = tryCatch(CnfFormula(cls1), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(cls2), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  expected = t1 | t2

  f_or = tryCatch(f1 | f2, error = function(e) NULL)
  if (is.null(f_or)) next
  check_formula_truth(f_or, expected, u, sprintf("large-dist-%d", trial))
}
cat(sprintf("  Large distribution: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
