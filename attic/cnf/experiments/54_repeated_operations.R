#!/usr/bin/env Rscript
# Test repeated operations and idempotence of formula operations
# f & f, f | f, f & f & f, (f & g) & (f & g), etc.
# Also test that operations don't corrupt the original formulas
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

# === Idempotence: f & f == f, f | f == f ===
cat("=== Idempotence ===\n")
set.seed(54001)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c"))
  }

  n_cl = sample(1:4, 1)
  cls = lapply(1:n_cl, function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  t_f = evaluate_formula(f, u)

  # f & f == f
  ff_and = tryCatch(f & f, error = function(e) NULL)
  if (!is.null(ff_and)) {
    check_formula_truth(ff_and, t_f, u, sprintf("idemp-and-%d", trial))
  }

  # f | f == f
  ff_or = tryCatch(f | f, error = function(e) NULL)
  if (!is.null(ff_or)) {
    check_formula_truth(ff_or, t_f, u, sprintf("idemp-or-%d", trial))
  }
}
cat(sprintf("  Idempotence: %d tests, %d failures\n", n_tests, n_failures))

# === Triple repetition: f & f & f, f | f | f ===
cat("\n=== Triple repetition ===\n")
set.seed(54002)

for (trial in 1:200) {
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

  # f & f & f == f
  fff_and = tryCatch(f & f & f, error = function(e) NULL)
  if (!is.null(fff_and)) {
    check_formula_truth(fff_and, t_f, u, sprintf("triple-and-%d", trial))
  }

  # f | f | f == f
  fff_or = tryCatch(f | f | f, error = function(e) NULL)
  if (!is.null(fff_or)) {
    check_formula_truth(fff_or, t_f, u, sprintf("triple-or-%d", trial))
  }
}
cat(sprintf("  Triple repetition: %d tests, %d failures\n", n_tests, n_failures))

# === Non-corruption: operations don't modify originals ===
cat("\n=== Non-corruption ===\n")
set.seed(54003)

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

  # Record original truths
  t1_before = evaluate_formula(f1, u)
  t2_before = evaluate_formula(f2, u)

  # Do various operations
  tryCatch(f1 & f2, error = function(e) NULL)
  tryCatch(f1 | f2, error = function(e) NULL)
  tryCatch(!f1, error = function(e) NULL)
  tryCatch(!f2, error = function(e) NULL)
  tryCatch(f1 & !f2, error = function(e) NULL)

  # Check originals are unchanged
  t1_after = evaluate_formula(f1, u)
  t2_after = evaluate_formula(f2, u)

  n_tests = n_tests + 1
  if (!all(t1_before == t1_after)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [non-corrupt-%d]: f1 changed after operations\n", trial))
  }
  n_tests = n_tests + 1
  if (!all(t2_before == t2_after)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [non-corrupt-%d]: f2 changed after operations\n", trial))
  }
}
cat(sprintf("  Non-corruption: %d tests, %d failures\n", n_tests, n_failures))

# === Absorption: f & (f | g) == f, f | (f & g) == f ===
cat("\n=== Absorption ===\n")
set.seed(54004)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b"))
  }

  cls1 = lapply(1:sample(1:2, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls2 = lapply(1:sample(1:2, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b"), 1)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(cls1), error = function(e) NULL)
  g = tryCatch(CnfFormula(cls2), error = function(e) NULL)
  if (is.null(f) || is.null(g)) next

  t_f = evaluate_formula(f, u)
  t_g = evaluate_formula(g, u)

  # f & (f | g) == f
  f_or_g = tryCatch(f | g, error = function(e) NULL)
  if (!is.null(f_or_g)) {
    result = tryCatch(f & f_or_g, error = function(e) NULL)
    if (!is.null(result)) {
      check_formula_truth(result, t_f, u, sprintf("absorb-and-%d", trial))
    }
  }

  # f | (f & g) == f
  f_and_g = tryCatch(f & g, error = function(e) NULL)
  if (!is.null(f_and_g)) {
    result = tryCatch(f | f_and_g, error = function(e) NULL)
    if (!is.null(result)) {
      check_formula_truth(result, t_f, u, sprintf("absorb-or-%d", trial))
    }
  }
}
cat(sprintf("  Absorption: %d tests, %d failures\n", n_tests, n_failures))

# === Consensus: (f & g) | (f & !g) == f ===
cat("\n=== Consensus ===\n")
set.seed(54005)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:2) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c"))
  }

  cls1 = lapply(1:sample(1:2, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls2 = lapply(1:sample(1:2, 1), function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(cls1), error = function(e) NULL)
  g = tryCatch(CnfFormula(cls2), error = function(e) NULL)
  if (is.null(f) || is.null(g)) next

  t_f = evaluate_formula(f, u)

  # (f & g) | (f & !g) == f
  not_g = tryCatch(!g, error = function(e) NULL)
  if (is.null(not_g)) next
  fg = tryCatch(f & g, error = function(e) NULL)
  fng = tryCatch(f & not_g, error = function(e) NULL)
  if (is.null(fg) || is.null(fng)) next
  result = tryCatch(fg | fng, error = function(e) NULL)
  if (!is.null(result)) {
    check_formula_truth(result, t_f, u, sprintf("consensus-%d", trial))
  }
}
cat(sprintf("  Consensus: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
