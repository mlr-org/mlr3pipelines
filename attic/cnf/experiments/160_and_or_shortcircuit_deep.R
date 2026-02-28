#!/usr/bin/env Rscript
# Deep testing of & and | shortcircuit paths:
# - &.CnfFormula: e1 converted first, then check e2
# - |.CnfFormula: e1_bare taken before conversion, then e2 checked
# - Edge case: e1 is TRUE atom, e2 is normal formula
# - Edge case: e1 is FALSE clause, e2 is TRUE formula
# - Verify all shortcircuit returns have correct type and semantics
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

u = CnfUniverse()
A = CnfSymbol(u, "A", c("a", "b", "c"))
B = CnfSymbol(u, "B", c("d", "e", "f"))

# Helper to evaluate any Cnf type
eval_any = function(obj, universe) {
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  obj_bare = unclass(obj)
  if (isTRUE(obj_bare)) return(rep(TRUE, nrow(assignments)))
  if (isFALSE(obj_bare)) return(rep(FALSE, nrow(assignments)))

  if (inherits(obj, "CnfAtom")) {
    return(assignments[[obj$symbol]] %in% obj$values)
  }
  if (inherits(obj, "CnfClause")) {
    result = rep(FALSE, nrow(assignments))
    for (sym in names(obj_bare)) {
      result = result | (assignments[[sym]] %in% obj_bare[[sym]])
    }
    return(result)
  }
  result = rep(TRUE, nrow(assignments))
  for (clause in obj_bare) {
    clause_result = rep(FALSE, nrow(assignments))
    for (sym in names(clause)) {
      clause_result = clause_result | (assignments[[sym]] %in% clause[[sym]])
    }
    result = result & clause_result
  }
  result
}

# All object types to test as operands
make_objects = function() {
  list(
    atom_true = as.CnfAtom(TRUE),
    atom_false = as.CnfAtom(FALSE),
    atom_a = A %among% c("a", "b"),
    atom_b = B %among% "d",
    clause_true = as.CnfClause(TRUE),
    clause_false = as.CnfClause(FALSE),
    clause_ab = A %among% "a" | B %among% "d",
    formula_true = as.CnfFormula(TRUE),
    formula_false = as.CnfFormula(FALSE),
    formula_normal = (A %among% c("a", "b")) & (B %among% c("d", "e"))
  )
}

objects = make_objects()

# === & shortcircuit exhaustive ===
cat("=== & shortcircuit paths ===\n")
for (n1 in names(objects)) {
  for (n2 in names(objects)) {
    o1 = objects[[n1]]
    o2 = objects[[n2]]
    n_tests = n_tests + 1

    result = tryCatch(o1 & o2, error = function(e) e)
    if (inherits(result, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [&-%s-%s]: %s\n", n1, n2, result$message))
      next
    }

    # Must be CnfFormula
    if (!inherits(result, "CnfFormula")) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [&-%s-%s]: not CnfFormula\n", n1, n2))
      next
    }

    # Semantic check
    t1 = eval_any(o1, u)
    t2 = eval_any(o2, u)
    tr = eval_any(result, u)
    if (!all(tr == (t1 & t2))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [&-%s-%s]: semantic\n", n1, n2))
    }
  }
}
cat(sprintf("  & paths: %d tests, %d failures\n", n_tests, n_failures))

# === | shortcircuit exhaustive ===
cat("\n=== | shortcircuit paths ===\n")
for (n1 in names(objects)) {
  for (n2 in names(objects)) {
    o1 = objects[[n1]]
    o2 = objects[[n2]]
    n_tests = n_tests + 1

    result = tryCatch(o1 | o2, error = function(e) e)
    if (inherits(result, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [|-%s-%s]: %s\n", n1, n2, result$message))
      next
    }

    # Semantic check
    t1 = eval_any(o1, u)
    t2 = eval_any(o2, u)
    tr = eval_any(result, u)
    if (!all(tr == (t1 | t2))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [|-%s-%s]: semantic\n", n1, n2))
    }
  }
}
cat(sprintf("  | paths: %d tests, %d failures\n", n_tests, n_failures))

# === Verify return types for key shortcircuit paths ===
cat("\n=== Return type verification ===\n")

# TRUE & x = x (returned as CnfFormula)
n_tests = n_tests + 1
r = as.CnfFormula(TRUE) & (A %among% "a" | B %among% "d")
if (!inherits(r, "CnfFormula")) {
  n_failures = n_failures + 1
  cat("FAIL: TRUE & clause not CnfFormula\n")
}

# x & FALSE = FALSE (returned as CnfFormula)
n_tests = n_tests + 1
r = (A %among% "a") & as.CnfFormula(FALSE)
if (!inherits(r, "CnfFormula") || !isFALSE(unclass(r))) {
  n_failures = n_failures + 1
  cat("FAIL: atom & FALSE not FALSE CnfFormula\n")
}

# FALSE | x = x (returned as CnfFormula)
n_tests = n_tests + 1
r = as.CnfFormula(FALSE) | (A %among% "a" & B %among% "d")
if (!inherits(r, "CnfFormula")) {
  n_failures = n_failures + 1
  cat("FAIL: FALSE | formula not CnfFormula\n")
}

# x | TRUE = TRUE (returned as CnfFormula)
n_tests = n_tests + 1
r = (A %among% "a") | as.CnfFormula(TRUE)
if (!inherits(r, "CnfFormula") || !isTRUE(unclass(r))) {
  n_failures = n_failures + 1
  cat("FAIL: atom | TRUE not TRUE CnfFormula\n")
}

cat(sprintf("  Return types: %d tests, %d failures\n", n_tests, n_failures))

# === Random shortcircuit stress ===
cat("\n=== Random shortcircuit stress ===\n")
set.seed(160001)

for (trial in 1:2000) {
  u2 = CnfUniverse()
  dom = c("a", "b", "c", "d")
  X = CnfSymbol(u2, "X", dom)
  Y = CnfSymbol(u2, "Y", dom)
  syms = list(X = X, Y = Y)

  # Random formula (possibly TRUE or FALSE)
  if (sample(3, 1) == 1) {
    f = as.CnfFormula(sample(c(TRUE, FALSE), 1))
  } else {
    clauses = lapply(1:sample(1:3, 1), function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) == 0) {
      f = as.CnfFormula(TRUE)
    } else {
      f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
      if (is.null(f)) next
    }
  }

  # Another random object
  type = sample(3, 1)
  if (type == 1) {
    g = X %among% sample(dom, sample(1:3, 1))  # CnfAtom
  } else if (type == 2) {
    atoms = lapply(sample(names(syms), sample(1:2, 1)), function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    g = as.CnfClause(Reduce(`|`, atoms))  # CnfClause
  } else {
    if (sample(3, 1) == 1) {
      g = as.CnfFormula(sample(c(TRUE, FALSE), 1))
    } else {
      gcl = lapply(1:sample(1:2, 1), function(j) {
        atoms = lapply(sample(names(syms), sample(1:2, 1)), function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
        as.CnfClause(Reduce(`|`, atoms))
      })
      gcl = gcl[!sapply(gcl, function(x) isTRUE(unclass(x)))]
      if (length(gcl) == 0) g = as.CnfFormula(TRUE)
      else g = tryCatch(CnfFormula(gcl), error = function(e) NULL)
      if (is.null(g)) next
    }
  }

  tf = eval_any(f, u2)
  tg = eval_any(g, u2)

  # Test &
  n_tests = n_tests + 1
  r = tryCatch(f & g, error = function(e) NULL)
  if (!is.null(r)) {
    tr = eval_any(r, u2)
    if (!all(tr == (tf & tg))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [rand-&-%d]\n", trial))
    }
  }

  # Test |
  n_tests = n_tests + 1
  r2 = tryCatch(f | g, error = function(e) NULL)
  if (!is.null(r2)) {
    tr2 = eval_any(r2, u2)
    if (!all(tr2 == (tf | tg))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [rand-|-%d]\n", trial))
    }
  }
}
cat(sprintf("  Random shortcircuit: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
