#!/usr/bin/env Rscript
# Test interactions when variables have different domain sizes.
# The simplifier handles domain-dependent operations (tautology checks,
# complement computation) per-symbol. Mixed domains stress these paths.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

evaluate_raw_clauses = function(clauses, universe) {
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in clauses) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }
  raw_truth
}

# === Binary + ternary + quaternary mixed ===
cat("=== Mixed domain sizes: 2 + 3 + 4 ===\n")
set.seed(151001)

for (trial in 1:1000) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("0", "1"))
  B = CnfSymbol(u, "B", c("x", "y", "z"))
  C = CnfSymbol(u, "C", c("a", "b", "c", "d"))
  syms = list(A = A, B = B, C = C)
  doms = list(A = c("0", "1"), B = c("x", "y", "z"), C = c("a", "b", "c", "d"))

  n_cl = sample(3:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) {
      dom = doms[[s]]
      syms[[s]] %among% sample(dom, sample(1:(length(dom) - 1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [234-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [234-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  2+3+4 domains: %d tests, %d failures\n", n_tests, n_failures))

# === Singleton + large domain ===
cat("\n=== Singleton + large domain ===\n")
set.seed(151002)

for (trial in 1:500) {
  u = CnfUniverse()
  # Singleton domain: only one value, so any clause with this symbol
  # is either tautological (all values) or equivalent to the atom
  A = CnfSymbol(u, "A", c("only"))
  B = CnfSymbol(u, "B", c("a", "b", "c", "d", "e", "f"))
  C = CnfSymbol(u, "C", c("x", "y", "z"))
  syms = list(A = A, B = B, C = C)
  doms = list(A = c("only"), B = c("a", "b", "c", "d", "e", "f"), C = c("x", "y", "z"))

  n_cl = sample(2:6, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) {
      dom = doms[[s]]
      if (length(dom) == 1) {
        syms[[s]] %among% dom  # Always tautological for singleton
      } else {
        syms[[s]] %among% sample(dom, sample(1:(length(dom) - 1), 1))
      }
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [sing-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [sing-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  Singleton + large: %d tests, %d failures\n", n_tests, n_failures))

# === 4 vars with very different domain sizes ===
cat("\n=== 4 vars mixed domains (1,2,3,5) ===\n")
set.seed(151003)

for (trial in 1:500) {
  u = CnfUniverse()
  W = CnfSymbol(u, "W", c("w"))           # domain 1
  X = CnfSymbol(u, "X", c("0", "1"))      # domain 2
  Y = CnfSymbol(u, "Y", c("a", "b", "c")) # domain 3
  Z = CnfSymbol(u, "Z", c("p", "q", "r", "s", "t"))  # domain 5
  syms = list(W = W, X = X, Y = Y, Z = Z)
  doms = list(W = c("w"), X = c("0", "1"), Y = c("a", "b", "c"), Z = c("p", "q", "r", "s", "t"))

  n_cl = sample(3:7, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) {
      dom = doms[[s]]
      # For singleton, sometimes use the value, sometimes not (though "not" would be empty)
      if (length(dom) == 1) {
        syms[[s]] %among% dom
      } else {
        syms[[s]] %among% sample(dom, sample(1:(length(dom) - 1), 1))
      }
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [1235-%d]: %s\n", trial, result$message)); next
  }
  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(clauses, u)
  if (!all(truth == raw_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [1235-%d]: semantic mismatch\n", trial))
  }
}
cat(sprintf("  4 vars mixed domains: %d tests, %d failures\n", n_tests, n_failures))

# === Negation with mixed domains ===
cat("\n=== Negation with mixed domains ===\n")
set.seed(151004)

for (trial in 1:500) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("0", "1"))
  B = CnfSymbol(u, "B", c("a", "b", "c", "d", "e"))
  syms = list(A = A, B = B)
  doms = list(A = c("0", "1"), B = c("a", "b", "c", "d", "e"))

  n_cl = sample(1:3, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) {
      dom = doms[[s]]
      syms[[s]] %among% sample(dom, sample(1:(length(dom) - 1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) == 0) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next
  t_f = evaluate_formula(f, u)

  n_tests = n_tests + 1
  neg_f = tryCatch(!f, error = function(e) NULL)
  if (is.null(neg_f)) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [neg-mixed-%d]\n", trial)); next
  }
  t_neg = evaluate_formula(neg_f, u)
  if (!all(t_neg == !t_f)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [neg-mixed-%d]: negation wrong\n", trial))
  }

  # Double check complement
  n_tests = n_tests + 1
  f_and_neg = tryCatch(f & neg_f, error = function(e) NULL)
  if (!is.null(f_and_neg)) {
    t_both = evaluate_formula(f_and_neg, u)
    if (any(t_both)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [comp-mixed-%d]: f & !f not FALSE\n", trial))
    }
  }
}
cat(sprintf("  Negation mixed domains: %d tests, %d failures\n", n_tests, n_failures))

# === Operations between formulas with mixed domains ===
cat("\n=== Mixed domain operations ===\n")
set.seed(151005)

for (trial in 1:500) {
  u = CnfUniverse()
  A = CnfSymbol(u, "A", c("0", "1"))
  B = CnfSymbol(u, "B", c("a", "b", "c"))
  C = CnfSymbol(u, "C", c("x", "y", "z", "w"))
  syms = list(A = A, B = B, C = C)
  doms = list(A = c("0", "1"), B = c("a", "b", "c"), C = c("x", "y", "z", "w"))

  make_f = function() {
    clauses = lapply(1:sample(1:2, 1), function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) {
        dom = doms[[s]]
        syms[[s]] %among% sample(dom, sample(1:(length(dom) - 1), 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) == 0) return(NULL)
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)

  # f1 & f2
  n_tests = n_tests + 1
  r_and = tryCatch(f1 & f2, error = function(e) NULL)
  if (!is.null(r_and)) {
    t_and = evaluate_formula(r_and, u)
    if (!all(t_and == (t1 & t2))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [and-mixed-%d]\n", trial))
    }
  }

  # f1 | f2
  n_tests = n_tests + 1
  r_or = tryCatch(f1 | f2, error = function(e) NULL)
  if (!is.null(r_or)) {
    t_or = evaluate_formula(r_or, u)
    if (!all(t_or == (t1 | t2))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [or-mixed-%d]\n", trial))
    }
  }
}
cat(sprintf("  Mixed domain operations: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
