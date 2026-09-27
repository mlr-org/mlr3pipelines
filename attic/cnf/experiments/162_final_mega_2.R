#!/usr/bin/env Rscript
# Final comprehensive fuzzer: many configurations, truth table comparison,
# plus algebraic property verification
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

configs = list(
  list(n_vars = 3, dom_size = 3, n_cl_range = c(3, 8), reps = 1000),
  list(n_vars = 4, dom_size = 3, n_cl_range = c(4, 10), reps = 500),
  list(n_vars = 2, dom_size = 6, n_cl_range = c(3, 8), reps = 500),
  list(n_vars = 5, dom_size = 2, n_cl_range = c(5, 15), reps = 500),
  list(n_vars = 3, dom_size = 4, n_cl_range = c(4, 10), reps = 500),
  list(n_vars = 6, dom_size = 2, n_cl_range = c(8, 20), reps = 300),
  list(n_vars = 2, dom_size = 8, n_cl_range = c(3, 8), reps = 300),
  list(n_vars = 4, dom_size = 4, n_cl_range = c(5, 12), reps = 300),
  list(n_vars = 3, dom_size = 5, n_cl_range = c(4, 10), reps = 300),
  list(n_vars = 7, dom_size = 2, n_cl_range = c(10, 25), reps = 200)
)

cat("=== Truth table comparison ===\n")
set.seed(162001)

for (ci in seq_along(configs)) {
  config = configs[[ci]]
  cat(sprintf("  Config %d: %dv%dd, %d-%d clauses, %d reps\n",
      ci, config$n_vars, config$dom_size, config$n_cl_range[1], config$n_cl_range[2], config$reps))

  for (trial in 1:config$reps) {
    u = CnfUniverse()
    dom = paste0("v", 1:config$dom_size)
    syms = list()
    for (i in 1:config$n_vars) {
      vname = paste0("X", i)
      syms[[vname]] = CnfSymbol(u, vname, dom)
    }

    n_cl = sample(config$n_cl_range[1]:config$n_cl_range[2], 1)
    clauses = lapply(1:n_cl, function(j) {
      n_sym = sample(1:min(4, config$n_vars), 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:(config$dom_size - 1), 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) < 2) next

    n_tests = n_tests + 1
    result = tryCatch(CnfFormula(clauses), error = function(e) e)
    if (inherits(result, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("    ERROR [%d-%d]: %s\n", ci, trial, result$message)); next
    }
    truth = evaluate_formula(result, u)
    raw_truth = evaluate_raw_clauses(clauses, u)
    if (!all(truth == raw_truth)) {
      n_failures = n_failures + 1
      cat(sprintf("    FAIL [%d-%d]: semantic mismatch\n", ci, trial))
    }
  }
}
cat(sprintf("  Truth table: %d tests, %d failures\n", n_tests, n_failures))

# === Algebraic properties ===
cat("\n=== Algebraic properties ===\n")
set.seed(162002)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    clauses = lapply(1:sample(1:3, 1), function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) == 0) return(NULL)
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  f = make_f()
  g = make_f()
  if (is.null(f) || is.null(g)) next

  tf = evaluate_formula(f, u)
  tg = evaluate_formula(g, u)

  # Double negation
  n_tests = n_tests + 1
  ff = tryCatch(!!f, error = function(e) NULL)
  if (!is.null(ff)) {
    tff = evaluate_formula(ff, u)
    if (!all(tff == tf)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [dblneg-%d]\n", trial))
    }
  }

  # Complement: f & !f = FALSE
  n_tests = n_tests + 1
  nf = tryCatch(!f, error = function(e) NULL)
  if (!is.null(nf)) {
    comp = tryCatch(f & nf, error = function(e) NULL)
    if (!is.null(comp)) {
      tcomp = evaluate_formula(comp, u)
      if (any(tcomp)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [comp-%d]\n", trial))
      }
    }
  }

  # De Morgan: !(f & g) == !f | !g
  n_tests = n_tests + 1
  lhs = tryCatch(!(f & g), error = function(e) NULL)
  rhs = tryCatch(!f | !g, error = function(e) NULL)
  if (!is.null(lhs) && !is.null(rhs)) {
    tlhs = evaluate_formula(lhs, u)
    trhs = evaluate_formula(rhs, u)
    expected = !(tf & tg)
    if (!all(tlhs == expected) || !all(trhs == expected)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [demorgan-%d]\n", trial))
    }
  }

  # Absorption: f & (f | g) == f
  n_tests = n_tests + 1
  absorbed = tryCatch(f & (f | g), error = function(e) NULL)
  if (!is.null(absorbed)) {
    ta = evaluate_formula(absorbed, u)
    if (!all(ta == tf)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [absorb-%d]\n", trial))
    }
  }
}
cat(sprintf("  Algebraic: %d tests, %d failures\n", n_tests, n_failures))

# === Operation chains ===
cat("\n=== Operation chains ===\n")
set.seed(162003)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  make_f = function() {
    clauses = lapply(1:sample(1:2, 1), function(j) {
      n_sym = sample(1:2, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
    if (length(clauses) == 0) return(NULL)
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  }

  fs = replicate(sample(3:5, 1), make_f(), simplify = FALSE)
  fs = Filter(Negate(is.null), fs)
  if (length(fs) < 3) next

  ts = lapply(fs, evaluate_formula, u)

  # Chain of ANDs
  n_tests = n_tests + 1
  result = tryCatch(Reduce(`&`, fs), error = function(e) NULL)
  if (!is.null(result)) {
    tr = evaluate_formula(result, u)
    expected = Reduce(`&`, ts)
    if (!all(tr == expected)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [and-chain-%d]\n", trial))
    }
  }

  # Chain of ORs
  n_tests = n_tests + 1
  result2 = tryCatch(Reduce(`|`, fs), error = function(e) NULL)
  if (!is.null(result2)) {
    tr2 = evaluate_formula(result2, u)
    expected2 = Reduce(`|`, ts)
    if (!all(tr2 == expected2)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [or-chain-%d]\n", trial))
    }
  }

  # Alternating AND/OR
  n_tests = n_tests + 1
  ops = sample(c("&", "|"), length(fs) - 1, replace = TRUE)
  result3 = fs[[1]]
  expected3 = ts[[1]]
  ok = TRUE
  for (i in seq_along(ops)) {
    if (ops[i] == "&") {
      result3 = tryCatch(result3 & fs[[i + 1]], error = function(e) NULL)
      expected3 = expected3 & ts[[i + 1]]
    } else {
      result3 = tryCatch(result3 | fs[[i + 1]], error = function(e) NULL)
      expected3 = expected3 | ts[[i + 1]]
    }
    if (is.null(result3)) { ok = FALSE; break }
  }
  if (ok) {
    tr3 = evaluate_formula(result3, u)
    if (!all(tr3 == expected3)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [mixed-chain-%d]\n", trial))
    }
  }
}
cat(sprintf("  Operation chains: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
