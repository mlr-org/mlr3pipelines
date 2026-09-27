#!/usr/bin/env Rscript
# Monotonicity and absorption law testing:
# - If f implies g, then f & h implies g & h and f | h implies g | h
# - Absorption: f & (f | g) == f, f | (f & g) == f
# - Consensus: (f & g) | (f & !g) == f
# These are fundamental properties that must hold.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Pattern 1: Absorption law: f & (f | g) == f ===
cat("=== Absorption: f & (f | g) == f ===\n")
set.seed(224001)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  sym_names = paste0("V", 1:4)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:4, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f = make_f(); g = make_f()
  if (is.null(f) || is.null(g)) next

  n_tests = n_tests + 1

  # f & (f | g) should equal f
  f_or_g = tryCatch(f | g, error = function(e) e)
  if (inherits(f_or_g, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [abs1-%d]: %s\n", trial, f_or_g$message)); next
  }

  result = tryCatch(f & f_or_g, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [abs1a-%d]: %s\n", trial, result$message)); next
  }

  t_f = evaluate_formula(f, u)
  t_result = evaluate_formula(result, u)
  if (!all(t_f == t_result)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [abs1-%d]: f & (f|g) != f\n", trial))
  }
}
cat(sprintf("  Absorption AND: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Absorption law: f | (f & g) == f ===
cat("\n=== Absorption: f | (f & g) == f ===\n")
set.seed(224002)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f = make_f(); g = make_f()
  if (is.null(f) || is.null(g)) next

  n_tests = n_tests + 1

  f_and_g = tryCatch(f & g, error = function(e) e)
  if (inherits(f_and_g, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [abs2-%d]: %s\n", trial, f_and_g$message)); next
  }

  result = tryCatch(f | f_and_g, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [abs2a-%d]: %s\n", trial, result$message)); next
  }

  t_f = evaluate_formula(f, u)
  t_result = evaluate_formula(result, u)
  if (!all(t_f == t_result)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [abs2-%d]: f | (f&g) != f\n", trial))
  }
}
cat(sprintf("  Absorption OR: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Consensus: (f & g) | (f & !g) == f ===
cat("\n=== Consensus: (f & g) | (f & !g) == f ===\n")
set.seed(224003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:2, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f = make_f(); g = make_f()
  if (is.null(f) || is.null(g)) next

  n_tests = n_tests + 1

  # (f & g) | (f & !g) should equal f
  ng = tryCatch(!g, error = function(e) e)
  if (inherits(ng, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [cons-%d]: %s\n", trial, ng$message)); next
  }

  fg = tryCatch(f & g, error = function(e) e)
  fng = tryCatch(f & ng, error = function(e) e)
  if (inherits(fg, "error") || inherits(fng, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [cons2-%d]: error\n", trial)); next
  }

  result = tryCatch(fg | fng, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [cons3-%d]: %s\n", trial, result$message)); next
  }

  t_f = evaluate_formula(f, u)
  t_result = evaluate_formula(result, u)
  if (!all(t_f == t_result)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [cons-%d]: (f&g)|(f&!g) != f\n", trial))
  }
}
cat(sprintf("  Consensus: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Monotonicity: f => g implies (f & h) => (g & h) ===
cat("\n=== Monotonicity ===\n")
set.seed(224004)

for (trial in 1:400) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:3)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(sym_names, n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f = make_f(); g = make_f(); h = make_f()
  if (any(sapply(list(f, g, h), is.null))) next

  # Make g = f | extra (so f implies g)
  g = tryCatch(f | g, error = function(e) e)
  if (inherits(g, "error")) next

  n_tests = n_tests + 1

  # f & h
  fh = tryCatch(f & h, error = function(e) e)
  # g & h
  gh = tryCatch(g & h, error = function(e) e)

  if (inherits(fh, "error") || inherits(gh, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [mono-%d]: error\n", trial)); next
  }

  t_fh = evaluate_formula(fh, u)
  t_gh = evaluate_formula(gh, u)

  # fh should imply gh (wherever fh is TRUE, gh must be TRUE)
  if (any(t_fh & !t_gh)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mono-%d]: monotonicity violated\n", trial))
  }
}
cat(sprintf("  Monotonicity: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
