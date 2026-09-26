#!/usr/bin/env Rscript
# Stress test De Morgan's laws:
# !(f & g) == !f | !g
# !(f | g) == !f & !g
# These exercise the negation + OR/AND interaction deeply.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== De Morgan AND ===\n")
set.seed(290001)

for (trial in 1:300) {
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

  f = make_f(); g = make_f()
  if (is.null(f) || is.null(g)) next

  n_tests = n_tests + 1

  # !(f & g) should == !f | !g
  lhs = tryCatch(!(f & g), error = function(e) NULL)
  nf = tryCatch(!f, error = function(e) NULL)
  ng = tryCatch(!g, error = function(e) NULL)
  if (is.null(lhs) || is.null(nf) || is.null(ng)) next
  rhs = tryCatch(nf | ng, error = function(e) NULL)
  if (is.null(rhs)) next

  tl = evaluate_formula(lhs, u)
  tr = evaluate_formula(rhs, u)
  if (!all(tl == tr)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dm-and-%d]: !(f&g) != !f|!g\n", trial))
  }
}
cat(sprintf("  De Morgan AND: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== De Morgan OR ===\n")
set.seed(290002)

for (trial in 1:300) {
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

  f = make_f(); g = make_f()
  if (is.null(f) || is.null(g)) next

  n_tests = n_tests + 1

  # !(f | g) should == !f & !g
  fog = tryCatch(f | g, error = function(e) NULL)
  if (is.null(fog)) next
  lhs = tryCatch(!fog, error = function(e) NULL)
  nf = tryCatch(!f, error = function(e) NULL)
  ng = tryCatch(!g, error = function(e) NULL)
  if (is.null(lhs) || is.null(nf) || is.null(ng)) next
  rhs = tryCatch(nf & ng, error = function(e) NULL)
  if (is.null(rhs)) next

  tl = evaluate_formula(lhs, u)
  tr = evaluate_formula(rhs, u)
  if (!all(tl == tr)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [dm-or-%d]: !(f|g) != !f&!g\n", trial))
  }
}
cat(sprintf("  De Morgan OR: %d tests, %d failures\n", n_tests, n_failures))

# === Nested De Morgan ===
cat("\n=== Nested De Morgan ===\n")
set.seed(290003)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  sym_names = paste0("V", 1:2)
  syms = list()
  for (s in sym_names) syms[[s]] = CnfSymbol(u, s, dom)

  make_f = function() {
    cls = lapply(1:sample(1:2, 1), function(j) {
      atoms = lapply(sym_names, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f = make_f(); g = make_f(); h = make_f()
  if (is.null(f) || is.null(g) || is.null(h)) next

  n_tests = n_tests + 1

  # !(!f & !g) should == f | g
  nf = tryCatch(!f, error = function(e) NULL)
  ng = tryCatch(!g, error = function(e) NULL)
  if (is.null(nf) || is.null(ng)) next
  lhs = tryCatch(!(nf & ng), error = function(e) NULL)
  rhs = tryCatch(f | g, error = function(e) NULL)
  if (is.null(lhs) || is.null(rhs)) next

  tl = evaluate_formula(lhs, u)
  tr = evaluate_formula(rhs, u)
  if (!all(tl == tr)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [nested-dm-%d]: !(!f & !g) != f | g\n", trial))
  }
}
cat(sprintf("  Nested De Morgan: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
