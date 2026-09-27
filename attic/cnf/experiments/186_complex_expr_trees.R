#!/usr/bin/env Rscript
# Complex expression trees: build random expression trees with &, |, !
# and verify semantic correctness at every step
# Focus on deep composition with many intermediate simplifications
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Pattern 1: Random binary expression trees ===
cat("=== Random expression trees ===\n")
set.seed(186001)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Build 3-5 leaf formulas
  n_leaves = sample(3:5, 1)
  leaves = list()
  leaf_truth = list()
  for (i in 1:n_leaves) {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    f = if (length(cls) >= 1) tryCatch(CnfFormula(cls), error = function(e) NULL) else NULL
    if (is.null(f)) {
      leaves[[i]] = as.CnfFormula(TRUE)
      leaf_truth[[i]] = rep(TRUE, 4^3)  # 4^3 for 3 vars domain 4
    } else {
      leaves[[i]] = f
      leaf_truth[[i]] = evaluate_formula(f, u)
    }
  }

  # Combine them with random operations
  n_tests = n_tests + 1
  current = leaves[[1]]
  current_truth = leaf_truth[[1]]
  ok = TRUE
  for (i in 2:n_leaves) {
    op = sample(c("&", "|"), 1)
    negate = sample(c(TRUE, FALSE), 1)
    target = if (negate) tryCatch(!leaves[[i]], error = function(e) NULL) else leaves[[i]]
    target_truth = if (negate) !leaf_truth[[i]] else leaf_truth[[i]]
    if (is.null(target)) { ok = FALSE; break }

    if (op == "&") {
      current = tryCatch(current & target, error = function(e) NULL)
      current_truth = current_truth & target_truth
    } else {
      current = tryCatch(current | target, error = function(e) NULL)
      current_truth = current_truth | target_truth
    }
    if (is.null(current)) { ok = FALSE; break }
  }
  if (!ok) next

  actual = evaluate_formula(current, u)
  if (!all(actual == current_truth)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [tree-%d]: expression tree mismatch\n", trial))
  }
}
cat(sprintf("  Expression trees: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Triple negation chains ===
cat("\n=== Triple negation chains ===\n")
set.seed(186002)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  n_cl = sample(2:4, 1)
  cls = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
  if (length(cls) < 1) next

  f = tryCatch(CnfFormula(cls), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  # !!!f should equal !f
  nf = tryCatch(!f, error = function(e) NULL)
  if (is.null(nf)) next
  nnf = tryCatch(!nf, error = function(e) NULL)
  if (is.null(nnf)) next
  nnnf = tryCatch(!nnf, error = function(e) NULL)
  if (is.null(nnnf)) next

  tf = evaluate_formula(f, u)
  tnf = evaluate_formula(nf, u)
  tnnnf = evaluate_formula(nnnf, u)
  if (!all(tnf == tnnnf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [3neg-%d]: !!!f != !f\n", trial))
  }
}
cat(sprintf("  Triple negation: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Chained OR then AND ===
cat("\n=== Chained OR-then-AND ===\n")
set.seed(186003)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create 3-4 formulas
  fs = list()
  for (i in 1:sample(3:4, 1)) {
    n_cl = sample(2:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    f = if (length(cls) >= 1) tryCatch(CnfFormula(cls), error = function(e) NULL) else NULL
    if (!is.null(f)) fs[[length(fs) + 1]] = f
  }
  if (length(fs) < 3) next

  n_tests = n_tests + 1
  # (f1 | f2) & (f3 | f4)
  f12 = tryCatch(fs[[1]] | fs[[2]], error = function(e) NULL)
  f34 = if (length(fs) >= 4) tryCatch(fs[[3]] | fs[[4]], error = function(e) NULL) else fs[[3]]
  if (is.null(f12) || is.null(f34)) next
  result = tryCatch(f12 & f34, error = function(e) NULL)
  if (is.null(result)) next

  t1 = evaluate_formula(fs[[1]], u)
  t2 = evaluate_formula(fs[[2]], u)
  t3 = evaluate_formula(fs[[3]], u)
  t4 = if (length(fs) >= 4) evaluate_formula(fs[[4]], u) else rep(FALSE, length(t1))
  if (length(fs) >= 4) {
    expected = (t1 | t2) & (t3 | t4)
  } else {
    expected = (t1 | t2) & t3
  }
  actual = evaluate_formula(result, u)
  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [orAnd-%d]: (f|g)&(h|k) mismatch\n", trial))
  }
}
cat(sprintf("  Chained OR-AND: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Mixed large domain + operations ===
cat("\n=== Large domain operations ===\n")
set.seed(186004)

for (trial in 1:200) {
  u = CnfUniverse()
  dom = paste0("v", 1:6)
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    n_cl = sample(2:4, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:4, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f = make_f(); g = make_f()
  if (is.null(f) || is.null(g)) next

  n_tests = n_tests + 1

  # Test: (f & g) | (!f & !g) should be semantically correct
  fg = tryCatch(f & g, error = function(e) NULL)
  nf = tryCatch(!f, error = function(e) NULL)
  ng = tryCatch(!g, error = function(e) NULL)
  if (is.null(fg) || is.null(nf) || is.null(ng)) next
  nfng = tryCatch(nf & ng, error = function(e) NULL)
  if (is.null(nfng)) next
  result = tryCatch(fg | nfng, error = function(e) NULL)
  if (is.null(result)) next

  tf = evaluate_formula(f, u)
  tg = evaluate_formula(g, u)
  expected = (tf & tg) | (!tf & !tg)
  actual = evaluate_formula(result, u)
  if (!all(actual == expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [ldops-%d]: mismatch\n", trial))
  }
}
cat(sprintf("  Large domain ops: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
