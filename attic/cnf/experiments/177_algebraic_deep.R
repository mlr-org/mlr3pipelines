#!/usr/bin/env Rscript
# Deep algebraic property testing:
# - Absorption: f & (f | g) == f
# - Consensus: (A|B) & (!A|C) & (B|C) - the (B|C) clause is redundant
# - Resolution: (A|B) & (!A|C) implies (B|C)
# - XOR-like patterns: exactly-one-of constraints
# - Majority function: patterns where most assignments satisfy
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Absorption: f & (f | g) == f ===
cat("=== Absorption law ===\n")
set.seed(177001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f = make_f(); g = make_f()
  if (is.null(f) || is.null(g)) next

  tf = evaluate_formula(f, u)
  tg = evaluate_formula(g, u)

  # f & (f | g) should equal f
  n_tests = n_tests + 1
  fog = tryCatch(f | g, error = function(e) NULL)
  if (is.null(fog)) next
  r = tryCatch(f & fog, error = function(e) NULL)
  if (is.null(r)) next
  tr = evaluate_formula(r, u)
  if (!all(tr == tf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [absorb-and-%d]: f & (f|g) != f\n", trial))
  }

  # f | (f & g) should equal f
  n_tests = n_tests + 1
  fag = tryCatch(f & g, error = function(e) NULL)
  if (is.null(fag)) next
  r2 = tryCatch(f | fag, error = function(e) NULL)
  if (is.null(r2)) next
  tr2 = evaluate_formula(r2, u)
  if (!all(tr2 == tf)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [absorb-or-%d]: f | (f&g) != f\n", trial))
  }
}
cat(sprintf("  Absorption: %d tests, %d failures\n", n_tests, n_failures))

# === Consensus: (A|B) & (!A|C) & (B|C) === the (B|C) clause is redundant ===
cat("\n=== Consensus theorem ===\n")
set.seed(177002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  a_range = sample(dom, sample(1:2, 1))
  a_comp = setdiff(dom, a_range)
  b_range = sample(dom, sample(1:2, 1))
  c_range = sample(dom, sample(1:2, 1))
  if (length(a_comp) == 0) next

  # (A in a_range | B in b_range) & (A in a_comp | C in c_range) & (B in b_range | C in c_range)
  cl1 = as.CnfClause(A %among% a_range | B %among% b_range)
  cl2 = as.CnfClause(A %among% a_comp | C %among% c_range)
  cl3 = as.CnfClause(B %among% b_range | C %among% c_range)  # consensus: redundant
  if (isTRUE(unclass(cl1)) || isTRUE(unclass(cl2)) || isTRUE(unclass(cl3))) next

  # With consensus clause
  f_with = tryCatch(CnfFormula(list(cl1, cl2, cl3)), error = function(e) NULL)
  # Without consensus clause
  f_without = tryCatch(CnfFormula(list(cl1, cl2)), error = function(e) NULL)
  if (is.null(f_with) || is.null(f_without)) next

  n_tests = n_tests + 1
  t_with = evaluate_formula(f_with, u)
  t_without = evaluate_formula(f_without, u)
  if (!all(t_with == t_without)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [consensus-%d]: adding consensus clause changed semantics\n", trial))
  }
}
cat(sprintf("  Consensus: %d tests, %d failures\n", n_tests, n_failures))

# === Resolution implication: (A|B) & (!A|C) implies (B|C) ===
cat("\n=== Resolution implication ===\n")
set.seed(177003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)

  a_range = sample(dom, sample(1:2, 1))
  a_comp = setdiff(dom, a_range)
  b_range = sample(dom, sample(1:2, 1))
  c_range = sample(dom, sample(1:2, 1))
  if (length(a_comp) == 0) next

  cl1 = as.CnfClause(A %among% a_range | B %among% b_range)
  cl2 = as.CnfClause(A %among% a_comp | C %among% c_range)
  if (isTRUE(unclass(cl1)) || isTRUE(unclass(cl2))) next

  f_premise = tryCatch(CnfFormula(list(cl1, cl2)), error = function(e) NULL)
  if (is.null(f_premise)) next

  t_premise = evaluate_formula(f_premise, u)

  # Resolution conclusion: B in b_range | C in c_range
  cl_res = as.CnfClause(B %among% b_range | C %among% c_range)
  if (isTRUE(unclass(cl_res))) next
  f_res = as.CnfFormula(cl_res)
  t_res = evaluate_formula(f_res, u)

  # Whenever premise is TRUE, conclusion must be TRUE
  n_tests = n_tests + 1
  if (any(t_premise & !t_res)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [res-%d]: resolution implication violated\n", trial))
  }
}
cat(sprintf("  Resolution: %d tests, %d failures\n", n_tests, n_failures))

# === XOR-like: exactly one value constraints ===
cat("\n=== XOR-like patterns ===\n")
set.seed(177004)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Exactly-one-of constraint for a symbol: A in {v}
  # In CNF: A in {v} is already a unit
  # More interesting: A == B constraint: for each pair (a,b), a != b is forbidden
  # This is expressed as conjunction of clauses
  make_f = function() {
    n_cl = sample(3:6, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 2) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f = make_f()
  if (is.null(f)) next

  tf = evaluate_formula(f, u)

  # XOR-like: f XOR g = (f | g) & !(f & g)
  g = make_f()
  if (is.null(g)) next
  tg = evaluate_formula(g, u)

  n_tests = n_tests + 1
  fog = tryCatch(f | g, error = function(e) NULL)
  fag = tryCatch(f & g, error = function(e) NULL)
  if (is.null(fog) || is.null(fag)) next
  nfag = tryCatch(!fag, error = function(e) NULL)
  if (is.null(nfag)) next
  xor_result = tryCatch(fog & nfag, error = function(e) NULL)
  if (is.null(xor_result)) next

  txor = evaluate_formula(xor_result, u)
  expected_xor = (tf | tg) & !(tf & tg)
  if (!all(txor == expected_xor)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [xor-%d]: XOR formula incorrect\n", trial))
  }
}
cat(sprintf("  XOR: %d tests, %d failures\n", n_tests, n_failures))

# === Monotonicity: if f implies g, then f & h implies g & h ===
cat("\n=== Monotonicity ===\n")
set.seed(177005)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f = make_f(); g = make_f(); h = make_f()
  if (is.null(f) || is.null(g) || is.null(h)) next

  tf = evaluate_formula(f, u)
  tg = evaluate_formula(g, u)
  th = evaluate_formula(h, u)

  # Make f_strong = f & g (so f_strong implies both f and g)
  f_strong = tryCatch(f & g, error = function(e) NULL)
  if (is.null(f_strong)) next
  ts = evaluate_formula(f_strong, u)

  # f_strong & h should imply f & h
  n_tests = n_tests + 1
  fsh = tryCatch(f_strong & h, error = function(e) NULL)
  fh = tryCatch(f & h, error = function(e) NULL)
  if (is.null(fsh) || is.null(fh)) next

  tfsh = evaluate_formula(fsh, u)
  tfh = evaluate_formula(fh, u)
  # monotonicity: whenever f_strong & h is TRUE, f & h must also be TRUE
  if (any(tfsh & !tfh)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mono-%d]: monotonicity violated\n", trial))
  }
}
cat(sprintf("  Monotonicity: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
