#!/usr/bin/env Rscript
# Property-based testing: verify algebraic properties of CNF operations
# These properties MUST hold regardless of the simplification strategy
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

set.seed(45001)

mk_env = function() {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("x1", "x2", "x3"))
  Y = CnfSymbol(u, "Y", c("y1", "y2"))
  Z = CnfSymbol(u, "Z", c("z1", "z2"))
  list(u = u, X = X, Y = Y, Z = Z)
}

mk_formula = function(env) {
  u = env$u
  syms = list(X = env$X, Y = env$Y, Z = env$Z)
  n_cl = sample(1:3, 1)
  cls = lapply(1:n_cl, function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(c("X", "Y", "Z"), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  tryCatch(CnfFormula(cls), error = function(e) NULL)
}

eq = function(f1, f2, u) {
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)
  all(t1 == t2)
}

# === Commutativity: f & g == g & f ===
cat("=== Commutativity of & ===\n")
for (trial in 1:200) {
  env = mk_env()
  f = mk_formula(env); g = mk_formula(env)
  if (is.null(f) || is.null(g)) next
  fg = tryCatch(f & g, error = function(e) NULL)
  gf = tryCatch(g & f, error = function(e) NULL)
  if (is.null(fg) || is.null(gf)) next
  n_tests = n_tests + 1
  if (!eq(fg, gf, env$u)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [& commutative trial %d]\n", trial))
  }
}
cat(sprintf("  & commutativity: %d tests, %d failures\n", n_tests, n_failures))

# === Commutativity: f | g == g | f ===
cat("\n=== Commutativity of | ===\n")
for (trial in 1:200) {
  env = mk_env()
  f = mk_formula(env); g = mk_formula(env)
  if (is.null(f) || is.null(g)) next
  fg = tryCatch(f | g, error = function(e) NULL)
  gf = tryCatch(g | f, error = function(e) NULL)
  if (is.null(fg) || is.null(gf)) next
  n_tests = n_tests + 1
  if (!eq(fg, gf, env$u)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [| commutative trial %d]\n", trial))
  }
}
cat(sprintf("  | commutativity: %d tests, %d failures\n", n_tests, n_failures))

# === Associativity: (f & g) & h == f & (g & h) ===
cat("\n=== Associativity of & ===\n")
for (trial in 1:200) {
  env = mk_env()
  f = mk_formula(env); g = mk_formula(env); h = mk_formula(env)
  if (is.null(f) || is.null(g) || is.null(h)) next
  r1 = tryCatch((f & g) & h, error = function(e) NULL)
  r2 = tryCatch(f & (g & h), error = function(e) NULL)
  if (is.null(r1) || is.null(r2)) next
  n_tests = n_tests + 1
  if (!eq(r1, r2, env$u)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [& associative trial %d]\n", trial))
  }
}
cat(sprintf("  & associativity: %d tests, %d failures\n", n_tests, n_failures))

# === Associativity: (f | g) | h == f | (g | h) ===
cat("\n=== Associativity of | ===\n")
for (trial in 1:100) {
  env = mk_env()
  f = mk_formula(env); g = mk_formula(env); h = mk_formula(env)
  if (is.null(f) || is.null(g) || is.null(h)) next
  r1 = tryCatch((f | g) | h, error = function(e) NULL)
  r2 = tryCatch(f | (g | h), error = function(e) NULL)
  if (is.null(r1) || is.null(r2)) next
  n_tests = n_tests + 1
  if (!eq(r1, r2, env$u)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [| associative trial %d]\n", trial))
  }
}
cat(sprintf("  | associativity: %d tests, %d failures\n", n_tests, n_failures))

# === Distributivity: f & (g | h) == (f & g) | (f & h) ===
cat("\n=== Distributivity: & over | ===\n")
for (trial in 1:100) {
  env = mk_env()
  f = mk_formula(env); g = mk_formula(env); h = mk_formula(env)
  if (is.null(f) || is.null(g) || is.null(h)) next
  lhs = tryCatch(f & (g | h), error = function(e) NULL)
  rhs = tryCatch((f & g) | (f & h), error = function(e) NULL)
  if (is.null(lhs) || is.null(rhs)) next
  n_tests = n_tests + 1
  if (!eq(lhs, rhs, env$u)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [& over | distributive trial %d]\n", trial))
  }
}
cat(sprintf("  & over | distributivity: %d tests, %d failures\n", n_tests, n_failures))

# === De Morgan: !(f & g) == (!f) | (!g) ===
cat("\n=== De Morgan: !(f & g) ===\n")
for (trial in 1:100) {
  env = mk_env()
  f = mk_formula(env); g = mk_formula(env)
  if (is.null(f) || is.null(g)) next
  lhs = tryCatch(!(f & g), error = function(e) NULL)
  rhs = tryCatch((!f) | (!g), error = function(e) NULL)
  if (is.null(lhs) || is.null(rhs)) next
  n_tests = n_tests + 1
  if (!eq(lhs, rhs, env$u)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [De Morgan & trial %d]\n", trial))
  }
}
cat(sprintf("  De Morgan &: %d tests, %d failures\n", n_tests, n_failures))

# === De Morgan: !(f | g) == (!f) & (!g) ===
cat("\n=== De Morgan: !(f | g) ===\n")
for (trial in 1:100) {
  env = mk_env()
  f = mk_formula(env); g = mk_formula(env)
  if (is.null(f) || is.null(g)) next
  lhs = tryCatch(!(f | g), error = function(e) NULL)
  rhs = tryCatch((!f) & (!g), error = function(e) NULL)
  if (is.null(lhs) || is.null(rhs)) next
  n_tests = n_tests + 1
  if (!eq(lhs, rhs, env$u)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [De Morgan | trial %d]\n", trial))
  }
}
cat(sprintf("  De Morgan |: %d tests, %d failures\n", n_tests, n_failures))

# === Identity: f & TRUE == f ===
cat("\n=== Identity: f & TRUE ===\n")
for (trial in 1:100) {
  env = mk_env()
  f = mk_formula(env)
  if (is.null(f)) next
  r = tryCatch(f & as.CnfFormula(TRUE), error = function(e) NULL)
  if (is.null(r)) next
  n_tests = n_tests + 1
  if (!eq(f, r, env$u)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [f & TRUE trial %d]\n", trial))
  }
}
cat(sprintf("  f & TRUE: %d tests, %d failures\n", n_tests, n_failures))

# === Annihilation: f & FALSE == FALSE ===
cat("\n=== Annihilation: f & FALSE ===\n")
for (trial in 1:100) {
  env = mk_env()
  f = mk_formula(env)
  if (is.null(f)) next
  r = tryCatch(f & as.CnfFormula(FALSE), error = function(e) NULL)
  if (is.null(r)) next
  n_tests = n_tests + 1
  if (!isFALSE(as.logical(r))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [f & FALSE trial %d]\n", trial))
  }
}
cat(sprintf("  f & FALSE: %d tests, %d failures\n", n_tests, n_failures))

# === Identity: f | FALSE == f ===
cat("\n=== Identity: f | FALSE ===\n")
for (trial in 1:100) {
  env = mk_env()
  f = mk_formula(env)
  if (is.null(f)) next
  r = tryCatch(f | as.CnfFormula(FALSE), error = function(e) NULL)
  if (is.null(r)) next
  n_tests = n_tests + 1
  if (!eq(f, r, env$u)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [f | FALSE trial %d]\n", trial))
  }
}
cat(sprintf("  f | FALSE: %d tests, %d failures\n", n_tests, n_failures))

# === Annihilation: f | TRUE == TRUE ===
cat("\n=== Annihilation: f | TRUE ===\n")
for (trial in 1:100) {
  env = mk_env()
  f = mk_formula(env)
  if (is.null(f)) next
  r = tryCatch(f | as.CnfFormula(TRUE), error = function(e) NULL)
  if (is.null(r)) next
  n_tests = n_tests + 1
  if (!isTRUE(as.logical(r))) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [f | TRUE trial %d]\n", trial))
  }
}
cat(sprintf("  f | TRUE: %d tests, %d failures\n", n_tests, n_failures))

# === Complement: f & !f == FALSE ===
cat("\n=== Complement: f & !f ===\n")
for (trial in 1:100) {
  env = mk_env()
  f = mk_formula(env)
  if (is.null(f)) next
  nf = tryCatch(!f, error = function(e) NULL)
  if (is.null(nf)) next
  r = tryCatch(f & nf, error = function(e) NULL)
  if (is.null(r)) next
  n_tests = n_tests + 1
  t_r = evaluate_formula(r, env$u)
  if (any(t_r)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [f & !f trial %d]: some TRUE values\n", trial))
  }
}
cat(sprintf("  f & !f: %d tests, %d failures\n", n_tests, n_failures))

# === Complement: f | !f == TRUE ===
cat("\n=== Complement: f | !f ===\n")
for (trial in 1:100) {
  env = mk_env()
  f = mk_formula(env)
  if (is.null(f)) next
  nf = tryCatch(!f, error = function(e) NULL)
  if (is.null(nf)) next
  r = tryCatch(f | nf, error = function(e) NULL)
  if (is.null(r)) next
  n_tests = n_tests + 1
  t_r = evaluate_formula(r, env$u)
  if (!all(t_r)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [f | !f trial %d]: some FALSE values\n", trial))
  }
}
cat(sprintf("  f | !f: %d tests, %d failures\n", n_tests, n_failures))

# === Absorption: f & (f | g) == f ===
cat("\n=== Absorption: f & (f | g) ===\n")
for (trial in 1:100) {
  env = mk_env()
  f = mk_formula(env); g = mk_formula(env)
  if (is.null(f) || is.null(g)) next
  fg = tryCatch(f | g, error = function(e) NULL)
  if (is.null(fg)) next
  r = tryCatch(f & fg, error = function(e) NULL)
  if (is.null(r)) next
  n_tests = n_tests + 1
  if (!eq(f, r, env$u)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [absorption trial %d]\n", trial))
  }
}
cat(sprintf("  Absorption: %d tests, %d failures\n", n_tests, n_failures))

# === Idempotence: f & f == f ===
cat("\n=== Idempotence: f & f ===\n")
for (trial in 1:100) {
  env = mk_env()
  f = mk_formula(env)
  if (is.null(f)) next
  r = tryCatch(f & f, error = function(e) NULL)
  if (is.null(r)) next
  n_tests = n_tests + 1
  if (!eq(f, r, env$u)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [f & f trial %d]\n", trial))
  }
}
cat(sprintf("  f & f: %d tests, %d failures\n", n_tests, n_failures))

# === Idempotence: f | f == f ===
cat("\n=== Idempotence: f | f ===\n")
for (trial in 1:100) {
  env = mk_env()
  f = mk_formula(env)
  if (is.null(f)) next
  r = tryCatch(f | f, error = function(e) NULL)
  if (is.null(r)) next
  n_tests = n_tests + 1
  if (!eq(f, r, env$u)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [f | f trial %d]\n", trial))
  }
}
cat(sprintf("  f | f: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
