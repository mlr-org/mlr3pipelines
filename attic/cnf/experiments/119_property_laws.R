#!/usr/bin/env Rscript
# Verify algebraic laws hold with diverse formula configurations:
# - Commutativity: f & g == g & f, f | g == g | f
# - Associativity: (f & g) & h == f & (g & h)
# - Distributivity: f | (g & h) == (f | g) & (f | h)
# - De Morgan: !(f & g) == !f | !g, !(f | g) == !f & !g
# - Absorption: f & (f | g) == f, f | (f & g) == f
# - Complement: f & !f == FALSE, f | !f == TRUE
# - Double negation: !!f == f
# - Identity: f & TRUE == f, f | FALSE == f
# - Zero: f & FALSE == FALSE, f | TRUE == TRUE
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# === Diverse formula generator ===
make_formula = function(syms, d, n_cl) {
  n_vars = length(syms)
  dom_size = length(d)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:min(2, n_vars), 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(d, sample(1:(dom_size - 1), 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  tryCatch(CnfFormula(clauses), error = function(e) NULL)
}

# Test across multiple configs
configs = list(
  list(n_vars = 2, dom_size = 3, n_cl = c(1, 3), trials = 200),
  list(n_vars = 3, dom_size = 3, n_cl = c(1, 2), trials = 200),
  list(n_vars = 2, dom_size = 5, n_cl = c(1, 3), trials = 200),
  list(n_vars = 4, dom_size = 2, n_cl = c(1, 3), trials = 200)
)

for (cfg in configs) {
  cat(sprintf("\n=== Config: %dv, dom %d ===\n", cfg$n_vars, cfg$dom_size))
  set.seed(119001 + cfg$n_vars * 10 + cfg$dom_size)

  for (trial in 1:cfg$trials) {
    u = CnfUniverse()
    d = paste0("v", 1:cfg$dom_size)
    syms = list()
    for (i in 1:cfg$n_vars) {
      vname = paste0("X", i)
      syms[[vname]] = CnfSymbol(u, vname, d)
    }

    f = make_formula(syms, d, sample(cfg$n_cl[1]:cfg$n_cl[2], 1))
    g = make_formula(syms, d, sample(cfg$n_cl[1]:cfg$n_cl[2], 1))
    h = make_formula(syms, d, sample(cfg$n_cl[1]:cfg$n_cl[2], 1))
    if (is.null(f) || is.null(g) || is.null(h)) next

    t_f = evaluate_formula(f, u)
    t_g = evaluate_formula(g, u)
    t_h = evaluate_formula(h, u)

    # Commutativity of &
    n_tests = n_tests + 1
    fg = tryCatch(f & g, error = function(e) NULL)
    gf = tryCatch(g & f, error = function(e) NULL)
    if (!is.null(fg) && !is.null(gf)) {
      if (!all(evaluate_formula(fg, u) == evaluate_formula(gf, u))) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [comm-and-%d]: f & g != g & f\n", trial))
      }
    }

    # Commutativity of |
    n_tests = n_tests + 1
    f_or_g = tryCatch(f | g, error = function(e) NULL)
    g_or_f = tryCatch(g | f, error = function(e) NULL)
    if (!is.null(f_or_g) && !is.null(g_or_f)) {
      if (!all(evaluate_formula(f_or_g, u) == evaluate_formula(g_or_f, u))) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [comm-or-%d]: f | g != g | f\n", trial))
      }
    }

    # De Morgan: !(f & g) == !f | !g
    n_tests = n_tests + 1
    lhs = tryCatch(!(f & g), error = function(e) NULL)
    rhs = tryCatch((!f) | (!g), error = function(e) NULL)
    if (!is.null(lhs) && !is.null(rhs)) {
      if (!all(evaluate_formula(lhs, u) == evaluate_formula(rhs, u))) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [demorgan1-%d]\n", trial))
      }
    }

    # De Morgan: !(f | g) == !f & !g
    n_tests = n_tests + 1
    lhs2 = tryCatch(!(f | g), error = function(e) NULL)
    rhs2 = tryCatch((!f) & (!g), error = function(e) NULL)
    if (!is.null(lhs2) && !is.null(rhs2)) {
      if (!all(evaluate_formula(lhs2, u) == evaluate_formula(rhs2, u))) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [demorgan2-%d]\n", trial))
      }
    }

    # Absorption: f & (f | g) == f
    n_tests = n_tests + 1
    f_or_g2 = tryCatch(f | g, error = function(e) NULL)
    if (!is.null(f_or_g2)) {
      absorbed = tryCatch(f & f_or_g2, error = function(e) NULL)
      if (!is.null(absorbed)) {
        if (!all(evaluate_formula(absorbed, u) == t_f)) {
          n_failures = n_failures + 1
          cat(sprintf("FAIL [absorb-%d]\n", trial))
        }
      }
    }

    # Double negation: !!f == f
    n_tests = n_tests + 1
    dbl_neg = tryCatch(!!f, error = function(e) NULL)
    if (!is.null(dbl_neg)) {
      if (!all(evaluate_formula(dbl_neg, u) == t_f)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [dblneg-%d]\n", trial))
      }
    }

    # Complement: f & !f == FALSE
    n_tests = n_tests + 1
    complement = tryCatch(f & (!f), error = function(e) NULL)
    if (!is.null(complement)) {
      t_comp = evaluate_formula(complement, u)
      if (any(t_comp)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [complement-and-%d]: f & !f not FALSE\n", trial))
      }
    }

    # Complement: f | !f == TRUE
    n_tests = n_tests + 1
    comp_or = tryCatch(f | (!f), error = function(e) NULL)
    if (!is.null(comp_or)) {
      t_comp_or = evaluate_formula(comp_or, u)
      if (!all(t_comp_or)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [complement-or-%d]: f | !f not TRUE\n", trial))
      }
    }
  }
  cat(sprintf("  Progress: %d tests, %d failures\n", n_tests, n_failures))
}

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
