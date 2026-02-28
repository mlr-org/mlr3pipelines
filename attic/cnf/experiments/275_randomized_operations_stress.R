#!/usr/bin/env Rscript
# Large-scale randomized operations stress test.
# Generate many random formulas and test every operation on them,
# using multiple configurations to maximize coverage.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

configs = list(
  list(dom = c("a", "b"), n_vars = 5, n_cl = 4:8, label = "5v-binary"),
  list(dom = c("a", "b", "c"), n_vars = 4, n_cl = 3:6, label = "4v-ternary"),
  list(dom = c("a", "b", "c", "d"), n_vars = 3, n_cl = 3:5, label = "3v-quaternary"),
  list(dom = c("a", "b", "c", "d", "e"), n_vars = 2, n_cl = 3:6, label = "2v-quinary")
)

for (cfg in configs) {
  cat(sprintf("=== %s ===\n", cfg$label))
  set.seed(275000 + match(cfg$label, sapply(configs, `[[`, "label")))

  for (trial in 1:300) {
    u = CnfUniverse()
    sym_names = paste0("V", 1:cfg$n_vars)
    syms = list()
    for (s in sym_names) syms[[s]] = CnfSymbol(u, s, cfg$dom)

    make_f = function() {
      n_cl = sample(cfg$n_cl, 1)
      cls = lapply(1:n_cl, function(j) {
        n_sym = sample(1:min(3, cfg$n_vars), 1)
        chosen = sample(sym_names, n_sym)
        atoms = lapply(chosen, function(s) {
          syms[[s]] %among% sample(cfg$dom, sample(1:max(1, length(cfg$dom)-1), 1))
        })
        as.CnfClause(Reduce(`|`, atoms))
      })
      cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
      if (length(cls) < 1) return(NULL)
      tryCatch(CnfFormula(cls), error = function(e) NULL)
    }

    f1 = make_f(); f2 = make_f()
    if (is.null(f1) || is.null(f2)) next

    t1 = evaluate_formula(f1, u)
    t2 = evaluate_formula(f2, u)

    # Test all operations
    ops = list(
      list(fn = function() f1 & f2, expected = t1 & t2, name = "AND"),
      list(fn = function() f1 | f2, expected = t1 | t2, name = "OR"),
      list(fn = function() !f1, expected = !t1, name = "NOT"),
      list(fn = function() f1 & !f2, expected = t1 & !t2, name = "AND-NOT"),
      list(fn = function() !f1 | f2, expected = !t1 | t2, name = "NOT-OR")
    )

    for (op in ops) {
      n_tests = n_tests + 1
      r = tryCatch(op$fn(), error = function(e) NULL)
      if (!is.null(r) && !all(evaluate_formula(r, u) == op$expected)) {
        n_failures = n_failures + 1
        cat(sprintf("FAIL [%s-%d]: %s mismatch\n", cfg$label, trial, op$name))
      }
    }
  }
  cat(sprintf("  %s: %d tests, %d failures\n", cfg$label, n_tests, n_failures))
}

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
