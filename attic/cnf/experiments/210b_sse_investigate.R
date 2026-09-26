#!/usr/bin/env Rscript
# Investigate the SSE completeness failure from experiment 210, trial 359
source("experiments/test_harness.R")

set.seed(210002)
u = CnfUniverse()
dom = c("a", "b", "c", "d", "e")
A = CnfSymbol(u, "A", dom)
B = CnfSymbol(u, "B", dom)
C = CnfSymbol(u, "C", dom)
syms = list(A = A, B = B, C = C)

for (trial in 1:359) {
  a1 = sample(dom, sample(1:3, 1))
  a2 = sample(dom, sample(2:4, 1))
  b1 = sample(dom, sample(1:3, 1))
  c1 = sample(dom, sample(1:3, 1))
  clauses = list(
    as.CnfClause(A %among% a1 | B %among% b1),
    as.CnfClause(A %among% a2 | B %among% b1 | C %among% c1)
  )
  for (j in 1:sample(2:5, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3 && trial < 359) next
  if (trial == 359) {
    cat("Input clauses:\n")
    for (i in seq_along(clauses)) {
      cl = unclass(clauses[[i]])
      parts = sapply(names(cl), function(s) sprintf("%s={%s}", s, paste(cl[[s]], collapse=",")))
      cat(sprintf("  %d: %s\n", i, paste(parts, collapse=" | ")))
    }
    result = CnfFormula(clauses)
    cat("\nSimplified:\n")
    print(result)
    f_bare = unclass(result)
    if (!is.logical(f_bare)) {
      cat("\nSimplified clauses:\n")
      for (i in seq_along(f_bare)) {
        parts = sapply(names(f_bare[[i]]), function(s) sprintf("%s={%s}", s, paste(f_bare[[i]][[s]], collapse=",")))
        cat(sprintf("  %d: %s\n", i, paste(parts, collapse=" | ")))
      }
    }
    # Semantic check
    raw_truth = rep(TRUE, 5^3)  # 3 vars, 5 values each
    varnames = ls(u)
    domains = lapply(varnames, function(v) get(v, u))
    names(domains) = varnames
    assignments = expand.grid(domains, stringsAsFactors = FALSE)
    raw_truth = rep(TRUE, nrow(assignments))
    for (cl in clauses) {
      cl_bare = unclass(cl)
      if (isTRUE(cl_bare)) next
      if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
      cl_truth = rep(FALSE, nrow(assignments))
      for (sym in names(cl_bare)) cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
      raw_truth = raw_truth & cl_truth
    }
    simp_truth = evaluate_formula(result, u)
    cat(sprintf("\nSemantic match: %s\n", all(simp_truth == raw_truth)))
    cat(sprintf("Satisfying assignments raw: %d, simplified: %d\n", sum(raw_truth), sum(simp_truth)))
  }
}
