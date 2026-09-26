#!/usr/bin/env Rscript
# Reproduce the semantic mismatch from experiment 173 "Mixed CnfClause + CnfFormula"
source("experiments/test_harness.R")

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

set.seed(173001)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  cls = lapply(1:sample(2:3, 1), function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]

  f_cls = lapply(1:sample(1:2, 1), function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  f_cls = f_cls[!sapply(f_cls, function(x) isTRUE(unclass(x)))]
  if (length(f_cls) < 1 || length(cls) < 1) next
  inner_f = tryCatch(CnfFormula(f_cls), error = function(e) NULL)
  if (is.null(inner_f)) next

  mixed = c(cls, list(inner_f))

  result = tryCatch(CnfFormula(mixed), error = function(e) e)
  if (inherits(result, "error")) next

  all_clauses = c(cls, as.list(inner_f))
  all_clauses = all_clauses[!sapply(all_clauses, function(x) {
    b = unclass(x)
    isTRUE(b) || isFALSE(b)
  })]
  if (length(all_clauses) < 1) next

  truth = evaluate_formula(result, u)
  raw_truth = evaluate_raw_clauses(all_clauses, u)
  if (!all(truth == raw_truth)) {
    cat(sprintf("\n=== FAIL at trial %d ===\n", trial))

    # Print the clauses
    cat("Individual clauses (cls):\n")
    for (i in seq_along(cls)) {
      cat(sprintf("  cls[[%d]]: ", i))
      print(cls[[i]])
    }
    cat("\nInner formula (f_cls -> inner_f):\n")
    cat("  Original f_cls:\n")
    for (i in seq_along(f_cls)) {
      cat(sprintf("    f_cls[[%d]]: ", i))
      print(f_cls[[i]])
    }
    cat("  Simplified inner_f:\n")
    print(inner_f)
    cat("  inner_f clauses via as.list:\n")
    for (cl in as.list(inner_f)) {
      cat("    "); print(cl)
    }

    cat("\nMixed input to CnfFormula:\n")
    for (i in seq_along(mixed)) {
      cat(sprintf("  mixed[[%d]] class=%s: ", i, class(mixed[[i]])))
      if (inherits(mixed[[i]], "CnfFormula")) {
        cat(format(mixed[[i]]), "\n")
      } else {
        cat(format(mixed[[i]]), "\n")
      }
    }

    cat("\nResult formula:\n")
    print(result)

    cat("\nall_clauses for raw evaluation:\n")
    for (i in seq_along(all_clauses)) {
      cat(sprintf("  all_clauses[[%d]]: ", i))
      print(all_clauses[[i]])
    }

    # Show assignments where truth differs
    varnames = ls(u)
    domains = lapply(varnames, function(v) get(v, u))
    names(domains) = varnames
    assignments = expand.grid(domains, stringsAsFactors = FALSE)
    diffs = which(truth != raw_truth)
    cat(sprintf("\nDifferences: %d assignments out of %d\n", length(diffs), nrow(assignments)))
    if (length(diffs) <= 10) {
      for (d in diffs) {
        cat(sprintf("  Row %d: %s | formula=%s raw=%s\n",
          d,
          paste(names(assignments), "=", assignments[d, ], collapse=", "),
          truth[d], raw_truth[d]))
      }
    }

    # Also try: just use & operator instead
    cat("\nAlternative: cls joined via &:\n")
    all_individual = c(cls, as.list(inner_f))
    all_individual = all_individual[!sapply(all_individual, function(x) {
      b = unclass(x)
      isTRUE(b) || isFALSE(b)
    })]
    if (length(all_individual) >= 2) {
      alt = tryCatch({
        result_alt = as.CnfFormula(all_individual[[1]])
        for (i in 2:length(all_individual)) {
          result_alt = result_alt & as.CnfFormula(all_individual[[i]])
        }
        result_alt
      }, error = function(e) e)
      if (inherits(alt, "error")) {
        cat(sprintf("  ERROR: %s\n", alt$message))
      } else {
        truth_alt = evaluate_formula(alt, u)
        cat(sprintf("  Alt formula matches raw? %s\n", all(truth_alt == raw_truth)))
        cat(sprintf("  Alt formula matches constructor? %s\n", all(truth_alt == truth)))
      }
    }

    if (trial >= 60) break  # stop after first few
  }
}
