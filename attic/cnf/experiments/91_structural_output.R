#!/usr/bin/env Rscript
# Test structural properties of simplifier output:
# - No empty ranges in output clauses
# - No tautological clauses (symbol covering full domain)
# - No duplicate symbols within a clause
# - Proper list structure
# - as.list roundtrip consistency
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

check_structure = function(formula, universe, label) {
  n_tests <<- n_tests + 1
  f_bare = unclass(formula)

  # TRUE/FALSE formulas
  if (is.logical(f_bare)) {
    if (!isTRUE(f_bare) && !isFALSE(f_bare)) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [%s]: logical formula is not TRUE or FALSE\n", label))
      return(FALSE)
    }
    return(TRUE)
  }

  if (!is.list(f_bare)) {
    n_failures <<- n_failures + 1
    cat(sprintf("FAIL [%s]: formula is not a list or logical\n", label))
    return(FALSE)
  }

  if (length(f_bare) == 0) {
    n_failures <<- n_failures + 1
    cat(sprintf("FAIL [%s]: formula is empty list (should be TRUE)\n", label))
    return(FALSE)
  }

  for (i in seq_along(f_bare)) {
    clause = f_bare[[i]]
    if (!is.list(clause)) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [%s]: clause %d is not a list\n", label, i))
      return(FALSE)
    }

    if (length(clause) == 0) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [%s]: clause %d is empty (should be FALSE)\n", label, i))
      return(FALSE)
    }

    cnames = names(clause)
    if (is.null(cnames)) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [%s]: clause %d has no names\n", label, i))
      return(FALSE)
    }

    if (length(cnames) != length(unique(cnames))) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [%s]: clause %d has duplicate symbol names\n", label, i))
      return(FALSE)
    }

    for (sym in cnames) {
      range = clause[[sym]]
      if (length(range) == 0) {
        n_failures <<- n_failures + 1
        cat(sprintf("FAIL [%s]: clause %d symbol '%s' has empty range\n", label, i, sym))
        return(FALSE)
      }

      if (length(range) != length(unique(range))) {
        n_failures <<- n_failures + 1
        cat(sprintf("FAIL [%s]: clause %d symbol '%s' has duplicate values\n", label, i, sym))
        return(FALSE)
      }

      # Check range is valid (subset of domain)
      sym_domain = tryCatch(get(sym, envir = universe), error = function(e) NULL)
      if (!is.null(sym_domain)) {
        if (!all(range %in% sym_domain)) {
          n_failures <<- n_failures + 1
          cat(sprintf("FAIL [%s]: clause %d symbol '%s' has values outside domain\n", label, i, sym))
          return(FALSE)
        }
        # Check not tautological (doesn't cover full domain)
        if (length(range) == length(sym_domain) && all(sym_domain %in% range)) {
          n_failures <<- n_failures + 1
          cat(sprintf("FAIL [%s]: clause %d symbol '%s' covers full domain (tautological)\n", label, i, sym))
          return(FALSE)
        }
      }
    }
  }
  TRUE
}

# === Random formulas with various configs ===
cat("=== Random formulas structural check ===\n")
set.seed(91001)

configs = list(
  list(n_vars = 3, dom_size = 3, n_cl_range = c(2, 5)),
  list(n_vars = 4, dom_size = 3, n_cl_range = c(3, 8)),
  list(n_vars = 3, dom_size = 5, n_cl_range = c(2, 6)),
  list(n_vars = 5, dom_size = 2, n_cl_range = c(3, 10)),
  list(n_vars = 2, dom_size = 8, n_cl_range = c(2, 5))
)

for (cfg_idx in seq_along(configs)) {
  cfg = configs[[cfg_idx]]
  for (trial in 1:300) {
    u = CnfUniverse()
    dom = paste0("v", 1:cfg$dom_size)
    syms = list()
    for (v in 1:cfg$n_vars) {
      vname = paste0("V", v)
      syms[[vname]] = CnfSymbol(u, vname, dom)
    }

    n_cl = sample(cfg$n_cl_range[1]:cfg$n_cl_range[2], 1)
    clauses = lapply(1:n_cl, function(j) {
      chosen = sample(names(syms), sample(1:min(3, cfg$n_vars), 1))
      atoms = lapply(chosen, function(s) {
        syms[[s]] %among% sample(dom, sample(1:(cfg$dom_size - 1), 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })

    f = tryCatch(CnfFormula(clauses), error = function(e) e)
    if (inherits(f, "error")) {
      n_tests = n_tests + 1; n_failures = n_failures + 1
      cat(sprintf("ERROR [struct-c%d-t%d]: %s\n", cfg_idx, trial, f$message)); next
    }
    check_structure(f, u, sprintf("struct-c%d-t%d", cfg_idx, trial))
  }
}
cat(sprintf("  Random structural: %d tests, %d failures\n", n_tests, n_failures))

# === as.list roundtrip ===
cat("\n=== as.list roundtrip ===\n")
set.seed(91002)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(1:4, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1

  # Roundtrip: CnfFormula -> as.list -> CnfFormula
  f_list = tryCatch(as.list(f), error = function(e) e)
  if (inherits(f_list, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [roundtrip-%d]: as.list failed: %s\n", trial, f_list$message)); next
  }

  if (length(f_list) > 0) {
    f2 = tryCatch(CnfFormula(f_list), error = function(e) e)
    if (inherits(f2, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [roundtrip-%d]: reconstruct failed: %s\n", trial, f2$message)); next
    }

    # Should be semantically equivalent
    t1 = evaluate_formula(f, u)
    t2 = evaluate_formula(f2, u)
    if (!all(t1 == t2)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [roundtrip-%d]: roundtrip changed semantics\n", trial))
    }
  }
}
cat(sprintf("  Roundtrip: %d tests, %d failures\n", n_tests, n_failures))

# === format/print don't crash ===
cat("\n=== format/print safety ===\n")

for (trial in 1:100) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, dom)
  }

  n_cl = sample(1:4, 1)
  clauses = lapply(1:n_cl, function(j) {
    chosen = sample(names(syms), sample(1:2, 1))
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  n_tests = n_tests + 1
  r = tryCatch({
    invisible(capture.output(print(f)))
    format(f)
  }, error = function(e) e)
  if (inherits(r, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [fmt-%d]: %s\n", trial, r$message))
  }
}

# TRUE/FALSE
n_tests = n_tests + 1
tryCatch({
  invisible(capture.output(print(as.CnfFormula(TRUE))))
  format(as.CnfFormula(TRUE))
  invisible(capture.output(print(as.CnfFormula(FALSE))))
  format(as.CnfFormula(FALSE))
}, error = function(e) {
  n_failures <<- n_failures + 1
  cat(sprintf("ERROR [fmt-tf]: %s\n", e$message))
})

cat(sprintf("  Format/print safety: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
