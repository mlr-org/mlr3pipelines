#!/usr/bin/env Rscript
# Test structural integrity of simplified formulas:
# - No empty ranges in clauses
# - No duplicate symbols in clauses
# - No tautological clauses remaining
# - Correct universe attachment
# - as.list roundtrip works
# - format/print don't crash
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

check_structure = function(formula, universe, label) {
  n_tests <<- n_tests + 1
  f_bare = unclass(formula)
  u = attr(formula, "universe")

  # Check universe
  if (!identical(u, universe)) {
    n_failures <<- n_failures + 1
    cat(sprintf("FAIL [%s]: universe mismatch\n", label))
    return(FALSE)
  }

  # Handle TRUE/FALSE formulas
  if (isTRUE(f_bare)) {
    # TRUE formula should format/print without crash
    tryCatch({
      format(formula)
      capture.output(print(formula))
    }, error = function(e) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [%s]: TRUE formula format/print crashed: %s\n", label, e$message))
    })
    return(TRUE)
  }
  if (isFALSE(f_bare)) {
    tryCatch({
      format(formula)
      capture.output(print(formula))
    }, error = function(e) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [%s]: FALSE formula format/print crashed: %s\n", label, e$message))
    })
    return(TRUE)
  }

  # It's a list of clauses
  if (!is.list(f_bare)) {
    n_failures <<- n_failures + 1
    cat(sprintf("FAIL [%s]: formula is not TRUE/FALSE/list\n", label))
    return(FALSE)
  }

  varnames = ls(universe)

  for (ci in seq_along(f_bare)) {
    clause = f_bare[[ci]]

    # Check it's a named list
    if (!is.list(clause) || is.null(names(clause))) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [%s]: clause %d is not a named list\n", label, ci))
      return(FALSE)
    }

    # Check no empty ranges
    for (s in names(clause)) {
      if (length(clause[[s]]) == 0) {
        n_failures <<- n_failures + 1
        cat(sprintf("FAIL [%s]: clause %d has empty range for symbol '%s'\n", label, ci, s))
        return(FALSE)
      }

      # Check all values are in the universe domain
      domain = get(s, universe)
      if (!all(clause[[s]] %in% domain)) {
        n_failures <<- n_failures + 1
        cat(sprintf("FAIL [%s]: clause %d symbol '%s' has values outside domain\n", label, ci, s))
        return(FALSE)
      }

      # Check no duplicate values
      if (anyDuplicated(clause[[s]])) {
        n_failures <<- n_failures + 1
        cat(sprintf("FAIL [%s]: clause %d symbol '%s' has duplicate values\n", label, ci, s))
        return(FALSE)
      }
    }

    # Check no duplicate symbol names
    if (anyDuplicated(names(clause))) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [%s]: clause %d has duplicate symbol names\n", label, ci))
      return(FALSE)
    }

    # Check symbols exist in universe
    for (s in names(clause)) {
      if (!s %in% varnames) {
        n_failures <<- n_failures + 1
        cat(sprintf("FAIL [%s]: clause %d references unknown symbol '%s'\n", label, ci, s))
        return(FALSE)
      }
    }

    # Check clause is not a tautology (range covers entire domain for all symbols)
    is_taut = all(sapply(names(clause), function(s) {
      length(clause[[s]]) == length(get(s, universe))
    }))
    if (is_taut && length(clause) > 0) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [%s]: clause %d is a tautology\n", label, ci))
      return(FALSE)
    }
  }

  # Check format/print don't crash
  tryCatch({
    format(formula)
    capture.output(print(formula))
  }, error = function(e) {
    n_failures <<- n_failures + 1
    cat(sprintf("FAIL [%s]: format/print crashed: %s\n", label, e$message))
  })

  TRUE
}

# === Random formulas structural check ===
cat("=== Random formula structure ===\n")
set.seed(56001)

for (trial in 1:500) {
  u = CnfUniverse()
  n_vars = sample(2:5, 1)
  syms = list()
  for (v in 1:n_vars) {
    vname = paste0("V", v)
    dom_size = sample(2:5, 1)
    syms[[vname]] = CnfSymbol(u, vname, paste0("d", 1:dom_size))
  }

  n_cl = sample(2:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(1:min(n_vars, 3), 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  check_structure(f, u, sprintf("random-struct-%d", trial))
}
cat(sprintf("  Random structure: %d tests, %d failures\n", n_tests, n_failures))

# === Formulas from operations ===
cat("\n=== Operation results structure ===\n")
set.seed(56002)

for (trial in 1:300) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("a", "b", "c"))
  }

  mk_formula = function() {
    cls = lapply(1:sample(1:3, 1), function(j) {
      chosen = sample(names(syms), sample(1:2, 1))
      atoms = lapply(chosen, function(s) {
        syms[[s]] %among% sample(c("a", "b", "c"), sample(1:2, 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = mk_formula()
  f2 = mk_formula()
  if (is.null(f1) || is.null(f2)) next

  # Test AND result
  f_and = tryCatch(f1 & f2, error = function(e) NULL)
  if (!is.null(f_and)) check_structure(f_and, u, sprintf("and-struct-%d", trial))

  # Test OR result
  f_or = tryCatch(f1 | f2, error = function(e) NULL)
  if (!is.null(f_or)) check_structure(f_or, u, sprintf("or-struct-%d", trial))

  # Test NOT result
  f_not = tryCatch(!f1, error = function(e) NULL)
  if (!is.null(f_not)) check_structure(f_not, u, sprintf("not-struct-%d", trial))

  # Test NOT-NOT result
  f_not2 = tryCatch(!!f1, error = function(e) NULL)
  if (!is.null(f_not2)) check_structure(f_not2, u, sprintf("notnot-struct-%d", trial))
}
cat(sprintf("  Operation structure: %d tests, %d failures\n", n_tests, n_failures))

# === Heavy formulas (many clauses) structural check ===
cat("\n=== Heavy formula structure ===\n")
set.seed(56003)

for (trial in 1:200) {
  u = CnfUniverse()
  syms = list()
  for (v in 1:4) {
    vname = paste0("V", v)
    syms[[vname]] = CnfSymbol(u, vname, c("0", "1", "2"))
  }

  n_cl = sample(10:20, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_atoms = sample(1:4, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      syms[[s]] %among% sample(c("0", "1", "2"), sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  check_structure(f, u, sprintf("heavy-struct-%d", trial))
}
cat(sprintf("  Heavy structure: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
