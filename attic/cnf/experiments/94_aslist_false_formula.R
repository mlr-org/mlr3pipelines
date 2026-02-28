#!/usr/bin/env Rscript
# Test as.list.CnfFormula on FALSE formulas.
# The function has: if (isTRUE(x_bare)) return(list())
# But NO check for isFALSE. What happens?
# Also test as.logical on various formulas.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

cat("=== as.list on FALSE formula (no universe) ===\n")

# FALSE formula from as.CnfFormula(FALSE)
f_false_bare = as.CnfFormula(FALSE)

n_tests = n_tests + 1
result = tryCatch(as.list(f_false_bare), error = function(e) e)
if (inherits(result, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: as.list(as.CnfFormula(FALSE)) crashed: %s\n", result$message))
} else {
  cat(sprintf("  as.list(as.CnfFormula(FALSE)) = list of length %d\n", length(result)))
  if (length(result) > 0) {
    cat(sprintf("  First element class: %s, value: %s\n",
      paste(class(result[[1]]), collapse = ","),
      paste(unclass(result[[1]]), collapse = ",")))
  }
}

cat("\n=== as.list on FALSE formula (with universe) ===\n")

# FALSE formula from contradiction
u = CnfUniverse()
A = CnfSymbol(u, "A", c("a", "b"))
f_contr = tryCatch(CnfFormula(list(
  as.CnfClause(A %among% "a"),
  as.CnfClause(A %among% "b")
)), error = function(e) NULL)

if (!is.null(f_contr) && isFALSE(as.logical(f_contr))) {
  n_tests = n_tests + 1
  result = tryCatch(as.list(f_contr), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR: as.list on contradiction formula crashed: %s\n", result$message))
  } else {
    cat(sprintf("  as.list(contradiction) = list of length %d\n", length(result)))
    if (length(result) > 0) {
      for (i in seq_along(result)) {
        cat(sprintf("  Element %d class: %s\n", i, paste(class(result[[i]]), collapse = ",")))
      }
    }
  }
} else {
  cat("  Skipped: formula was not FALSE\n")
}

cat("\n=== as.list roundtrip for FALSE formulas ===\n")

# Can we reconstruct a FALSE formula from as.list?
if (!is.null(f_contr) && isFALSE(as.logical(f_contr))) {
  n_tests = n_tests + 1
  clauses = tryCatch(as.list(f_contr), error = function(e) e)
  if (!inherits(clauses, "error") && length(clauses) > 0) {
    f2 = tryCatch(CnfFormula(clauses), error = function(e) e)
    if (inherits(f2, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR: reconstruct from as.list crashed: %s\n", f2$message))
    } else {
      if (!isFALSE(as.logical(f2))) {
        n_failures = n_failures + 1
        cat("FAIL: reconstructed formula is not FALSE\n")
      } else {
        cat("  Roundtrip OK: FALSE -> as.list -> CnfFormula -> FALSE\n")
      }
    }
  }
}

cat("\n=== as.logical on various formulas ===\n")

# TRUE formula
n_tests = n_tests + 1
if (!isTRUE(as.logical(as.CnfFormula(TRUE)))) {
  n_failures = n_failures + 1
  cat("FAIL: as.logical(TRUE formula) != TRUE\n")
}

# FALSE formula
n_tests = n_tests + 1
if (!isFALSE(as.logical(as.CnfFormula(FALSE)))) {
  n_failures = n_failures + 1
  cat("FAIL: as.logical(FALSE formula) != FALSE\n")
}

# Normal formula -> should be NA
u2 = CnfUniverse()
A2 = CnfSymbol(u2, "A", c("a", "b"))
f_normal = tryCatch(CnfFormula(list(as.CnfClause(A2 %among% "a"))), error = function(e) NULL)
if (!is.null(f_normal)) {
  n_tests = n_tests + 1
  if (!is.na(as.logical(f_normal))) {
    n_failures = n_failures + 1
    cat("FAIL: as.logical(normal formula) should be NA\n")
  }
}

cat("\n=== as.list on TRUE formula ===\n")
n_tests = n_tests + 1
result = tryCatch(as.list(as.CnfFormula(TRUE)), error = function(e) e)
if (inherits(result, "error")) {
  n_failures = n_failures + 1
  cat(sprintf("ERROR: as.list(TRUE formula) crashed: %s\n", result$message))
} else if (length(result) != 0) {
  n_failures = n_failures + 1
  cat(sprintf("FAIL: as.list(TRUE formula) should be empty list, got length %d\n", length(result)))
}

cat("\n=== as.list on normal formula ===\n")
set.seed(94001)

for (trial in 1:200) {
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
  f_list = tryCatch(as.list(f), error = function(e) e)
  if (inherits(f_list, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [aslist-%d]: %s\n", trial, f_list$message)); next
  }

  # Check each element is a proper CnfClause
  for (i in seq_along(f_list)) {
    if (!inherits(f_list[[i]], "CnfClause")) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [aslist-%d]: element %d not CnfClause\n", trial, i))
      break
    }
  }
}
cat(sprintf("  Normal as.list: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
