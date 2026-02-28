source("experiments/test_harness.R")

# Deep edge case testing for CnfClause [, [[, and as.list
# Including the discovered as.list.CnfClause TRUE bug
# and potential [.CnfClause FALSE[FALSE] inconsistency

errors = character(0)
report = function(test_name, ok, msg = "") {
  if (ok) {
    # cat(sprintf("  PASS: %s\n", test_name))
  } else {
    cat(sprintf("  FAIL: %s -- %s\n", test_name, msg))
    errors <<- c(errors, paste(test_name, msg, sep = ": "))
  }
}

u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("d", "e", "f"))
Z = CnfSymbol(u, "Z", c("g", "h", "i"))

cat("=== Bug #1 verification: as.list.CnfClause on TRUE ===\n")

# TRUE clause as.list should return a list, but crashes
clause_T = as.CnfClause(TRUE)
result_T = tryCatch(as.list(clause_T), error = function(e) e)
if (inherits(result_T, "error")) {
  cat(sprintf("  CONFIRMED BUG #1: as.list(TRUE clause) errors: %s\n", result_T$message))
} else {
  cat(sprintf("  Bug #1 appears fixed! Result type: %s, length: %d\n", class(result_T), length(result_T)))
}

# Also test lapply on TRUE clause (uses as.list internally)
result_lapply = tryCatch(lapply(clause_T, identity), error = function(e) e)
if (inherits(result_lapply, "error")) {
  cat(sprintf("  CONFIRMED: lapply on TRUE clause errors: %s\n", result_lapply$message))
}

cat("\n=== [.CnfClause: FALSE clause with logical index ===\n")

clause_F = as.CnfClause(FALSE)

# FALSE[integer(0)] -> should return FALSE (works)
result = tryCatch(clause_F[integer(0)], error = function(e) e)
report("FALSE[integer(0)] returns FALSE clause", !inherits(result, "error") && isFALSE(as.logical(result)))

# FALSE[0] -> should return FALSE (works)
result = tryCatch(clause_F[0], error = function(e) e)
report("FALSE[0] returns FALSE clause", !inherits(result, "error") && isFALSE(as.logical(result)))

# FALSE[FALSE] -> errors (potential inconsistency)
result = tryCatch(clause_F[FALSE], error = function(e) e)
if (inherits(result, "error")) {
  cat(sprintf("  NOTE: FALSE clause[FALSE] errors: %s\n", result$message))
  cat("  (This is a potential inconsistency: FALSE[integer(0)] works but FALSE[FALSE] doesn't)\n")
} else {
  report("FALSE[FALSE] returns FALSE clause", isFALSE(as.logical(result)))
}

# For comparison: normal clause[FALSE] works
clause_normal = X %among% c("a", "b") | Y %among% "d"
result = tryCatch(clause_normal[FALSE], error = function(e) e)
report("normal clause[FALSE] returns FALSE clause", !inherits(result, "error") && isFALSE(as.logical(result)))

# TRUE[FALSE] works
result = tryCatch(clause_T[FALSE], error = function(e) e)
if (inherits(result, "error")) {
  cat(sprintf("  NOTE: TRUE clause[FALSE] errors: %s\n", result$message))
} else {
  report("TRUE[FALSE] returns FALSE clause", isFALSE(as.logical(result)))
}

cat("\n=== [[.CnfClause behavior ===\n")

# Normal clause
clause_normal = X %among% c("a", "b") | Y %among% "d" | Z %among% c("g", "h")

# By name
report('clause[["X"]] gives values', identical(clause_normal[["X"]], c("a", "b")))
report('clause[["Y"]] gives values', identical(clause_normal[["Y"]], "d"))
report('clause[["Z"]] gives values', setequal(clause_normal[["Z"]], c("g", "h")))

# By index
report('clause[[1]] gives first symbol values', is.character(clause_normal[[1]]))
report('clause[[2]] gives second symbol values', is.character(clause_normal[[2]]))

# Nonexistent name
result = tryCatch(clause_normal[["NonExistent"]], error = function(e) e)
if (is.null(result)) {
  cat("  NOTE: clause[[\"NonExistent\"]] returns NULL silently (no error)\n")
} else if (inherits(result, "error")) {
  report("clause[[NonExistent]] errors", TRUE)
} else {
  report("clause[[NonExistent]] returns something", FALSE, sprintf("got %s", class(result)))
}

# TRUE clause [[ - this uses default [[ on a logical, which should error
result = tryCatch(clause_T[[1]], error = function(e) e)
if (inherits(result, "error")) {
  cat(sprintf("  NOTE: TRUE clause[[1]] errors: %s\n", result$message))
} else {
  cat(sprintf("  TRUE clause[[1]] returns: %s\n", result))
}

result = tryCatch(clause_T[["anything"]], error = function(e) e)
if (inherits(result, "error")) {
  cat(sprintf("  NOTE: TRUE clause[[\"anything\"]] errors: %s\n", result$message))
}

# FALSE clause [[ - same issue
result = tryCatch(clause_F[[1]], error = function(e) e)
if (inherits(result, "error")) {
  cat(sprintf("  NOTE: FALSE clause[[1]] errors: %s\n", result$message))
}

cat("\n=== length.CnfClause behavior ===\n")

# length on normal clause
report("length(normal clause) = 3", length(clause_normal) == 3)

# length on TRUE clause - TRUE is logical(1), so length should be 1
len_T = length(clause_T)
cat(sprintf("  length(TRUE clause) = %d (might be unexpected)\n", len_T))

# length on FALSE clause
len_F = length(clause_F)
cat(sprintf("  length(FALSE clause) = %d\n", len_F))

cat("\n=== names.CnfClause behavior ===\n")

# names on normal clause
report("names(normal clause)", identical(names(clause_normal), c("X", "Y", "Z")))

# names on TRUE clause - TRUE is logical(1), no names
names_T = names(clause_T)
cat(sprintf("  names(TRUE clause) = %s (expected NULL)\n", deparse(names_T)))

# names on FALSE clause
names_F = names(clause_F)
cat(sprintf("  names(FALSE clause) = %s (expected NULL)\n", deparse(names_F)))

cat("\n=== as.list.CnfFormula edge cases ===\n")

# Formula with FALSE
f_false = as.CnfFormula(FALSE)
result = tryCatch(as.list(f_false), error = function(e) e)
if (inherits(result, "error")) {
  cat(sprintf("  ERROR: as.list(FALSE formula): %s\n", result$message))
} else {
  cat(sprintf("  as.list(FALSE formula): length=%d", length(result)))
  if (length(result) > 0) {
    cat(sprintf(", element class=%s", class(result[[1]])))
    if (inherits(result[[1]], "CnfClause")) {
      cat(sprintf(", element value=%s", as.logical(result[[1]])))
    }
  }
  cat("\n")
}

# Formula with TRUE
f_true = as.CnfFormula(TRUE)
result = tryCatch(as.list(f_true), error = function(e) e)
if (inherits(result, "error")) {
  cat(sprintf("  ERROR: as.list(TRUE formula): %s\n", result$message))
} else {
  cat(sprintf("  as.list(TRUE formula): length=%d (expected 0)\n", length(result)))
}

cat("\n=== Summary ===\n")
if (length(errors)) {
  cat(sprintf("FAILURES (%d):\n", length(errors)))
  for (e in errors) cat(sprintf("  - %s\n", e))
} else {
  cat("All tests passed (see NOTEs above for documented edge cases).\n")
}
