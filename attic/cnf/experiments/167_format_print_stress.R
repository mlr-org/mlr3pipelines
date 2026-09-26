#!/usr/bin/env Rscript
# Test format and print methods don't crash on various objects:
# - Normal, TRUE, FALSE atoms/clauses/formulas
# - Formulas with many clauses
# - Clauses with many symbols
# - Long value names
# Also test names(), length(), and other accessors
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

u = CnfUniverse()
A = CnfSymbol(u, "A", c("a", "b", "c"))
B = CnfSymbol(u, "B", c("d", "e", "f"))
C = CnfSymbol(u, "C", c("x", "y", "z"))

# === format on all types ===
cat("=== format/print safety ===\n")

objects_to_test = list(
  "TRUE atom" = as.CnfAtom(TRUE),
  "FALSE atom" = as.CnfAtom(FALSE),
  "normal atom" = A %among% c("a", "b"),
  "TRUE clause" = as.CnfClause(TRUE),
  "FALSE clause" = as.CnfClause(FALSE),
  "1-sym clause" = as.CnfClause(A %among% "a"),
  "2-sym clause" = A %among% "a" | B %among% "d",
  "3-sym clause" = A %among% "a" | B %among% "d" | C %among% "x",
  "TRUE formula" = as.CnfFormula(TRUE),
  "FALSE formula" = as.CnfFormula(FALSE),
  "1-clause formula" = as.CnfFormula(A %among% c("a", "b")),
  "multi-clause formula" = (A %among% "a" | B %among% "d") & (A %among% c("a", "b"))
)

for (nm in names(objects_to_test)) {
  obj = objects_to_test[[nm]]

  # format
  n_tests = n_tests + 1
  r = tryCatch(format(obj), error = function(e) e)
  if (inherits(r, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR format(%s): %s\n", nm, r$message))
  } else if (!is.character(r) || length(r) != 1) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL format(%s): not single string\n", nm))
  }

  # print
  n_tests = n_tests + 1
  r = tryCatch(capture.output(print(obj)), error = function(e) e)
  if (inherits(r, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR print(%s): %s\n", nm, r$message))
  }
}
cat(sprintf("  format/print: %d tests, %d failures\n", n_tests, n_failures))

# === names/length on various objects ===
cat("\n=== names/length ===\n")

# Normal clause
n_tests = n_tests + 1
cl = A %among% "a" | B %among% "d"
nms = names(cl)
if (!setequal(nms, c("A", "B"))) {
  n_failures = n_failures + 1
  cat("FAIL: names of 2-sym clause\n")
}
n_tests = n_tests + 1
if (length(cl) != 2) {
  n_failures = n_failures + 1
  cat("FAIL: length of 2-sym clause\n")
}

# TRUE clause
n_tests = n_tests + 1
cl_true = as.CnfClause(TRUE)
if (length(cl_true) != 1) {
  cat(sprintf("  Note: length(TRUE clause) = %d\n", length(cl_true)))
}

# FALSE clause
n_tests = n_tests + 1
cl_false = as.CnfClause(FALSE)
if (length(cl_false) != 1) {
  cat(sprintf("  Note: length(FALSE clause) = %d\n", length(cl_false)))
}

# Normal formula
n_tests = n_tests + 1
f = (A %among% "a" | B %among% "d") & (A %among% c("a", "b"))
if (length(f) < 1) {
  n_failures = n_failures + 1
  cat("FAIL: length of formula < 1\n")
}

cat(sprintf("  names/length: %d tests, %d failures\n", n_tests, n_failures))

# === Stress: many random objects ===
cat("\n=== Random format/print stress ===\n")
set.seed(167001)

for (trial in 1:500) {
  u2 = CnfUniverse()
  dom = c("a", "b", "c", "d")
  X = CnfSymbol(u2, "X", dom)
  Y = CnfSymbol(u2, "Y", dom)
  Z = CnfSymbol(u2, "Z", dom)
  syms = list(X = X, Y = Y, Z = Z)

  n_cl = sample(1:5, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) == 0) next

  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next

  # format
  n_tests = n_tests + 1
  tryCatch(format(f), error = function(e) {
    n_failures <<- n_failures + 1
    cat(sprintf("ERROR format[%d]: %s\n", trial, e$message))
  })

  # print
  n_tests = n_tests + 1
  tryCatch(capture.output(print(f)), error = function(e) {
    n_failures <<- n_failures + 1
    cat(sprintf("ERROR print[%d]: %s\n", trial, e$message))
  })

  # as.list roundtrip
  n_tests = n_tests + 1
  cl_list = tryCatch(as.list(f), error = function(e) NULL)
  if (!is.null(cl_list)) {
    for (cl in cl_list) {
      tryCatch({
        format(cl)
        capture.output(print(cl))
      }, error = function(e) {
        n_failures <<- n_failures + 1
        cat(sprintf("ERROR clause format/print[%d]: %s\n", trial, e$message))
      })
    }
  }
}
cat(sprintf("  Random stress: %d tests, %d failures\n", n_tests, n_failures))

# === Long value names ===
cat("\n=== Long value names ===\n")
set.seed(167002)

u3 = CnfUniverse()
long_dom = c("very_long_value_name_alpha", "very_long_value_name_beta", "very_long_value_name_gamma")
LongA = CnfSymbol(u3, "LongSymbolName", long_dom)
LongB = CnfSymbol(u3, "AnotherLongName", long_dom)

n_tests = n_tests + 1
cl = LongA %among% c("very_long_value_name_alpha", "very_long_value_name_beta") | LongB %among% "very_long_value_name_gamma"
r = tryCatch({
  format(cl)
  capture.output(print(cl))
  "ok"
}, error = function(e) e$message)
if (r != "ok") {
  n_failures = n_failures + 1
  cat(sprintf("ERROR long names: %s\n", r))
}

n_tests = n_tests + 1
f = tryCatch(CnfFormula(list(cl, as.CnfClause(LongA %among% "very_long_value_name_alpha"))), error = function(e) NULL)
if (!is.null(f)) {
  r2 = tryCatch({
    format(f)
    capture.output(print(f))
    "ok"
  }, error = function(e) e$message)
  if (r2 != "ok") {
    n_failures = n_failures + 1
    cat(sprintf("ERROR long names formula: %s\n", r2))
  }
}

cat(sprintf("  Long names: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
