source("experiments/test_harness.R")

# Test roundtrip: CnfFormula -> as.list -> reconstruct -> compare
# Also test format/print methods

n_failures = 0
n_tests = 0

cat("=== Roundtrip: as.list -> reconstruct ===\n")

# Helper: verify roundtrip
check_roundtrip = function(formula, universe, label) {
  n_tests <<- n_tests + 1

  # Get clauses via as.list
  clause_list = tryCatch(as.list(as.CnfFormula(formula)), error = function(e) e)
  if (inherits(clause_list, "error")) {
    n_failures <<- n_failures + 1
    cat(sprintf("FAIL [%s]: as.list error: %s\n", label, clause_list$message))
    return(FALSE)
  }

  # For each clause, get atoms via as.list
  reconstructed_clauses = list()
  for (ci in seq_along(clause_list)) {
    clause = clause_list[[ci]]
    atom_list = tryCatch(as.list(clause), error = function(e) e)
    if (inherits(atom_list, "error")) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [%s]: as.list.CnfClause error at clause %d: %s\n", label, ci, atom_list$message))
      return(FALSE)
    }
    # Reconstruct clause from atoms
    if (length(atom_list) == 0) {
      reconstructed_clauses[[ci]] = as.CnfClause(FALSE)
    } else if (length(atom_list) == 1) {
      reconstructed_clauses[[ci]] = as.CnfClause(atom_list[[1]])
    } else {
      reconstructed_clauses[[ci]] = as.CnfClause(Reduce(`|`, atom_list))
    }
  }

  # Reconstruct formula
  reconstructed = tryCatch(CnfFormula(reconstructed_clauses), error = function(e) e)
  if (inherits(reconstructed, "error")) {
    n_failures <<- n_failures + 1
    cat(sprintf("FAIL [%s]: CnfFormula reconstruction error: %s\n", label, reconstructed$message))
    return(FALSE)
  }

  # Compare semantics
  truth_orig = evaluate_formula(formula, universe)
  truth_recon = evaluate_formula(reconstructed, universe)
  mismatches = which(truth_orig != truth_recon)
  if (length(mismatches)) {
    n_failures <<- n_failures + 1
    cat(sprintf("FAIL [%s]: roundtrip mismatch at row %d\n", label, mismatches[1]))
    return(FALSE)
  }
  TRUE
}

# Test with known formulas
u1 = CnfUniverse()
A = CnfSymbol(u1, "A", c("a1", "a2", "a3"))
B = CnfSymbol(u1, "B", c("b1", "b2", "b3"))
C = CnfSymbol(u1, "C", c("c1", "c2", "c3"))

# Simple formulas
check_roundtrip(CnfFormula(list()), u1, "empty formula (TRUE)")
check_roundtrip(A %among% "a1" & B %among% "b1", u1, "two units")
check_roundtrip((A %among% c("a1", "a2") | B %among% "b1") & (C %among% c("c1", "c2")), u1, "mixed clause+unit")

# Formula that simplifies significantly
check_roundtrip(
  (A %among% c("a1", "a2") | B %among% "b1") & (A %among% c("a2", "a3") | B %among% "b1"),
  u1, "SSE candidate"
)

# Random formulas
set.seed(123)
for (trial in 1:500) {
  u_r = CnfUniverse()
  syms = list(
    X = CnfSymbol(u_r, "X", c("x1", "x2", "x3")),
    Y = CnfSymbol(u_r, "Y", c("y1", "y2", "y3")),
    Z = CnfSymbol(u_r, "Z", c("z1", "z2"))
  )
  n_clauses = sample(2:6, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), min(n_atoms, length(syms)))
    atoms = lapply(chosen, function(s) {
      dom = u_r[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom) - 1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })
  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f)) next
  check_roundtrip(f, u_r, sprintf("random-%d", trial))
}
cat(sprintf("  Roundtrip done: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Format/print methods ===\n")

# Test that format/print don't crash on various inputs
test_format = function(obj, label) {
  n_tests <<- n_tests + 1
  result = tryCatch({
    fmt = format(obj)
    if (!is.character(fmt)) stop("format did not return character")
    capture.output(print(obj))
    TRUE
  }, error = function(e) {
    n_failures <<- n_failures + 1
    cat(sprintf("FAIL [format %s]: %s\n", label, e$message))
    FALSE
  })
}

u2 = CnfUniverse()
X2 = CnfSymbol(u2, "X", c("a", "b", "c"))
Y2 = CnfSymbol(u2, "Y", c("d", "e"))

# Atoms
test_format(X2 %among% "a", "atom normal")
test_format(X2 %among% c("a", "b", "c"), "atom TRUE")
test_format(X2 %among% character(0), "atom FALSE")
test_format(as.CnfAtom(TRUE), "atom bare TRUE")
test_format(as.CnfAtom(FALSE), "atom bare FALSE")

# Clauses
test_format(X2 %among% "a" | Y2 %among% "d", "clause normal")
test_format(as.CnfClause(TRUE), "clause TRUE")
test_format(as.CnfClause(FALSE), "clause FALSE")
test_format(as.CnfClause(X2 %among% "a"), "clause single atom")

# Formulas
test_format(CnfFormula(list()), "formula empty (TRUE)")
test_format(CnfFormula(list(as.CnfClause(X2 %among% "a"))), "formula single unit")
test_format((X2 %among% "a" | Y2 %among% "d") & (X2 %among% c("a", "b")), "formula mixed")

# CnfUniverse
test_format(u2, "universe")
test_format(CnfUniverse(), "empty universe")

# CnfSymbol
test_format(X2, "symbol")

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
