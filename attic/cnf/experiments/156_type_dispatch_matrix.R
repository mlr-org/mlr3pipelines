#!/usr/bin/env Rscript
# Exhaustive testing of all type combinations for operators:
# CnfAtom, CnfClause, CnfFormula with TRUE/FALSE/normal
# x {&, |, !} with all type pairs
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

# Evaluate any Cnf object (atom, clause, or formula) against all assignments
eval_any = function(obj, universe) {
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  obj_bare = unclass(obj)
  if (isTRUE(obj_bare)) return(rep(TRUE, nrow(assignments)))
  if (isFALSE(obj_bare)) return(rep(FALSE, nrow(assignments)))

  if (inherits(obj, "CnfAtom")) {
    # Atom: symbol %in% values
    return(assignments[[obj$symbol]] %in% obj$values)
  }
  if (inherits(obj, "CnfClause")) {
    # Clause: OR of atoms
    result = rep(FALSE, nrow(assignments))
    for (sym in names(obj_bare)) {
      result = result | (assignments[[sym]] %in% obj_bare[[sym]])
    }
    return(result)
  }
  # Formula: AND of clauses
  result = rep(TRUE, nrow(assignments))
  for (clause in obj_bare) {
    clause_result = rep(FALSE, nrow(assignments))
    for (sym in names(clause)) {
      clause_result = clause_result | (assignments[[sym]] %in% clause[[sym]])
    }
    result = result & clause_result
  }
  result
}

u = CnfUniverse()
A = CnfSymbol(u, "A", c("a", "b", "c"))
B = CnfSymbol(u, "B", c("d", "e", "f"))

# Create all types of objects
make_objects = function() {
  list(
    atom_true = as.CnfAtom(TRUE),
    atom_false = as.CnfAtom(FALSE),
    atom_normal = A %among% c("a", "b"),
    atom_normal2 = B %among% "d",
    clause_true = as.CnfClause(TRUE),
    clause_false = as.CnfClause(FALSE),
    clause_unit = as.CnfClause(A %among% "a"),
    clause_normal = A %among% "a" | B %among% "d",
    formula_true = as.CnfFormula(TRUE),
    formula_false = as.CnfFormula(FALSE),
    formula_single = as.CnfFormula(A %among% c("a", "b")),
    formula_multi = (A %among% "a" | B %among% "d") & (A %among% c("a", "b"))
  )
}

objects = make_objects()

# === Test & operator for all pairs ===
cat("=== & operator all pairs ===\n")
for (name1 in names(objects)) {
  for (name2 in names(objects)) {
    o1 = objects[[name1]]
    o2 = objects[[name2]]
    n_tests = n_tests + 1
    result = tryCatch(o1 & o2, error = function(e) e)
    if (inherits(result, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [&-%s-%s]: %s\n", name1, name2, result$message))
      next
    }
    # Result should always be a CnfFormula
    if (!inherits(result, "CnfFormula")) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [&-%s-%s]: result not CnfFormula (class: %s)\n", name1, name2, class(result)[1]))
      next
    }
    # Verify semantic correctness
    t1 = eval_any(o1, u)
    t2 = eval_any(o2, u)
    t_result = eval_any(result, u)
    if (!all(t_result == (t1 & t2))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [&-%s-%s]: semantic mismatch\n", name1, name2))
    }
  }
}
cat(sprintf("  & pairs: %d tests, %d failures\n", n_tests, n_failures))

# === Test | operator for all pairs ===
cat("\n=== | operator all pairs ===\n")
for (name1 in names(objects)) {
  for (name2 in names(objects)) {
    o1 = objects[[name1]]
    o2 = objects[[name2]]
    n_tests = n_tests + 1
    result = tryCatch(o1 | o2, error = function(e) e)
    if (inherits(result, "error")) {
      n_failures = n_failures + 1
      cat(sprintf("ERROR [|-%s-%s]: %s\n", name1, name2, result$message))
      next
    }
    # Verify semantic correctness
    t1 = eval_any(o1, u)
    t2 = eval_any(o2, u)
    t_result = eval_any(result, u)
    if (!all(t_result == (t1 | t2))) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [|-%s-%s]: semantic mismatch\n", name1, name2))
    }
  }
}
cat(sprintf("  | pairs: %d tests, %d failures\n", n_tests, n_failures))

# === Test ! operator on all objects ===
cat("\n=== ! operator all objects ===\n")
for (name1 in names(objects)) {
  o1 = objects[[name1]]
  n_tests = n_tests + 1
  result = tryCatch(!o1, error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [!-%s]: %s\n", name1, result$message))
    next
  }
  t1 = eval_any(o1, u)
  t_result = eval_any(result, u)
  if (!all(t_result == !t1)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [!-%s]: semantic mismatch\n", name1))
  }
}
cat(sprintf("  ! objects: %d tests, %d failures\n", n_tests, n_failures))

# === Test complex expressions with mixed types ===
cat("\n=== Mixed type expressions ===\n")
set.seed(156001)

for (trial in 1:2000) {
  # Pick 2-3 random objects and combine with random operators
  objs = make_objects()
  n_ops = sample(1:3, 1)
  expr_objs = sample(names(objs), n_ops + 1, replace = TRUE)
  ops = sample(c("&", "|"), n_ops, replace = TRUE)

  # Build expression
  result = objs[[expr_objs[1]]]
  t_expected = eval_any(objs[[expr_objs[1]]], u)
  ok = TRUE
  for (i in seq_len(n_ops)) {
    next_obj = objs[[expr_objs[i + 1]]]
    t_next = eval_any(next_obj, u)
    if (ops[i] == "&") {
      result = tryCatch(result & next_obj, error = function(e) NULL)
      t_expected = t_expected & t_next
    } else {
      result = tryCatch(result | next_obj, error = function(e) NULL)
      t_expected = t_expected | t_next
    }
    if (is.null(result)) { ok = FALSE; break }
  }
  if (!ok) next

  # Maybe negate
  if (sample(c(TRUE, FALSE), 1)) {
    result = tryCatch(!result, error = function(e) NULL)
    if (is.null(result)) next
    t_expected = !t_expected
  }

  n_tests = n_tests + 1
  t_result = eval_any(result, u)
  if (!all(t_result == t_expected)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mixed-%d]: semantic mismatch (ops: %s, types: %s)\n",
        trial, paste(ops, collapse=","), paste(expr_objs, collapse=",")))
  }
}
cat(sprintf("  Mixed expressions: %d tests, %d failures\n", n_tests, n_failures))

# === Test chained operations with specific type transitions ===
cat("\n=== Type transition chains ===\n")
set.seed(156002)

for (trial in 1:500) {
  u2 = CnfUniverse()
  dom = c("a", "b", "c")
  X = CnfSymbol(u2, "X", dom)
  Y = CnfSymbol(u2, "Y", dom)

  # atom & clause
  a1 = X %among% sample(dom, sample(1:2, 1))
  c1 = X %among% sample(dom, sample(1:2, 1)) | Y %among% sample(dom, sample(1:2, 1))
  n_tests = n_tests + 1
  r = tryCatch(a1 & c1, error = function(e) e)
  if (inherits(r, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [chain-%d a&c]: %s\n", trial, r$message)); next
  }
  t_r = eval_any(r, u2)
  t_e = eval_any(a1, u2) & eval_any(c1, u2)
  if (!all(t_r == t_e)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chain-%d a&c]: semantic\n", trial))
  }

  # clause | atom
  n_tests = n_tests + 1
  r2 = tryCatch(c1 | a1, error = function(e) e)
  if (inherits(r2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [chain-%d c|a]: %s\n", trial, r2$message)); next
  }
  t_r2 = eval_any(r2, u2)
  t_e2 = eval_any(c1, u2) | eval_any(a1, u2)
  if (!all(t_r2 == t_e2)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chain-%d c|a]: semantic\n", trial))
  }

  # (atom & atom) | clause
  a2 = Y %among% sample(dom, sample(1:2, 1))
  n_tests = n_tests + 1
  r3 = tryCatch((a1 & a2) | c1, error = function(e) e)
  if (inherits(r3, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [chain-%d (a&a)|c]: %s\n", trial, r3$message)); next
  }
  t_r3 = eval_any(r3, u2)
  t_e3 = (eval_any(a1, u2) & eval_any(a2, u2)) | eval_any(c1, u2)
  if (!all(t_r3 == t_e3)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chain-%d (a&a)|c]: semantic\n", trial))
  }

  # !(clause | atom) & atom
  n_tests = n_tests + 1
  r4 = tryCatch(!(c1 | a1) & a2, error = function(e) e)
  if (inherits(r4, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [chain-%d !(c|a)&a]: %s\n", trial, r4$message)); next
  }
  t_r4 = eval_any(r4, u2)
  t_e4 = !(eval_any(c1, u2) | eval_any(a1, u2)) & eval_any(a2, u2)
  if (!all(t_r4 == t_e4)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [chain-%d !(c|a)&a]: semantic\n", trial))
  }
}
cat(sprintf("  Type transitions: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
