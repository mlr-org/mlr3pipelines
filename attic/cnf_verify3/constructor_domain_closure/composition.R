source("attic/cnf_verify3/constructor_domain_closure/harness.R")
set.seed(908216)
counts = c(random_cases = 0L, result_checks = 0L, valuation_rows = 0L,
  exact_normalized_comparisons = 0L, proper_formula_negations = 0L,
  contradiction_negations = 0L, direct_mixed_calls = 0L, native_mixed_calls = 0L,
  old_dispatch_exclusions = 0L, known_class_losses = 0L, gate_calls = 0L)
outputs = list()
gate_enabled = identical(Sys.getenv("CNF_CLOSURE_GATE"), "1")
if (gate_enabled) {
  # Secondary diagnostic only. The original production function is still
  # executed with the same inputs; the primary run leaves its binding alone.
  original_simplify = simplify_cnf
  simplify_cnf = function(entries, universe) {
    stopifnot((is.logical(entries) && length(entries) == 1L && !anyNA(entries)) ||
      (is.list(entries) && all(vapply(entries, proper_clause, logical(1L), universe = universe))))
    counts[["gate_calls"]] <<- counts[["gate_calls"]] + 1L
    original_simplify(entries, universe)
  }
}
check_result = function(object, expected, assignments) {
  stopifnot(canonical_object(object), identical(object_truth(object, assignments), expected))
  counts[["result_checks"]] <<- counts[["result_checks"]] + 1L
  counts[["valuation_rows"]] <<- counts[["valuation_rows"]] + nrow(assignments)
  invisible(object)
}
choose_shape = function(x) {
  variants = shapes(x)
  variants[[sample.int(length(variants), 1L)]]
}

for (case_id in seq_len(400L)) {
  domains = lapply(seq_len(sample.int(3L, 1L)), function(i) {
    sample(c("", "a", "b", "c"), sample.int(3L, 1L) + 1L)
  })
  # Explicit names avoid any vector recycling in the number of symbols.
  names(domains) = c("X", "Y", "Z")[seq_along(domains)]
  stored_domains = lapply(domains, function(d) {
    choose_shape(c(d, d[sample.int(length(d), sample.int(4L, 1L), replace = TRUE)]))
  })
  normalized_domains = lapply(stored_domains, flat_unique)
  assignments = expand.grid(normalized_domains, stringsAsFactors = FALSE)
  raw = lapply(seq_len(sample.int(5L, 1L)), function(ci) {
    symbols = names(domains)[sample.int(length(domains), sample.int(length(domains), 1L))]
    setNames(lapply(symbols, function(s) {
      d = domains[[s]]
      d[sample.int(length(d), sample.int(length(d) - 1L, 1L))]
    }), symbols)
  })
  values = lapply(raw, function(clause) lapply(clause, function(v) {
    choose_shape(c(v, v[sample.int(length(v), sample.int(4L, 1L), replace = TRUE)]))
  }))
  expected = rep(TRUE, nrow(assignments))
  for (clause in raw) expected = expected & raw_clause_truth(clause, assignments)

  build = function(domain_objects, selected_values) {
    u = CnfUniverse()
    symbols = lapply(names(domain_objects), function(s) CnfSymbol(u, s, domain_objects[[s]]))
    names(symbols) = names(domain_objects)
    atoms = lapply(seq_along(raw), function(i) lapply(names(raw[[i]]), function(s) {
      CnfAtom(symbols[[s]], selected_values[[i]][[s]])
    }))
    clauses = lapply(atoms, function(a) CnfClause(c(a, a[1L])))
    formula = CnfFormula(clauses)
    check_result(formula, expected, assignments)
    for (i in seq_along(clauses)) {
      cl_expected = raw_clause_truth(raw[[i]], assignments)
      check_result(clauses[[i]], cl_expected, assignments)
      check_result(CnfClause(as.list(clauses[[i]])), cl_expected, assignments)
      check_result(!clauses[[i]], !cl_expected, assignments)
      for (j in seq_along(atoms[[i]])) {
        atom = atoms[[i]][[j]]
        at_expected = literal_truth(names(raw[[i]])[[j]], raw[[i]][[j]], assignments)
        check_result(atom, at_expected, assignments)
        check_result(!atom, !at_expected, assignments)
        check_result(as.CnfFormula(!atom), !at_expected, assignments)
      }
    }
    negation = !formula
    check_result(negation, !expected, assignments)
    check_result(!negation, expected, assignments)
    stopifnot(identical(isTRUE(c(negation)), !any(expected)))
    counts[["proper_formula_negations"]] <<- counts[["proper_formula_negations"]] + !is.logical(formula)
    counts[["contradiction_negations"]] <<- counts[["contradiction_negations"]] + !any(expected)
    nested = CnfFormula(list(formula, clauses[[1L]]))
    check_result(nested, expected, assignments)
    first = as.CnfFormula(clauses[[1L]])
    check_result(`&.CnfFormula`(first, formula), expected, assignments)
    check_result(`|.CnfFormula`(first, formula), raw_clause_truth(raw[[1L]], assignments), assignments)
    list(formula = bare_payload(formula), negation = bare_payload(negation),
      clauses = lapply(clauses, bare_payload))
  }
  stored = build(stored_domains, values)
  normalized = build(normalized_domains, lapply(values, function(cl) lapply(cl, flat_unique)))
  stopifnot(identical(stored, normalized))
  counts[["exact_normalized_comparisons"]] = counts[["exact_normalized_comparisons"]] + 1L
  counts[["random_cases"]] = counts[["random_cases"]] + 1L
  outputs[[length(outputs) + 1L]] = stored
}

# Every ordered mixed pair for each storage shape. Direct invocation chooses
# the same CNF method as R >= 4.3's left-first chooseOpsMethod rules.
cnf_class = function(x) {
  hit = intersect(class(x), c("CnfAtom", "CnfClause", "CnfFormula"))
  if (length(hit)) hit[[1L]] else ""
}
for (shape_name in names(shapes(c("a", "b", "b", "c", "d", "a")))) {
  u = CnfUniverse()
  stored = shapes(c("a", "b", "b", "c", "d", "a"))[[shape_name]]
  x = CnfSymbol(u, matrix("X", 1L), stored)
  y = CnfSymbol(u, c(ignored = "Y"), stored)
  atom = CnfAtom(x, shapes(c("d", "a", "d", "a"))[[shape_name]])
  atom_y = CnfAtom(y, shapes(c("b", "c", "b", "c"))[[shape_name]])
  clause = atom | atom_y
  formula = atom & atom_y
  true_atom = CnfAtom(x, flat_unique(stored))
  false_atom = CnfAtom(x, character())
  pool = list(atom = atom, negated_atom = !atom, clause = clause, formula = formula,
    negated_clause = !clause, true = TRUE, false = FALSE,
    true_atom = true_atom, false_atom = false_atom,
    true_clause = as.CnfClause(true_atom), false_clause = as.CnfClause(false_atom),
    true_formula = as.CnfFormula(true_atom), false_formula = as.CnfFormula(false_atom),
    named_true = c(irrelevant = TRUE), matrix_false = matrix(FALSE, 1L))
  assignments = expand.grid(X = flat_unique(stored), Y = flat_unique(stored), stringsAsFactors = FALSE)
  truth = lapply(pool, object_truth, assignments = assignments)
  for (i in seq_along(pool)) for (j in seq_along(pool)) for (op in c("&", "|")) {
    left = pool[[i]]
    right = pool[[j]]
    class_left = cnf_class(left)
    class_right = cnf_class(right)
    selected = if (nzchar(class_left)) class_left else class_right
    expected = if (op == "&") truth[[i]] & truth[[j]] else truth[[i]] | truth[[j]]
    method = if (nzchar(selected)) get(paste0(op, ".", selected)) else get(op)
    direct = method(left, right)
    check_result(direct, expected, assignments)
    counts[["direct_mixed_calls"]] = counts[["direct_mixed_calls"]] + 1L
    expected_class = if (!nzchar(selected)) "logical" else if (op == "&" ||
        "CnfFormula" %in% c(class_left, class_right)) "CnfFormula" else "CnfClause"
    correct_class = if (expected_class == "logical") is.logical(direct) else inherits(direct, expected_class)
    if (!correct_class) {
      stopifnot(op == "|", selected == "CnfClause", !nzchar(class_left),
        isTRUE(left), !isTRUE(right), isTRUE(direct))
      counts[["known_class_losses"]] = counts[["known_class_losses"]] + 1L
    }
    old_conflict = getRversion() < "4.3.0" && nzchar(class_left) &&
      nzchar(class_right) && class_left != class_right
    if (old_conflict) {
      counts[["old_dispatch_exclusions"]] = counts[["old_dispatch_exclusions"]] + 1L
    } else {
      native = attempt(if (op == "&") left & right else left | right)
      stopifnot(!nzchar(native$error), !length(native$warnings),
        identical(class(native$value), class(direct)), identical(bare_payload(native$value), bare_payload(direct)))
      check_result(native$value, expected, assignments)
      counts[["native_mixed_calls"]] = counts[["native_mixed_calls"]] + 1L
    }
    outputs[[length(outputs) + 1L]] = list(class = class(direct), payload = bare_payload(direct))
  }
}
stem = if (gate_enabled) "composition_gate" else "composition"
saveRDS(outputs, file.path(closure_here, paste0(stem, "_", closure_version, ".rds")), version = 2L)
record_results(stem, counts, list(seed = 908216L, gate_enabled = gate_enabled))
