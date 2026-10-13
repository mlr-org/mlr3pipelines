# Independent constructor-closure review. Run from the repository root.
# This script does not source the author's harness and does not replace any
# production binding. Exact output checks retain every inner clause attribute.
suppressPackageStartupMessages(library(checkmate))
suppressPackageStartupMessages(library(jsonlite))
stopf = function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)
map_chr = function(.x, .f, ...) vapply(.x, .f, character(1L), ...)
review_dir = "attic/cnf_verify3/constructor_closure_review"
runtime = if (getRversion() < "4") "r36" else "r46"
sources = file.path("R", paste0(c("CnfUniverse", "CnfSymbol", "CnfAtom",
  "CnfClause", "CnfFormula", "CnfFormula_simplify"), ".R"))
for (path in sources) source(path)
cat(R.version.string, "; checkmate ", as.character(packageVersion("checkmate")), "\n", sep = "")

counts = c(unique_shapes = 0L, atom_pairs = 0L, repeated_atom_payloads = 0L,
  dimensional_atom_payloads = 0L, exact_formula_comparisons = 0L,
  truth_objects = 0L, truth_rows = 0L, grammar_controls = 0L)
require_true = function(ok, label) {
  if (!isTRUE(ok)) stop(label, call. = FALSE)
}
# Both the reference support and truth evaluator use scalar comparisons in
# storage order, without unique(), match(), %in%, setdiff(), or CNF operators.
scalars = function(x) {
  ans = character(length(x))
  for (i in seq_along(x)) ans[[i]] = x[[i]]
  ans
}
support = function(x) {
  ans = character()
  for (v in scalars(x)) if (!any(v == ans)) ans = c(ans, v)
  ans
}
complement = function(domain, values) {
  ans = character()
  for (v in support(domain)) if (!any(v == scalars(values))) ans = c(ans, v)
  ans
}
payload = function(x) {
  attr(x, "class") = NULL
  attr(x, "universe") = NULL
  x
}
valid_clause = function(x, universe) {
  if (!is.list(x) || !length(x) || is.null(names(x)) || anyNA(names(x)) || anyDuplicated(names(x))) return(FALSE)
  if (!identical(attributes(x), list(names = names(x)))) return(FALSE)
  for (i in seq_along(x)) {
    s = names(x)[[i]]
    if (!exists(s, universe, inherits = FALSE)) return(FALSE)
    v = x[[i]]
    if (!is.character(v) || !is.null(attributes(v)) || anyNA(v) || !length(v)) return(FALSE)
    if (!identical(v, support(v))) return(FALSE)
    if (length(complement(v, universe[[s]])) || !length(complement(universe[[s]], v))) return(FALSE)
  }
  TRUE
}
valid_object = function(x) {
  if (is.logical(x)) return(length(x) == 1L && !anyNA(x))
  u = attr(x, "universe")
  if (inherits(x, "CnfAtom")) return(length(support(x$values)) > 0L &&
    !length(complement(x$values, u[[x$symbol]])) && length(complement(u[[x$symbol]], x$values)) > 0L)
  if (inherits(x, "CnfClause")) return(valid_clause(payload(x), u))
  if (!inherits(x, "CnfFormula") || !length(x)) return(FALSE)
  raw = payload(x)
  all(vapply(seq_along(raw), function(i) valid_clause(raw[[i]], u), logical(1L)))
}
literal_truth = function(s, values, valuation) any(valuation[[s]] == scalars(values))
clause_truth = function(cl, valuation) {
  if (is.logical(cl)) return(as.vector(cl))
  for (j in seq_along(cl)) if (literal_truth(names(cl)[[j]], cl[[j]], valuation)) return(TRUE)
  FALSE
}
object_truth = function(x, valuation) {
  if (is.logical(x)) return(as.vector(x))
  if (inherits(x, "CnfAtom")) return(literal_truth(x$symbol, x$values, valuation))
  if (inherits(x, "CnfClause")) return(clause_truth(payload(x), valuation))
  raw = payload(x)
  for (j in seq_along(raw)) if (!clause_truth(raw[[j]], valuation)) return(FALSE)
  TRUE
}
truth_table = function(x, assignments) {
  vapply(seq_len(nrow(assignments)), function(i) object_truth(x, as.list(assignments[i, , drop = FALSE])), logical(1L))
}
check_truth = function(x, expected, assignments, label) {
  require_true(valid_object(x), paste(label, "noncanonical result"))
  require_true(identical(truth_table(x, assignments), expected), paste(label, "incorrect truth"))
  counts[["truth_objects"]] <<- counts[["truth_objects"]] + 1L
  counts[["truth_rows"]] <<- counts[["truth_rows"]] + nrow(assignments)
  invisible(x)
}
variants = function(x) {
  n = length(x)
  weird_names = rep(c(NA_character_, "", "repeat", "repeat"), length.out = n)
  ans = list(plain = x, names = setNames(x, weird_names),
    metadata = structure(x, comment = "ordinary", review = list(1:3, NA_character_)),
    one_array = array(x, n), row_matrix = matrix(x, nrow = 1L), col_matrix = matrix(x, ncol = 1L))
  for (nr in c(2L, 3L, 4L)) if (n %% nr == 0L) {
    ans[[paste0("matrix_", nr)]] = matrix(x, nr)
    ans[[paste0("array_", nr)]] = array(x, c(nr, 1L, n %/% nr))
  }
  if (n %% 4L == 0L) ans$array_4d = array(x, c(2L, 2L, n %/% 4L, 1L))
  shaped = names(ans)[vapply(ans, function(v) !is.null(dim(v)), logical(1L))]
  for (label in shaped) {
    v = ans[[label]]
    dn = lapply(dim(v), function(d) rep(c("", NA_character_, "same"), length.out = d))
    names(dn) = rep(c(NA_character_, "", "axis", "axis"), length.out = length(dn))
    dimnames(v) = dn
    names(v) = weird_names
    ans[[paste0(label, "_named")]] = v
  }
  # I() on all ordinary shapes, not only plain vectors.
  for (label in names(ans)) ans[[paste0(label, "_AsIs")]] = I(ans[[label]])
  ans$inert_class = structure(x, class = "constructor_review_no_methods")
  ans
}

# Shapes longer and of higher rank than the author's exhaustive length <= 4
# grid. A forced duplicate first-axis slice supplements random ordinary data.
set.seed(612906)
sequences = list(c("b", "b", "a", "a", "a", "b"),
  c("d", "d", "c", "c", "b", "b", "a", "a", "", "", "d", "d"))
for (i in seq_len(120L)) {
  n = c(6L, 8L, 9L, 12L, 16L)[(i - 1L) %% 5L + 1L]
  sequences[[length(sequences) + 1L]] = c("", "a", "b", "c", "d")[sample.int(5L, n, replace = TRUE)]
}
for (i in seq_along(sequences)) {
  shapes = variants(sequences[[i]])
  for (label in names(shapes)) {
    x = shapes[[label]]
    require_true(identical(support(unique(x)), support(x)), paste("first occurrence", i, label))
    require_true(identical(unique(c(NULL, unique(x))), support(x)), paste("clause flattening", i, label))
    counts[["unique_shapes"]] = counts[["unique_shapes"]] + 1L
  }
}
cat("Scalar support/order and clause-boundary shapes:", counts[["unique_shapes"]], "\n")

domain_sequence = c("d", "", "b", "a", "c", "b", "d", "a", "c", "", "d", "b")
value_sequence = c("b", "b", "a", "a", "a", "b")
domain_shapes = variants(domain_sequence)
value_shapes = variants(value_sequence)
assignment = data.frame(X = support(domain_sequence), stringsAsFactors = FALSE)
expected_atom = vapply(assignment$X, function(v) any(v == value_sequence), logical(1L), USE.NAMES = FALSE)
for (dl in names(domain_shapes)) {
  u = CnfUniverse()
  # A missing scalar name is an attribute; the binding string is still X.
  sx = CnfSymbol(u, setNames(array("X", 1L), NA_character_), domain_shapes[[dl]])
  require_true(identical(u[["X"]], domain_shapes[[dl]]), paste("unchanged domain", dl))
  for (vl in names(value_shapes)) {
    label = paste(dl, vl)
    atom = CnfAtom(sx, value_shapes[[vl]])
    neg = !atom
    twice = !neg
    cl = as.CnfClause(atom)
    f = as.CnfFormula(cl)
    nf = !f
    wanted = complement(domain_sequence, value_sequence)
    require_true(identical(support(atom$values), support(value_sequence)), paste(label, "atom support"))
    require_true(identical(neg$values, wanted), paste(label, "atom complement"))
    require_true(identical(twice$values, complement(domain_sequence, wanted)), paste(label, "double atom complement"))
    require_true(identical(payload(cl), list(X = support(value_sequence))), paste(label, "clause payload"))
    require_true(identical(payload(nf), list(list(X = wanted))), paste(label, "direct formula complement"))
    check_truth(atom, expected_atom, assignment, label)
    check_truth(neg, !expected_atom, assignment, label)
    check_truth(cl, expected_atom, assignment, label)
    check_truth(nf, !expected_atom, assignment, label)
    counts[["atom_pairs"]] = counts[["atom_pairs"]] + 1L
    counts[["repeated_atom_payloads"]] = counts[["repeated_atom_payloads"]] + (length(scalars(atom$values)) > length(support(atom$values)))
    counts[["dimensional_atom_payloads"]] = counts[["dimensional_atom_payloads"]] + !is.null(dim(atom$values))
  }
}
cat("Independent domain/value shape pairs:", counts[["atom_pairs"]], "\n")

# Three unusual but ordinary binding names; no syntactic name rewriting.
symbol_names = c("a b", "...", "X$Y")
plain_domains = setNames(rep(list(support(domain_sequence)), 3L), symbol_names)
assignments = expand.grid(plain_domains, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
names(assignments) = symbol_names
saved = list()
for (case_id in seq_len(90L)) {
  raw = lapply(seq_len(2L + case_id %% 4L), function(ci) {
    chosen = sample.int(3L, 1L + (case_id + ci) %% 3L)
    setNames(lapply(chosen, function(si) {
      vals = support(domain_sequence)[sample.int(5L, 1L + (case_id + ci + si) %% 4L)]
      c(vals, rev(vals), vals)
    }), symbol_names[chosen])
  })
  expected = vapply(seq_len(nrow(assignments)), function(i) {
    valuation = as.list(assignments[i, , drop = FALSE])
    all(vapply(raw, clause_truth, logical(1L), valuation = valuation))
  }, logical(1L))
  build = function(normalized) {
    u = CnfUniverse()
    symbols = setNames(lapply(seq_along(symbol_names), function(i) {
      ds = domain_shapes[[(case_id + 11L * i - 1L) %% length(domain_shapes) + 1L]]
      handle = if (i == 1L) I(matrix(symbol_names[[i]], 1L)) else setNames(symbol_names[[i]], NA_character_)
      CnfSymbol(u, handle, if (normalized) support(ds) else ds)
    }), symbol_names)
    cls = lapply(seq_along(raw), function(ci) {
      atoms = lapply(seq_along(raw[[ci]]), function(j) {
        seq = raw[[ci]][[j]]
        shapes = variants(seq)
        val = shapes[[(case_id + ci + j - 1L) %% length(shapes) + 1L]]
        CnfAtom(symbols[[names(raw[[ci]])[[j]]]], if (normalized) support(val) else val)
      })
      # Exercises accumulation of duplicate atoms and canonical clauses.
      CnfClause(c(atoms, list(CnfClause(atoms[1L]))))
    })
    formula = CnfFormula(cls)
    negated = !formula
    twice = !negated
    check_truth(formula, expected, assignments, paste(case_id, "formula"))
    check_truth(negated, !expected, assignments, paste(case_id, "formula negation"))
    check_truth(twice, expected, assignments, paste(case_id, "double formula negation"))
    require_true(identical(isTRUE(as.vector(negated)), !any(expected)), paste(case_id, "negation completeness"))
    first_truth = vapply(seq_len(nrow(assignments)), function(i) clause_truth(raw[[1L]], as.list(assignments[i, , drop = FALSE])), logical(1L))
    joined = `|.CnfFormula`(formula, cls[[1L]])
    met = `&.CnfFormula`(formula, cls[[1L]])
    check_truth(joined, expected | first_truth, assignments, paste(case_id, "OR"))
    check_truth(met, expected & first_truth, assignments, paste(case_id, "AND"))
    for (ci in seq_along(cls)) {
      cl_expected = vapply(seq_len(nrow(assignments)), function(i) clause_truth(raw[[ci]], as.list(assignments[i, , drop = FALSE])), logical(1L))
      check_truth(!cls[[ci]], !cl_expected, assignments, paste(case_id, ci, "clause negation"))
      require_true(identical(payload(CnfClause(as.list(cls[[ci]]))), payload(cls[[ci]])), paste(case_id, ci, "proper clause round trip"))
    }
    nested = CnfFormula(list(formula, cls[[1L]], as.CnfClause(TRUE)))
    check_truth(nested, expected, assignments, paste(case_id, "safe nested constructor"))
    list(formula = payload(formula), negated = payload(negated), twice = payload(twice),
      joined = payload(joined), met = payload(met), nested = payload(nested),
      clauses = lapply(cls, payload))
  }
  stored = build(FALSE)
  normalized = build(TRUE)
  require_true(identical(stored, normalized), paste(case_id, "exact ordered payload comparison"))
  saved[[case_id]] = stored
  counts[["exact_formula_comparisons"]] = counts[["exact_formula_comparisons"]] + 1L
}
cat("Exact multi-symbol formula comparisons:", counts[["exact_formula_comparisons"]], "\n")

saveRDS(saved, file.path(review_dir, paste0("payloads_", runtime, ".rds")), version = 2L)
report = list(R = R.version.string, checkmate = as.character(packageVersion("checkmate")),
  seed = 612906L, source_md5 = as.list(tools::md5sum(sources)), counts = as.list(counts))
write_json(report, file.path(review_dir, paste0("results_", runtime, ".json")), pretty = TRUE, auto_unbox = TRUE)
cat(toJSON(report, pretty = TRUE, auto_unbox = TRUE), "\n")
