source("attic/cnf_verify3/r_values/bootstrap.R")
options(width = 200L)
cat("R:", R.version.string, "; checkmate:", as.character(packageVersion("checkmate")), "\n")

attempt = function(expr) {
  warnings = character()
  answer = withCallingHandlers(tryCatch(force(expr), error = function(e) e), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = answer, error = if (inherits(answer, "error")) conditionMessage(answer) else "",
    warnings = paste(warnings, collapse = " | "))
}
bare_text = function(x) {
  if (inherits(x, "CnfAtom") && !is.logical(x)) x = list(symbol = x$symbol, values = x$values)
  if (inherits(x, "CnfClause") || inherits(x, "CnfFormula") || is.logical(x)) x = c(x)
  paste(deparse(x, width.cutoff = 120L), collapse = " ")
}
rows = list()
record = function(section, input, status, details = "") {
  rows[[length(rows) + 1L]] <<- data.frame(section = section, input = input, status = status, details = details,
    stringsAsFactors = FALSE)
}

# Domain values are an abstract set: names and duplicates do not create new
# possible assignments, while missing values are excluded by the constructor.
domains = list(
  ordinary = c("a", "b", "c"), named = c(z = "a", z = "b", unused = "c"),
  missing_names = setNames(c("a", "b", "c"), c(NA, "", "")),
  repeated = c("a", "b", "a", "c", "b"), matrix = matrix(c("a", "b", "a", "c"), 2L),
  array = array(c("a", "b", "a", "c"), c(2L, 1L, 2L)),
  attributed = structure(c("a", "b", "c"), note = "metadata"),
  as_is = I(c("a", "b", "c")), missing = c("a", NA_character_),
  empty = character(), logical = TRUE, numeric = c(1, 2), factor = factor(c("a", "b")),
  list = list("a", "b"), null = NULL, empty_label = c("", "a", "b")
)
for (label in names(domains)) {
  domain = domains[[label]]
  universe = CnfUniverse()
  result = attempt(CnfSymbol(universe, "X", domain))
  if (nzchar(result$error)) {
    record("domain", label, "rejected", result$error)
    next
  }
  stopifnot(identical(universe[["X"]], domain))
  values = as.character(domain)[[1L]]
  atom = CnfAtom(result$value, values)
  clause = as.CnfClause(atom)
  formula = as.CnfFormula(atom)
  stopifnot(valid_clause(clause, universe), valid_formula(formula, universe))
  record("domain", label, "accepted; clause canonical", paste("stored:", paste(deparse(domain), collapse = " ")))
}

symbol_names = list(ordinary = "X", named = c(alias = "X"), missing_name_attribute = setNames("X", NA_character_),
  matrix = matrix("X", 1L), as_is = I("X"), blank = " ", empty = "", missing = NA_character_,
  multiple = c("X", "Y"), factor = factor("X"), numeric = 1)
for (label in names(symbol_names)) {
  universe = CnfUniverse()
  result = attempt(CnfSymbol(universe, symbol_names[[label]], c("a", "b")))
  if (nzchar(result$error)) {
    record("symbol name", label, "rejected", result$error)
    next
  }
  clause = as.CnfClause(CnfAtom(result$value, "a"))
  stopifnot(valid_clause(clause, universe))
  record("symbol name", label, "accepted; clause canonical", paste("clause names:", paste(names(clause), collapse = ",")))
}

universe = CnfUniverse()
x = CnfSymbol(universe, "X", c("a", "b", "c"))
y = CnfSymbol(universe, "Y", c("a", "b", "c"))
values = list(
  ordinary = c("a", "b"), named = c(foo = "a", bar = "b"),
  missing_names = setNames(c("a", "b"), c(NA_character_, "")),
  repeated = c("a", "a", "b"), matrix_duplicates = matrix(c("a", "a", "b", "a"), 2L),
  array_duplicates = array(c("a", "a", "b", "a"), c(2L, 1L, 2L)),
  attributed = structure(c("a", "b"), note = "metadata"), as_is = I(c("a", "b")),
  missing = c("a", NA_character_), outside = "d", factor = factor("a"),
  numeric = 1, logical = TRUE, raw = as.raw(1L), complex = 1 + 0i, list = list("a"),
  empty_character = character(), empty_numeric = numeric(), empty_logical = logical(),
  empty_raw = raw(), empty_complex = complex(), empty_list = list(), null = NULL,
  full_repeated = c("a", "b", "c", "a")
)
for (label in names(values)) {
  result = attempt(CnfAtom(x, values[[label]]))
  if (nzchar(result$error)) {
    record("atom values", label, "rejected", result$error)
    next
  }
  atom = result$value
  clause = as.CnfClause(atom)
  formula = as.CnfFormula(atom)
  stopifnot(valid_clause(clause, universe), valid_formula(formula, universe))
  assignments = data.frame(X = universe[["X"]], stringsAsFactors = FALSE)
  expected = assignments$X %in% values[[label]]
  stopifnot(identical(expected, clause_truth(clause, assignments)),
    identical(expected, formula_truth(formula, assignments)))
  record("atom values", label, "accepted; clause canonical", bare_text(atom))
}

cl = CnfClause(list(CnfAtom(x, "a"), CnfAtom(y, "b")))
named_atoms = list(irrelevant = CnfAtom(x, "a"), irrelevant = CnfAtom(y, "b"))
stopifnot(identical(CnfClause(named_atoms), cl))
record("list names", "duplicate atom-list names", "ignored", "Symbol identities come from each atom, not the outer list names.")
named_clauses = list(irrelevant = cl, irrelevant = as.CnfClause(CnfAtom(x, c("a", "c"))))
stopifnot(identical(CnfFormula(named_clauses), CnfFormula(unname(named_clauses))))
record("list names", "duplicate formula-list names", "ignored", "Clause order/contents are the same as for the unnamed list.")

selectors = list(
  null = NULL, empty_integer = integer(), empty_character = character(), empty_raw = raw(), empty_complex = complex(),
  zero = 0, zero_named = c(n = 0), fractional_zero = 0.99, numeric = c(2, 1),
  numeric_duplicates = c(2, 1, 2), numeric_fractional = c(1.9, 1.1, 2.9),
  character = c("Y", "X"), character_duplicates = c("Y", "X", "Y"),
  named_character = c(X = "Y"), named_numeric = c(Y = 1),
  negative = -1, numeric_missing = c(1, NA_real_), character_missing = c("X", NA_character_),
  infinite = Inf, not_a_number = NaN, out_of_bounds = 3, unknown_symbol = "Z",
  false = FALSE, named_false = c(n = FALSE), logical = c(TRUE, FALSE), named_logical = c(Y = TRUE, X = FALSE),
  recycled_true = TRUE, logical_missing = c(FALSE, NA), logical_missing_only = NA,
  logical_too_long = c(TRUE, FALSE, TRUE), logical_matrix = matrix(c(TRUE, FALSE), 1L),
  logical_matrix_missing = matrix(c(NA, FALSE), 1L),
  numeric_row_duplicates = matrix(c(2, 1, 2), 1L),
  numeric_column_duplicates = matrix(c(2, 1, 2), ncol = 1L),
  numeric_matrix_duplicates = matrix(c(1, 2, 2, 1), 2L),
  numeric_array_duplicates = array(c(2, 1, 2), c(1L, 1L, 3L)),
  character_row_duplicates = matrix(c("Y", "X", "Y"), 1L),
  character_column_duplicates = matrix(c("Y", "X", "Y"), ncol = 1L),
  character_array_duplicates = array(c("Y", "X", "Y"), c(1L, 1L, 3L)),
  raw = as.raw(1L), complex = 1 + 0i, factor = factor(c("Y", "X")),
  as_is_character = I(c("X", "Y", "X")), date = as.Date(c(1, 2), origin = "1970-01-01"),
  list = list(1L), expression = expression(1L)
)
selector_values = list()
for (label in names(selectors)) {
  result = attempt(cl[selectors[[label]]])
  if (nzchar(result$error)) {
    record("clause selector", label, "rejected", result$error)
    next
  }
  selector_values[[label]] = result$value
  status = if (valid_clause(result$value, universe)) "canonical" else "MALFORMED"
  record("clause selector", label, status, bare_text(result$value))
}
stopifnot(identical(cl[], cl), identical(cl[NULL], cl[0]),
  identical(names(selector_values$numeric_duplicates), c("Y", "X")),
  identical(names(selector_values$numeric_row_duplicates), c("Y", "X", "Y")),
  identical(names(selector_values$character_row_duplicates), c("Y", "X", "Y")),
  anyNA(names(selector_values$logical_missing)), is.null(selector_values$logical_missing[[1L]]))

# Dimension-free selector enumeration: all short positive/zero vector indices
# normalize duplicate occurrences. Logical NA is the only admitted invalid
# representation among the generated vector types.
counts = c(vector_accepted = 0L, vector_malformed = 0L, matrix_accepted = 0L, matrix_malformed = 0L)
for (type in c("numeric", "character", "logical")) {
  pool = switch(type, numeric = c(0, 1, 2, NA_real_), character = c("X", "Y", NA_character_), logical = c(FALSE, TRUE, NA))
  for (n in 1:4) {
    enumerated = expand.grid(rep(list(seq_along(pool)), n), KEEP.OUT.ATTRS = FALSE)
    for (row in seq_len(nrow(enumerated))) {
      index = pool[as.integer(enumerated[row, ])]
      for (shape in c("vector", "matrix")) {
        selected = if (shape == "matrix") matrix(index, nrow = 1L) else index
        result = attempt(cl[selected])
        if (nzchar(result$error)) next
        counts[[paste0(shape, "_accepted")]] = counts[[paste0(shape, "_accepted")]] + 1L
        if (!valid_clause(result$value, universe)) {
          counts[[paste0(shape, "_malformed")]] = counts[[paste0(shape, "_malformed")]] + 1L
          stopifnot((type == "logical" && anyNA(index)) || shape == "matrix")
        } else {
          expected_index = if (type == "numeric") floor(index) else index
          selected_input = unclass(cl)[expected_index]
          assignments = expand.grid(X = universe[["X"]], Y = universe[["Y"]], stringsAsFactors = FALSE)
          stopifnot(identical(clause_truth(result$value, assignments), clause_truth(selected_input, assignments)))
        }
      }
    }
  }
}
record("exhaustive selectors", "vectors/matrices of length 1..4", "checked", paste(names(counts), counts, collapse = "; "))

# Scalar logical conversion accepts ordinary names/dimensions. Longer or
# missing flags are rejected. Attributes may remain; Boolean values do not
# change, and constructors normalize their payloads when combining objects.
flags = list(true = TRUE, false = FALSE, named_true = c(n = TRUE), named_false = c(n = FALSE),
  matrix_true = matrix(TRUE, 1L), matrix_false = matrix(FALSE, 1L),
  attributed_true = structure(TRUE, note = "metadata"), as_is_false = I(FALSE),
  missing = NA, empty = logical(), longer = c(TRUE, FALSE))
for (class in c("CnfAtom", "CnfClause", "CnfFormula")) {
  convert = get(paste0("as.", class))
  for (label in names(flags)) {
    result = attempt(convert(flags[[label]]))
    if (nzchar(result$error)) {
      record("logical conversion", paste(class, label), "rejected", result$error)
      next
    }
    stopifnot(identical(as.vector(result$value), as.vector(flags[[label]])))
    record("logical conversion", paste(class, label), "accepted", bare_text(result$value))
  }
}
for (constant in c(TRUE, FALSE)) {
  constant_clause = as.CnfClause(CnfAtom(x, if (constant) universe[["X"]] else character()))
  for (label in c("zero", "null", "false", "named_false", "logical_missing_only", "numeric_row_duplicates")) {
    result = attempt(constant_clause[selectors[[label]]])
    record("constant selector", paste(constant, label), if (nzchar(result$error)) "rejected" else "accepted",
      if (nzchar(result$error)) result$error else bare_text(result$value))
  }
}
for (label in c("unnamed", "named", "matrix")) {
  scalar = switch(label, unnamed = TRUE, named = c(n = TRUE), matrix = matrix(TRUE, 1L))
  result = TRUE | cl
  if (label != "unnamed") result = scalar | cl
  stopifnot(isTRUE(result), identical(class(result), class(scalar)))
  record("logical operator", paste(label, "TRUE | clause"), "class lost", paste(class(result), collapse = ","))
}

# Known constructor boundaries: type wrappers and universe inference are
# separate from Boolean identities in the operator methods.
for (label in c("raw FALSE atom entry", "wrapped FALSE then proper atom", "proper atom then wrapped FALSE")) {
  input = switch(label, "raw FALSE atom entry" = list(FALSE, CnfAtom(x, "a")),
    "wrapped FALSE then proper atom" = list(as.CnfAtom(FALSE), CnfAtom(x, "a")),
    "proper atom then wrapped FALSE" = list(CnfAtom(x, "a"), as.CnfAtom(FALSE)))
  result = attempt(CnfClause(input))
  stopifnot(nzchar(result$error))
  record("constant constructor", label, "rejected", result$error)
}
for (label in c("raw TRUE clause entry", "wrapped TRUE then proper clause", "proper clause then wrapped TRUE")) {
  input = switch(label, "raw TRUE clause entry" = list(TRUE, cl),
    "wrapped TRUE then proper clause" = list(as.CnfClause(TRUE), cl),
    "proper clause then wrapped TRUE" = list(cl, as.CnfClause(TRUE)))
  result = attempt(CnfFormula(input))
  record("constant constructor", label, if (nzchar(result$error)) "rejected" else "accepted",
    if (nzchar(result$error)) result$error else bare_text(result$value))
}

bad = cl[c(FALSE, NA)]
bad_formula = CnfFormula(list(bad))
stopifnot(!valid_formula(bad_formula, universe), isTRUE(as.logical(CnfClause(as.list(bad)))))
record("missing selector consequence", "logical NA", "malformed formula accepted", "Round trip CnfClause(as.list(bad)) becomes TRUE; no intended Boolean meaning assigned to NA symbol.")

out = do.call(rbind, rows)
write.table(out, file.path("attic/cnf_verify3/r_values", paste0("normalization_r", getRversion(), ".tsv")),
  sep = "\t", quote = TRUE, row.names = FALSE)
saveRDS(list(rows = out, selectors = counts),
  file.path("attic/cnf_verify3/r_values", paste0("normalization_r", getRversion(), ".rds")))
print(out[, c("section", "input", "status")], row.names = FALSE)
cat("All constructor, selector, and constant checks passed.\n")
