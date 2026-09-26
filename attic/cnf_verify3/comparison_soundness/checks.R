# Small independent controls for one-sided soundness of default comparison.
suppressPackageStartupMessages(library(checkmate))
suppressPackageStartupMessages(library(jsonlite))
stopf = function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)
map_chr = function(.x, .f, ...) vapply(.x, .f, character(1L), ...)
files = file.path("R", paste0(c("CnfUniverse", "CnfSymbol", "CnfAtom",
  "CnfClause", "CnfFormula", "CnfFormula_simplify"), ".R"))
for (file in files) source(file)
here = "attic/cnf_verify3/comparison_soundness"
runtime = if (getRversion() < "4") "r36" else "r46"
cat(R.version.string, "\n")

same_set = function(x, y) {
  contained = function(a, b) all(vapply(seq_along(a), function(i)
    any(vapply(seq_along(b), function(j) identical(a[[i]], b[[j]]), logical(1L))), logical(1L)))
  contained(x, y) && contained(y, x)
}
same_clause = function(x, y) {
  if (length(x) != length(y)) return(FALSE)
  for (i in seq_along(x)) {
    j = which(vapply(names(y), function(s) identical(names(x)[[i]], s), logical(1L)))
    if (length(j) != 1L || !same_set(x[[i]], y[[j]])) return(FALSE)
  }
  TRUE
}
same_multiset = function(x, y) {
  if (length(x) != length(y)) return(FALSE)
  used = rep(FALSE, length(y))
  for (i in seq_along(x)) {
    candidates = which(!used & vapply(seq_along(y), function(j) same_clause(x[[i]], y[[j]]), logical(1L)))
    if (!length(candidates)) return(FALSE)
    used[[candidates[[1L]]]] = TRUE
  }
  TRUE
}
content = function(x, y) {
  if (is.logical(x) || is.logical(y)) return(is.logical(x) && is.logical(y) &&
    identical(as.vector(x), as.vector(y)))
  if (!identical(class(x), class(y))) return(FALSE)
  if (inherits(x, "CnfAtom")) return(identical(x$symbol[[1L]], y$symbol[[1L]]) && same_set(x$values, y$values))
  if (inherits(x, "CnfClause")) return(same_clause(unclass(x), unclass(y)))
  same_multiset(unclass(x), unclass(y))
}
records = list()
record = function(label, left, right, expected_content, expected_result) {
  exact = content(left, right)
  answer = all.equal(left, right)
  stopifnot(identical(exact, expected_content), identical(isTRUE(answer), expected_result),
    !isTRUE(answer) || exact)
  records[[length(records) + 1L]] <<- list(label = label, same_content = exact,
    result_true = isTRUE(answer), diagnostic = if (isTRUE(answer)) "" else answer)
  cat(label, ": content=", exact, ", comparison=", isTRUE(answer), "\n", sep = "")
}

u = CnfUniverse()
x = CnfSymbol(u, "X", c("a", "b", "c"))
y = CnfSymbol(u, "Y", c("a", "b", "c"))
xa = CnfAtom(x, "a")
ya = CnfAtom(y, "a")
xb = CnfAtom(x, "b")
yb = CnfAtom(y, "b")
xab = CnfAtom(x, c("a", "b"))
record("atom value permutation", xab, CnfAtom(x, c("b", "a")), TRUE, TRUE)
record("atom changed value", xab, CnfAtom(x, c("a", "c")), FALSE, FALSE)
record("atom changed symbol", xa, ya, FALSE, FALSE)
record("atom repeated scalar storage", xa, CnfAtom(x, matrix(c("a", "a"), 1L)), TRUE, FALSE)
left = CnfClause(list(xa, yb))
reversed = CnfClause(list(yb, xa))
swapped = CnfClause(list(xb, ya))
record("clause symbol permutation", left, reversed, TRUE, TRUE)
record("clause changed association", left, swapped, FALSE, FALSE)
record("clause changed symbol only", as.CnfClause(xa), as.CnfClause(ya), FALSE, FALSE)
f1 = CnfFormula(list(CnfClause(list(xa, ya)), CnfClause(list(xb, yb))))
f2 = CnfFormula(list(CnfClause(list(yb, xb)), CnfClause(list(ya, xa))))
different = CnfFormula(list(left, swapped))
record("formula clause and symbol permutation", f1, f2, TRUE, TRUE)
record("formula changed associations", f1, different, FALSE, FALSE)

# Proper unsimplified representation fixtures: constructors would remove the
# repeated conjuncts before the comparison could test their multiplicities.
formula_fixture = function(clauses) structure(clauses, class = "CnfFormula", universe = u)
a = c(as.CnfClause(xa))
b = c(as.CnfClause(yb))
record("formula duplicate count", formula_fixture(list(a)), formula_fixture(list(a, a)), FALSE, FALSE)
record("formula distinct clause multiplicities", formula_fixture(list(a, a, b)),
  formula_fixture(list(a, b, b)), FALSE, FALSE)
record("formula equal repeated multiset", formula_fixture(list(a, a, b)),
  formula_fixture(list(b, a, a)), TRUE, TRUE)
# Both multiplicity controls nevertheless have equal truth on all assignments.
grid = expand.grid(X = c("a", "b", "c"), Y = c("a", "b", "c"), stringsAsFactors = FALSE)
truth_a = grid$X == "a"
truth_b = grid$Y == "b"
stopifnot(identical(truth_a, truth_a & truth_a),
  identical(truth_a & truth_a & truth_b, truth_a & truth_b & truth_b))

record("equal constants across classes", as.CnfAtom(TRUE), as.CnfFormula(TRUE), TRUE, TRUE)
record("constant versus bare logical", as.CnfClause(FALSE), FALSE, TRUE, TRUE)
record("different constants", as.CnfFormula(TRUE), as.CnfClause(FALSE), FALSE, FALSE)
record("constant universe ignored", as.CnfAtom(structure(TRUE, universe = u)),
  as.CnfAtom(structure(TRUE, universe = CnfUniverse())), TRUE, TRUE)
record("constant names retained by c", as.CnfAtom(c(alias = TRUE)), as.CnfAtom(TRUE), TRUE, FALSE)
record("proper versus constant", xa, as.CnfAtom(TRUE), FALSE, FALSE)
record("proper class guard", xa, as.CnfClause(xa), FALSE, FALSE)

old_collate = Sys.getlocale("LC_COLLATE")
stopifnot(nzchar(Sys.setlocale("LC_COLLATE", "C.UTF-8")))
unicode = c("\u00e9", "e\u0301")
ux = CnfSymbol(u, "UnicodeValues", c(unicode, "other"))
record("collation tie equal set", CnfAtom(ux, unicode), CnfAtom(ux, rev(unicode)), TRUE, FALSE)
record("collation tie unequal singletons", CnfAtom(ux, unicode[[1L]]), CnfAtom(ux, unicode[[2L]]), FALSE, FALSE)
record("equivalent encodings", CnfAtom(ux, unicode[[1L]]),
  CnfAtom(ux, iconv(unicode[[1L]], from = "UTF-8", to = "latin1")), TRUE, TRUE)
invisible(Sys.setlocale("LC_COLLATE", old_collate))

# Caller-requested weaker comparisons are outside the default theorem.
cx = as.CnfClause(xa)
cy = as.CnfClause(ya)
stopifnot(!identical(grid$X == "a", grid$Y == "a"))
option_controls = list(
  default = isTRUE(all.equal(cx, cy)),
  check_attributes_false = isTRUE(all.equal(cx, cy, check.attributes = FALSE)),
  check_names_false = isTRUE(all.equal(cx, cy, check.names = FALSE)),
  use_names_false = isTRUE(all.equal(cx, cy, use.names = FALSE)),
  formula_check_attributes_false = isTRUE(all.equal(as.CnfFormula(cx), as.CnfFormula(cy), check.attributes = FALSE)),
  atom_check_attributes_false = isTRUE(all.equal(xa, ya, check.attributes = FALSE)))
stopifnot(identical(option_controls, list(default = FALSE, check_attributes_false = TRUE,
  check_names_false = TRUE, use_names_false = FALSE, formula_check_attributes_false = TRUE,
  atom_check_attributes_false = FALSE)))
cat("Intentional caller-option controls:\n")
print(option_controls)

base_controls = c(
  list_names_detected = !isTRUE(all.equal.list(list(X = "a"), list(Y = "a"))),
  list_lengths_detected = !isTRUE(all.equal.list(list("a"), list("a", "a"))),
  character_lengths_detected = !isTRUE(all.equal.character("a", c("a", "a"))),
  character_values_detected = !isTRUE(all.equal.character(c("a", "a"), c("a", "b"))))
stopifnot(all(base_controls))
contract_file = file.path(here, paste0("base_contract_", runtime, ".txt"))
sink(contract_file)
cat(R.version.string, "\n")
for (name in c("all.equal.list", "all.equal.character", "attr.all.equal", "all.equal.environment", "sort.default")) {
  cat("\n", name, "\n", paste(deparse(get(name, baseenv())), collapse = "\n"), "\n", sep = "")
}
sink()
result = list(R = R.version.string, checkmate = as.character(packageVersion("checkmate")),
  digest = as.character(packageVersion("digest")), default_controls = records,
  option_controls = option_controls, base_controls = as.list(base_controls), source_md5 = as.list(tools::md5sum(files)))
write_json(result, file.path(here, paste0("results_", runtime, ".json")), pretty = TRUE, auto_unbox = TRUE)
cat("Default CNF controls:", length(records), "; all TRUE results preserve content.\n")
