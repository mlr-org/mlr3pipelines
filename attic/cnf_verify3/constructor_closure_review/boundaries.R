# Independent controls for the proof's accepted grammar and exclusions.
suppressPackageStartupMessages(library(checkmate))
stopf = function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)
map_chr = function(.x, .f, ...) vapply(.x, .f, character(1L), ...)
for (path in file.path("R", paste0(c("CnfUniverse", "CnfSymbol", "CnfAtom",
  "CnfClause", "CnfFormula", "CnfFormula_simplify"), ".R"))) source(path)
cat(R.version.string, "; checkmate ", as.character(packageVersion("checkmate")), "\n", sep = "")
checked = 0L
expect_error = function(expr, label) {
  error = tryCatch({force(expr); NULL}, error = conditionMessage)
  stopifnot(is.character(error), length(error) == 1L)
  cat(label, ": expected error: ", error, "\n", sep = "")
  checked <<- checked + 1L
}
outer_payload = function(x) {
  attr(x, "class") = NULL
  attr(x, "universe") = NULL
  x
}
expect_payload = function(expr, expected, label) {
  answer = force(expr)
  stopifnot(identical(outer_payload(answer), expected))
  cat(label, ": expected payload\n", sep = "")
  checked <<- checked + 1L
}

u = CnfUniverse()
x = CnfSymbol(u, "X", matrix(c("a", "b", "b", "c"), 2L))
y = CnfSymbol(u, "Y", I(array(c("a", "b", "b", "c"), c(1L, 2L, 2L))))
a = CnfAtom(x, "a")
ab = CnfAtom(x, c("a", "b"))
bc = CnfAtom(x, c("b", "c"))
t = CnfAtom(x, c("a", "b", "c"))
f = CnfAtom(x, character())
ca = as.CnfClause(a)
cab = as.CnfClause(ab)
fa = as.CnfFormula(a)
v = CnfUniverse()
z = CnfSymbol(v, "Z", c("a", "b", "c"))
foreign = CnfAtom(z, "a")

expect_payload(CnfClause(list()), FALSE, "empty Clause")
expect_payload(CnfFormula(list()), TRUE, "empty Formula")
expect_payload(CnfClause(list(as.CnfAtom(TRUE), foreign)), TRUE, "free TRUE masks later Clause owner")
expect_error(CnfClause(list(foreign, as.CnfAtom(TRUE))), "later free TRUE still owner-checked in Clause")
expect_error(CnfClause(list(as.CnfAtom(FALSE), a)), "free FALSE first Clause")
expect_error(CnfClause(list(a, as.CnfAtom(FALSE))), "free FALSE last Clause")
expect_payload(CnfClause(list(ab, bc, foreign)), TRUE, "full union masks later Clause owner")
expect_error(CnfClause(list(t, 1)), "upfront Clause types checked after apparent TRUE")
expect_payload(CnfClause(list(a, f)), list(X = "a"), "same-owner FALSE in Clause")
expect_payload(CnfClause(matrix(list(a, a), 1L)), list(X = "a"), "dimensional outer atom list")

expect_error(CnfFormula(list(as.CnfClause(TRUE), ca)), "free TRUE first Formula")
expect_payload(CnfFormula(list(ca, as.CnfClause(TRUE))), list(list(X = "a")), "free TRUE last Formula")
expect_payload(CnfFormula(list(ca, as.CnfClause(FALSE))), FALSE, "only Clauses before FALSE")
expect_payload(CnfFormula(list(as.CnfClause(FALSE), fa)), FALSE, "FALSE before nested Formula")
expect_error(CnfFormula(list(fa, as.CnfClause(f))), "nested Formula before same-owner FALSE")
expect_error(CnfFormula(list(fa, as.CnfClause(FALSE))), "nested Formula before free FALSE")
expect_error(CnfFormula(list(as.CnfClause(FALSE), TRUE)), "upfront Formula types checked after apparent FALSE")
expect_payload(CnfFormula(list(as.CnfClause(t), ca)), list(list(X = "a")), "same-owner TRUE first Formula")
expect_payload(CnfFormula(array(list(cab, ca), c(1L, 1L, 2L))), list(list(X = "a")), "dimensional outer clause list")
expect_payload(CnfFormula(list(ca, as.CnfClause(CnfAtom(z, c("a", "b", "c"))))), list(list(X = "a")), "foreign TRUE skipped before owner check")
expect_error(CnfFormula(list(ca, as.CnfClause(foreign))), "proper foreign Formula member")

expect_error(as.list(as.CnfClause(TRUE)), "known as.list TRUE Clause")
expect_payload(CnfClause(as.list(as.CnfClause(f))), FALSE, "FALSE Clause round trip")
expect_payload(CnfFormula(as.list(as.CnfFormula(t))), TRUE, "TRUE Formula round trip")
expect_payload(CnfFormula(as.list(as.CnfFormula(f))), FALSE, "FALSE Formula round trip")
expect_payload(CnfClause(as.list(ca)), list(X = "a"), "proper Clause round trip")

raw_class_loss = `|.CnfClause`(TRUE, ca)
stopifnot(identical(raw_class_loss, TRUE), identical(class(raw_class_loss), "logical"))
cat("raw TRUE | proper Clause: correct truth, missing promised Clause class\n")
checked = checked + 1L
expect_error(CnfFormula(list(raw_class_loss)), "raw TRUE class-loss consequence")
expect_error(CnfSymbol(CnfUniverse(), "", c("a", "b")), "empty binding name")
expect_error(CnfSymbol(CnfUniverse(), NA_character_, c("a", "b")), "missing binding value")
expect_error(CnfSymbol(CnfUniverse(), "X", c("a", NA_character_)), "missing domain value")
expect_payload(as.CnfClause(CnfAtom(CnfSymbol(CnfUniverse(), setNames("X", NA_character_),
  setNames(c("a", "b"), c(NA_character_, ""))), "a")), list(X = "a"), "missing names are attributes")

for (empty in list(NULL, integer(), logical(), raw(), complex(), list(),
    matrix(character(), 0L, 3L), array(character(), c(2L, 0L, 2L)))) {
  stopifnot(isFALSE(CnfAtom(x, empty)))
  checked = checked + 1L
}
for (bad in list(TRUE, 1L, 1, 1i, as.raw(1), list("a"), factor("a"), NA_character_)) {
  expect_error(CnfAtom(x, bad), paste("nonempty rejected", typeof(bad), paste(class(bad), collapse = "/")))
}
for (constant in list(setNames(TRUE, NA_character_), matrix(FALSE, 1L), array(TRUE, c(1L, 1L, 1L)))) {
  for (converter in list(as.CnfAtom, as.CnfClause, as.CnfFormula)) {
    out = converter(constant)
    stopifnot(identical(as.vector(out), as.vector(constant)), identical(as.vector(!out), !as.vector(constant)))
    checked = checked + 1L
  }
}

warnings = character()
native = withCallingHandlers(tryCatch(a & ca, error = conditionMessage), warning = function(w) {
  warnings <<- c(warnings, conditionMessage(w))
  invokeRestart("muffleWarning")
})
if (getRversion() < "4.3.0") {
  stopifnot(any(grepl("Incompatible methods", warnings)), is.character(native))
  cat("native mixed old-R Ops: documented incompatible-method exclusion\n")
} else {
  stopifnot(!length(warnings), identical(outer_payload(native), list(list(X = "a"))))
  cat("native mixed current-R Ops: canonical expected payload\n")
}
expect_payload(`&.CnfFormula`(a, ca), list(list(X = "a")), "portable direct Formula method")

cat("\nInspected base bodies (no bytecode addresses):\n")
for (name in c("setdiff", ".set_ops_need_as_vector", "unique.matrix", "unique.array", "unique.default", "I", "[.AsIs")) {
  if (!exists(name, baseenv(), inherits = FALSE)) next
  cat("\n", name, "\n", paste(deparse(get(name, baseenv())), collapse = "\n"), "\n", sep = "")
}
cat("\nDirected duplicate-row case:\n")
sample_values = matrix(c("b", "b", "a", "a", "a", "b"), 3L)
dput(sample_values)
dput(unique(sample_values))
sample_atom = CnfAtom(x, sample_values)
cat("ordinary atom values:\n"); dput(sample_atom$values)
cat("AsIs atom values:\n"); dput(CnfAtom(x, I(sample_values))$values)
cat("complement atom values:\n"); dput((!sample_atom)$values)
cat("directly negated Formula payload:\n"); dput(outer_payload(!as.CnfFormula(sample_atom)))
cat("\nGrammar and exclusion controls:", checked, "\n")
