source("attic/cnf_verify3/constructor_domain_closure/harness.R")
cat(R.version.string, "; checkmate ", as.character(packageVersion("checkmate")), "\n", sep = "")
u = CnfUniverse()
x = CnfSymbol(u, "X", matrix(c("a", "b", "b", "c"), 2L))
y = CnfSymbol(u, "Y", setNames(c("a", "b", "a", "c"), c(NA, "", "same", "same")))
atom = CnfAtom(x, matrix(c("b", "b"), 1L))
stopifnot(identical(dim(atom$values), c(1L, 2L)), anyDuplicated(flat_values(atom$values)) > 0L)
cat("\nA publicly constructed atom can retain repeated matrix elements:\n")
dput(atom$values)
cat("Its negation is a flat unique complement:\n")
dput((!atom)$values)
stopifnot(identical((!atom)$values, c("a", "c")))
clause = as.CnfClause(atom)
cat("The ordinary Clause constructor globally deduplicates and flattens:\n")
dput(bare_payload(clause))
stopifnot(identical(bare_payload(clause), list(X = "b")))
cat("Formula negation constructs the same canonical complement directly:\n")
formula = as.CnfFormula(atom)
dput(bare_payload(!formula))
stopifnot(identical(bare_payload(!formula), list(list(X = c("a", "c")))))

reverse_atom = CnfAtom(x, matrix(c("b", "a", "b", "a"), 2L))
cat("\nDouble atom negation preserves support, but uses stored domain order:\n")
dput(reverse_atom$values)
dput((!(!reverse_atom))$values)
stopifnot(identical(flat_unique(reverse_atom$values), c("b", "a")),
  identical((!(!reverse_atom))$values, c("a", "b")))

symbol_variants = list("X", c(alias = "X"), setNames("X", NA_character_),
  matrix("X", 1L), array("X", c(1L, 1L, 1L)), I("X"))
for (name in symbol_variants) {
  v = CnfUniverse()
  sx = CnfSymbol(v, name, c("a", "b"))
  cl = as.CnfClause(CnfAtom(sx, "a"))
  stopifnot(identical(names(cl), "X"), identical(cl[["X"]], "a"), proper_clause(unclass(cl), v))
}
cat("\nSix ordinary scalar symbol-name representations produced the same canonical name.\n")

proper = atom | CnfAtom(y, "a")
owned_false_atom = CnfAtom(x, character())
owned_true_atom = CnfAtom(x, c("a", "b", "c"))
stopifnot(canonical_object(CnfClause(list(atom, owned_false_atom))),
  canonical_object(CnfFormula(list(as.CnfClause(owned_true_atom), proper))))
controls = list(
  clause_free_false_first = attempt(CnfClause(list(as.CnfAtom(FALSE), atom))),
  clause_free_false_last = attempt(CnfClause(list(atom, as.CnfAtom(FALSE)))),
  formula_free_true_first = attempt(CnfFormula(list(as.CnfClause(TRUE), proper))),
  nested_formula_then_false = attempt(CnfFormula(list(as.CnfFormula(proper), as.CnfClause(owned_false_atom))))
)
cat("\nPreviously recorded constructor exceptions, reproduced as exclusion controls:\n")
for (label in names(controls)) {
  stopifnot(nzchar(controls[[label]]$error))
  cat(label, ": ", controls[[label]]$error, "\n", sep = "")
}
class_loss = `|.CnfClause`(TRUE, proper)
stopifnot(isTRUE(class_loss), identical(class(class_loss), "logical"))
cat("Previously recorded TRUE | proper_clause result class: ", class(class_loss), "\n", sep = "")

# Supported dispatch is a separate boundary from evaluating a method body.
dispatch = attempt(atom & proper)
if (getRversion() < "4.3.0") {
  stopifnot(any(grepl("Incompatible methods", dispatch$warnings)), nzchar(dispatch$error))
  cat("Previously recorded old-R mixed Ops warning: ", dispatch$warnings[[1L]], "\n", sep = "")
  cat("Old-R native mixed Ops error: ", dispatch$error, "\n", sep = "")
} else {
  stopifnot(!length(dispatch$warnings), !nzchar(dispatch$error), canonical_object(dispatch$value))
  cat("Native mixed Atom & Clause dispatch returned a canonical Formula.\n")
}
assignments = expand.grid(X = c("a", "b", "c"), Y = c("a", "b", "c"), stringsAsFactors = FALSE)
direct = `&.CnfFormula`(atom, proper)
stopifnot(canonical_object(direct), identical(object_truth(direct, assignments),
  object_truth(atom, assignments) & object_truth(proper, assignments)))
cat("Direct Formula method has correct truth on all nine valuations on both runtimes.\n")
record_results("boundary_cases", c(symbol_name_variants = 6L, known_constructor_errors = 4L,
  known_class_loss = 1L, valuation_rows = 9L))
