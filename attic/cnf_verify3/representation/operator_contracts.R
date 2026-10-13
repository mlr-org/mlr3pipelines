# Exhaustive ordered-pair class and truth contracts for representative operands.
# Run under R >= 4.3; real checkmate validators are used.
if (nzchar(Sys.getenv("CNF_REP_RLIB"))) .libPaths(c(Sys.getenv("CNF_REP_RLIB"), .libPaths()))
suppressMessages(library(checkmate))
for (nm in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(nm, ".R")))
}
stopifnot(getRversion() >= "4.3.0")
u = CnfUniverse()
x = CnfSymbol(u, "X", c("a", "b", "c"))
y = CnfSymbol(u, "Y", c("a", "b", "c"))
atom = CnfAtom(x, "a")
clause = CnfClause(list(atom, CnfAtom(y, "b")))
formula = CnfFormula(list(clause, as.CnfClause(CnfAtom(y, c("a", "b")))))
operands = list(logical_true = TRUE, logical_false = FALSE)
for (class in c("CnfAtom", "CnfClause", "CnfFormula")) {
  convert = get(paste0("as.", class))
  for (value in c(TRUE, FALSE)) {
    operands[[paste0(class, "_", value, "_null")]] = convert(value)
    operands[[paste0(class, "_", value, "_universe")]] = convert(CnfAtom(x, if (value) u[["X"]] else character()))
  }
}
operands$atom = atom
operands$clause = clause
operands$formula = formula
assignments = expand.grid(X = u[["X"]], Y = u[["Y"]], stringsAsFactors = FALSE)
clause_truth = function(cl) {
  answer = rep(FALSE, nrow(assignments))
  for (s in names(cl)) answer = answer | assignments[[s]] %in% cl[[s]]
  answer
}
truth = function(x) {
  if (is.logical(x)) return(rep(c(x), nrow(assignments)))
  if (inherits(x, "CnfAtom")) return(assignments[[x$symbol]] %in% x$values)
  if (inherits(x, "CnfClause")) return(clause_truth(c(x)))
  answer = rep(TRUE, nrow(assignments))
  for (cl in c(x)) answer = answer & clause_truth(cl)
  answer
}
expected_class = function(op, lhs, rhs) {
  if (identical(class(lhs), "logical") && identical(class(rhs), "logical")) return("logical")
  if (op == "&" || inherits(lhs, "CnfFormula") || inherits(rhs, "CnfFormula")) return("CnfFormula")
  "CnfClause"
}
errors = class_failures = semantic_failures = warnings = list()
n_tests = 0L
for (ln in names(operands)) for (rn in names(operands)) for (op in c("&", "|")) {
  lhs = operands[[ln]]
  rhs = operands[[rn]]
  label = paste(ln, op, rn)
  n_tests = n_tests + 1L
  result = withCallingHandlers(tryCatch(do.call(op, list(lhs, rhs)), error = function(e) e), warning = function(w) {
    warnings[[length(warnings) + 1L]] <<- list(label = label, warning = conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  if (inherits(result, "error")) {
    errors[[length(errors) + 1L]] = list(label = label, error = conditionMessage(result))
    next
  }
  wanted = expected_class(op, lhs, rhs)
  if (!identical(class(result), wanted)) {
    class_failures[[length(class_failures) + 1L]] = list(label = label, actual = class(result), expected = wanted)
  }
  if (!identical(truth(result), do.call(op, list(truth(lhs), truth(rhs))))) {
    semantic_failures[[length(semantic_failures) + 1L]] = list(label = label, result = c(result))
  }
}
cat("R: ", R.version.string, "\n", sep = "")
cat("Ordered binary contracts: ", n_tests, "; class failures: ", length(class_failures),
  "; semantic failures: ", length(semantic_failures), "; errors: ", length(errors),
  "; warnings: ", length(warnings), "\n", sep = "")
dput(class_failures)
stopifnot(length(errors) == 0L, length(warnings) == 0L, length(semantic_failures) == 0L, length(class_failures) == 3L)
saveRDS(list(tests = n_tests, class_failures = class_failures, semantic_failures = semantic_failures,
  errors = errors, warnings = warnings, version = R.version.string),
  "attic/cnf_verify3/representation/operator_contracts_results.rds")
