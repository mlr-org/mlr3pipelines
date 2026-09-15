source("attic/cnf_verify3/r_values/bootstrap.R")
stopifnot(getRversion() >= "4.3.0")
universe = CnfUniverse()
x = CnfSymbol(universe, "X", c("a", "b", "c"))
y = CnfSymbol(universe, "Y", c("a", "b", "c"))
atom = CnfAtom(x, "a")
clause = CnfClause(list(atom, CnfAtom(y, "b")))
formula = CnfFormula(list(clause, as.CnfClause(CnfAtom(x, c("a", "b")))))
assignments = expand.grid(X = universe[["X"]], Y = universe[["Y"]], stringsAsFactors = FALSE)
truth = function(object) {
  if (is.logical(object)) return(rep(as.vector(object), nrow(assignments)))
  if (inherits(object, "CnfAtom")) return(assignments[[as.character(object$symbol)]] %in% object$values)
  if (inherits(object, "CnfClause")) return(clause_truth(object, assignments))
  formula_truth(object, assignments)
}
flags = list(TRUE, FALSE, c(n = TRUE), c(n = FALSE), matrix(TRUE, 1L), matrix(FALSE, 1L),
  structure(TRUE, note = "metadata"), structure(FALSE, note = "metadata"))
names(flags) = c("true", "false", "named_true", "named_false", "matrix_true", "matrix_false", "attr_true", "attr_false")
operands = list(atom = atom, clause = clause, formula = formula)
for (cl in c("logical", "CnfAtom", "CnfClause", "CnfFormula")) {
  for (flag in names(flags)) {
    value = flags[[flag]]
    if (cl != "logical") value = get(paste0("as.", cl))(value)
    operands[[paste(cl, flag, sep = "_")]] = value
  }
}
expected_class = function(op, left, right) {
  left_cnf = inherits(left, c("CnfAtom", "CnfClause", "CnfFormula"))
  right_cnf = inherits(right, c("CnfAtom", "CnfClause", "CnfFormula"))
  if (!left_cnf && !right_cnf) return(NULL)
  if (op == "&" || inherits(left, "CnfFormula") || inherits(right, "CnfFormula")) return("CnfFormula")
  "CnfClause"
}
classes = errors = warnings = list()
checks = 0L
for (left_name in names(operands)) for (right_name in names(operands)) for (op in c("&", "|")) {
  left = operands[[left_name]]
  right = operands[[right_name]]
  # Only operations involving an actual CNF object are part of this contract.
  needed_class = expected_class(op, left, right)
  if (is.null(needed_class)) next
  label = paste(left_name, op, right_name)
  output = withCallingHandlers(tryCatch(do.call(op, list(left, right)), error = function(e) e), warning = function(w) {
    warnings[[length(warnings) + 1L]] <<- list(label, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  if (inherits(output, "error")) {
    errors[[length(errors) + 1L]] = list(label, conditionMessage(output))
    next
  }
  checks = checks + 1L
  stopifnot(identical(truth(output), do.call(op, list(truth(left), truth(right)))))
  if (!inherits(output, needed_class)) classes[[length(classes) + 1L]] = list(label = label,
    expected = needed_class, actual = class(output))
}
stopifnot(length(errors) == 0L, length(warnings) == 0L)
cat("R:", R.version.string, "; checkmate:", as.character(packageVersion("checkmate")), "\n")
cat("Ordered Boolean cases:", checks, "; wrong truth tables: 0; errors: 0; warnings: 0; class losses:", length(classes), "\n")
dput(classes)
saveRDS(list(checks = checks, class_losses = classes, errors = errors, warnings = warnings),
  "attic/cnf_verify3/r_values/logical_operands_results.rds")
