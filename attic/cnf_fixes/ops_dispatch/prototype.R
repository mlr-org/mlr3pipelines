suppressPackageStartupMessages({ library(checkmate); library(mlr3misc) })
for (file in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(file, ".R")))
}
options(warn = 2)
u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("a", "b", "c"))
cat(R.version.string, "\n")
cat("Original mixed dispatch:", tryCatch({
  CnfAtom(X, "a") & as.CnfClause(CnfAtom(Y, "b"))
  "success"
}, error = conditionMessage), "\n")
and_impl = `&.CnfFormula`
or_formula_impl = `|.CnfFormula`
or_clause_impl = `|.CnfClause`
cnf_and = function(e1, e2) and_impl(e1, e2)
cnf_or = function(e1, e2) {
  if (inherits(e1, "CnfFormula") || inherits(e2, "CnfFormula")) {
    or_formula_impl(e1, e2)
  } else {
    or_clause_impl(e1, e2)
  }
}
`&.CnfAtom` = `&.CnfClause` = `&.CnfFormula` = cnf_and
`|.CnfAtom` = `|.CnfClause` = `|.CnfFormula` = cnf_or
bank = function(symbol) {
  values = list(CnfAtom(symbol, "a"), CnfAtom(symbol, character(0)), CnfAtom(symbol, c("a", "b", "c")))
  result = list(FALSE, TRUE)
  for (convert in list(as.CnfAtom, as.CnfClause, as.CnfFormula)) {
    result = c(result, lapply(values, convert))
  }
  result
}
worlds = expand.grid(X = c("a", "b", "c"), Y = c("a", "b", "c"), stringsAsFactors = FALSE)
eval_clause = function(clause) {
  Reduce(`|`, lapply(names(clause), function(symbol) worlds[[symbol]] %in% clause[[symbol]]), rep(FALSE, nrow(worlds)))
}
evaluate = function(x) {
  if (is.logical(x)) return(rep(as.vector(x), nrow(worlds)))
  if (inherits(x, "CnfAtom")) return(worlds[[x$symbol]] %in% x$values)
  if (inherits(x, "CnfClause")) return(eval_clause(unclass(x)))
  Reduce(`&`, lapply(unclass(x), eval_clause), rep(TRUE, nrow(worlds)))
}
run_cases = function() {
  total = 0L
  for (left in bank(X)) for (right in bank(Y)) {
    actual_and = left & right
    actual_or = left | right
    stopifnot(identical(evaluate(actual_and), evaluate(left) & evaluate(right)))
    stopifnot(identical(evaluate(actual_or), evaluate(left) | evaluate(right)))
    if (is.object(left) || is.object(right)) {
      stopifnot(inherits(actual_and, "CnfFormula"))
      expected_or = if (inherits(left, "CnfFormula") || inherits(right, "CnfFormula")) "CnfFormula" else "CnfClause"
      stopifnot(inherits(actual_or, expected_or))
    }
    total = total + 2L
  }
  total
}
cat("Shared aliases:", run_cases(), "operator cases passed, including truth tables and result classes\n")
for (name in c("&.CnfAtom", "&.CnfClause", "&.CnfFormula", "|.CnfAtom", "|.CnfClause", "|.CnfFormula")) {
  assign(name, compiler::cmpfun(get(name)))
}
cat("Separately byte-compiled aliases:", tryCatch(run_cases(), error = conditionMessage), "\n")
rm(list = c("&.CnfAtom", "&.CnfClause", "&.CnfFormula", "|.CnfAtom", "|.CnfClause", "|.CnfFormula"))
cnf_and = compiler::cmpfun(cnf_and)
cnf_or = compiler::cmpfun(cnf_or)
for (class in c("CnfAtom", "CnfClause", "CnfFormula")) {
  registerS3method("&", class, cnf_and, envir = asNamespace("base"))
  registerS3method("|", class, cnf_or, envir = asNamespace("base"))
}
cat("Shared compiled registrations:", run_cases(), "operator cases passed\n")
