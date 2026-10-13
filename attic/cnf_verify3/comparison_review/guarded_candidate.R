# Private function copies: modify only the proper-object normalization paths.
# Existing logical/type guards, universe behavior, attributes, and ... forwarding
# remain in the function bodies. Nothing is assigned to a production S3 method.
source("attic/cnf_verify3/comparison_review/helpers.R")

normalization_env = new.env(parent = globalenv())
normalization_env$canonical_range_review = function(values) sort(enc2utf8(values), method = "radix")
normalization_env$canonical_clause_review = function(clause) {
  reorder = order(enc2utf8(names(clause)), method = "radix")
  clause[] = lapply(unclass(clause)[reorder], canonical_range_review)
  names(clause) = enc2utf8(names(clause)[reorder])
  clause
}
environment(normalization_env$canonical_clause_review) = normalization_env
normalization_env$canonical_formula_review = function(formula) {
  formula[] = lapply(unclass(formula), canonical_clause_review)
  reorder = order(map_chr(unclass(formula), function(clause) {
    paste0(paste(names(clause), collapse = ".__."), digest::digest(c(clause), algo = "xxhash64"))
  }), method = "radix")
  formula[] = formula[reorder]
  names(formula) = names(formula)[reorder]
  formula
}
environment(normalization_env$canonical_formula_review) = normalization_env

guarded_methods = setNames(lapply(c("CnfAtom", "CnfClause", "CnfFormula"), function(class_name) {
  original = get(paste0("all.equal.", class_name), envir = globalenv())
  revised = original
  expressions = as.list(body(revised))
  replaced = 0L
  for (i in seq_along(expressions)) {
    expression = expressions[[i]]
    if (!is.call(expression) || !identical(expression[[1L]], as.name("="))) next
    if (class_name == "CnfAtom") {
      if (identical(expression[[2L]], quote(target$values))) {
        expressions[[i]] = quote({target$values = canonical_range_review(target$values)})[[2L]]
        replaced = replaced + 1L
      }
      if (identical(expression[[2L]], quote(current$values))) {
        expressions[[i]] = quote({current$values = canonical_range_review(current$values)})[[2L]]
        replaced = replaced + 1L
      }
    } else if (identical(expression[[2L]], as.name("normalize"))) {
      expressions[[i]] = if (class_name == "CnfClause") {
        quote({normalize = canonical_clause_review})[[2L]]
      } else {
        quote({normalize = canonical_formula_review})[[2L]]
      }
      replaced = replaced + 1L
    }
  }
  stopifnot(replaced == if (class_name == "CnfAtom") 2L else 1L)
  body(revised) = as.call(expressions)
  environment(revised) = normalization_env
  stopifnot(identical(formals(original), formals(revised)))
  revised
}), c("CnfAtom", "CnfClause", "CnfFormula"))

guarded_candidate_equal = function(target, current, ...) {
  method = guarded_methods[[class(target)]]
  method(target, current, ...)
}
