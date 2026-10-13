#' @title Symbol Table for CNF Formulas
#'
#' @description
#' A symbol table for CNF formulas. The `CnfUniverse` is a by-reference object
#' that stores the domain of each symbol. Symbols are created with [`CnfSymbol()`]
#' and can be retrieved with `$`.
#' Using `[[` retrieves a given symbol's domain.
#' Symbol lookup with `$` uses the same name checks and UTF-8 normalization as
#' [`CnfSymbol()`]. Keep the character locale (`LC_CTYPE`) unchanged while a universe is in use.
#'
#' It is only possible to combine symbols from the same (identical) universe.
#'
#' This is part of the CNF representation tooling, which is currently considered
#' experimental; it is for internal use.
#'
#' @return A new `CnfUniverse` object.
#' @examples
#' u = CnfUniverse()
#' X = CnfSymbol(u, "X", c("a", "b", "c"))
#' Y = CnfSymbol(u, "Y", c("d", "e", "f"))
#'
#' u$X
#' u[["Y"]]
#'
#' X %among% c("a", "c")
#' u$X %among% c("a", "c")
#' Y %among% c("d", "e", "f")
#' Y %among% character(0)
#'
#' u$X %among% "a" | u$Y %among% "d"
#' @family CNF representation objects
#' @keywords internal
#' @export
CnfUniverse = function() structure(new.env(parent = emptyenv()), class = "CnfUniverse")

# We allow retrieving symbols from the universe by name.
#' @export
`$.CnfUniverse` = function(universe, name) {
  name = normalize_cnf_name(name)
  if (!exists(name, universe)) {
    stopf("Variable '%s' does not exist in the universe.", name)
  }
  structure(
    name,
    universe = universe,
    class = "CnfSymbol"
  )
}

#' @method all.equal CnfUniverse
#' @export
all.equal.CnfUniverse = function(target, current, all.names = TRUE, evaluate = TRUE, ...) {
  assert_flag(all.names)
  assert_flag(evaluate)
  if (!is.environment(target) || !is.environment(current) || !evaluate) {
    return(base::all.equal.environment(target, current, all.names = all.names, evaluate = evaluate, ...))
  }
  if (identical(target, current)) return(TRUE)

  normalize = function(universe) {
    entries = as.list.environment(universe, all.names = all.names, sorted = FALSE)
    if (!length(entries)) return(entries)
    names(entries) = enc2utf8(names(entries))
    entries[order(names(entries), method = "radix")]
  }
  all.equal.list(normalize(target), normalize(current), ...)
}

#' @export
print.CnfUniverse = function(x, ...) {
  if (!length(x)) {
    cat("CnfUniverse (empty).\n")
    return(invisible(x))
  }
  cat("CnfUniverse with variables:\n")
  for (var in names(x)) {
    cat(sprintf("  %s: {%s}\n", var, paste(get(var, x), collapse = ", ")))
  }
  invisible(x)
}

#' @export
format.CnfUniverse = function(x, ...) {
  return(sprintf("CnfUniverse(%s)", length(x)))
}
