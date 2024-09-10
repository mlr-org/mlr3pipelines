#' @title Symbol Table for CNF Formulas
#'
#' @description
#' A symbol table for CNF formulas. The `CnfUniverse` is a by-reference object
#' that stores the domain of each symbol. Symbols are created with [`CnfSymbol()`]
#' and can be retrieved with `$`.
#' Using `[[` retrieves a given symbol's domain.
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
#' X %among% c("a", "d")
#' u$X %among% c("a", "d")
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
  assert_string(name)
  if (!exists(name, universe)) {
    stopf("Variable '%s' does not exist in the universe.", name)
  }
  structure(
    name,
    universe = universe,
    class = "CnfSymbol"
  )
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
