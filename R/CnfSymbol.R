
# We represent symbols through a name and a pointer to their universe.
# We only allow operations between symbols that are in the same universe.
#' @title Symbols for CNF Formulas
#'
#' @description
#' Representation of Symbols used in CNF formulas. Symbols have a name and a
#' domain (a set of possible values), and are stored in a [`CnfUniverse`].
#'
#' Once created, it is currently not intended to modify or delete symbols.
#'
#' Symbols can be used in CNF formulas by creating [`CnfAtom`] objects, either
#' by using the `%among%` operator or by using the [`CnfAtom()`] constructor
#' explicitly.
#'
#' This is part of the CNF representation tooling, which is currently considered
#' experimental; it is for internal use.
#'
#' @param universe ([`CnfUniverse`]) \cr
#'   The universe in which the symbol is defined.
#' @param name (`character(1)`) \cr
#'   The name of the symbol.
#' @param domain (`character`) \cr
#'   The domain, i.e. the set of possible values for the symbol.
#'   Must not be empty.
#' @return A new `CnfSymbol` object.
#' @examples
#' u = CnfUniverse()
#' X = CnfSymbol(u, "X", c("a", "b", "c"))
#'
#' # Use symbols to create CnfAtom objects
#' X %among% c("a", "b")
#' X %among% "a"
#' X %among% character(0)
#' X %among% c("a", "b", "c")
#'
#' @family CNF representation objects
#' @keywords internal
#' @export
CnfSymbol = function(universe, name, domain) {
  assert_class(universe, "CnfUniverse")
  assert_string(name)
  assert_character(domain, any.missing = FALSE, min.len = 1)
  if (exists(name, universe)) {
    stopf("Variable '%s' already exists in the universe.", name)
  }
  assign(name, domain, universe)
  structure(
    name,
    universe = universe,
    class = "CnfSymbol"
  )
}

#' @export
print.CnfSymbol = function(x, ...) {
  cat(sprintf("CnfSymbol '%s' with domain {%s}.\n", c(x), paste(attr(x, "universe")[[x]], collapse = ", ")))
  invisible(x)
}

#' @export
format.CnfSymbol = function(x, ...) {
  sprintf("CnfSymbol(%s)", c(x))
}
