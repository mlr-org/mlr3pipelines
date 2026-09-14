
# We represent symbols through a name and a pointer to their universe.
# We only allow operations between symbols that are in the same universe.
#' @title Symbols for CNF Formulas
#'
#' @description
#' Representation of Symbols used in CNF formulas. Symbols have a name and a
#' domain (a set of possible values), and are stored in a [`CnfUniverse`].
#'
#' Once created, it is currently not intended to modify or delete symbols.
#' Keep the character locale (`LC_CTYPE`) unchanged while a universe is in use.
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
#'   The nonempty name of the symbol. It must be representable without substitution
#'   in the current character locale so that environment bindings preserve its identity.
#' @param domain (`character`) \cr
#'   The domain, i.e. the set of possible values for the symbol.
#'   Must not be empty. Names and domains must be character vectors without dimensions
#'   or custom classes, missing values, byte-marked strings, or invalid encodings.
#'   Accepted text is converted to UTF-8; distinct Unicode code-point sequences remain distinct.
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
  name = normalize_cnf_name(name)
  assert_character(domain, any.missing = FALSE, min.len = 1)
  domain = normalize_cnf_text(domain, "domain")
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

normalize_cnf_text = function(x, var_name) {
  assert_character(x, any.missing = FALSE, .var.name = var_name)
  if (is.object(x) || !is.null(dim(x))) {
    stopf("Argument '%s' must be a character vector without dimensions or custom classes.", var_name)
  }
  encoding = Encoding(x)
  # Check native text before conversion, which can otherwise substitute printable escapes.
  if (any(encoding == "bytes") ||
    anyNA(iconv(x[encoding == "unknown"], from = "", to = "UTF-8", sub = NA))) {
    stopf("Argument '%s' must contain valid text without byte-marked strings.", var_name)
  }
  x = enc2utf8(x)
  # Older R versions do not consistently reject invalid Unicode scalar encodings.
  if (any(vapply(x, function(value) {
    codepoints = utf8ToInt(value)
    anyNA(codepoints) || any(codepoints > 0x10ffff)
  }, logical(1)))) {
    stopf("Argument '%s' must contain valid UTF-8 text.", var_name)
  }
  x
}

normalize_cnf_name = function(name) {
  assert_string(name, min.chars = 1)
  name = normalize_cnf_text(name, "name")
  native_name = iconv(name, from = "UTF-8", to = "", sub = NA)
  if (is.na(native_name) || enc2utf8(native_name) != name) {
    stop("Symbol name must be representable exactly in the native character locale.")
  }
  name
}

#' @export
print.CnfSymbol = function(x, ...) {
  cat(sprintf("CnfSymbol '%s' with domain {%s}.\n", unclass(x), paste(attr(x, "universe")[[x]], collapse = ", ")))
  invisible(x)
}

#' @export
format.CnfSymbol = function(x, ...) {
  sprintf("CnfSymbol(%s)", unclass(x))
}
