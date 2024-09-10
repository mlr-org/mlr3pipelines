# An expression of the form "X %in% {x1, x2, x3}"
#' @title Atoms for CNF Formulas
#'
#' @description
#' `Atom` objects represent a single statement that is used to build up CNF formulae.
#' They are mostly intermediate, created using the [`%among%`] operator or [`CnfAtom()`]
#' directly, and combined into [`CnfClause`] and [`CnfFormula`] objects.
#' [`CnfClause`] and [`CnfFormula`] do not, however, contain [`Atom`] objects directly,
#'
#' `Atom`s contain an indirect reference to a [`CnfSymbol`] by referencing its name
#' and its [`CnfUniverse`]. They furthermore contain a set of values. An `Atom`
#' represents a statement asserting that the given symbol takes up one of the
#' given values.
#'
#' If the set of values is empty, the `Atom` represents a contradiction (FALSE).
#' If it is the full domain of the symbol, the `Atom` represents a tautology (TRUE).
#' These values can be converted to, and from, `logical(1)` values using `as.logical()`
#' and `as.CnfAtom()`.
#'
#' `Atom` objects can be negated using the `!` operator, which will return the `Atom`
#' representing set membership in the complement of the symbol with respect to its domain.
#' `Atom`s can furthermore be combined using the `|` operator to form a [`CnfClause`],
#' and using the `&` operator to form a [`CnfFormula`]. This happens even if the
#' resulting statement could be represented as a single `Atom`.
#'
#' This is part of the CNF representation tooling, which is currently considered
#' experimental; it is for internal use.
#'
#' @details
#' We would have preferred to overload the `%in%` operator, but this is currently
#' not easily possible in R. We therefore created the `%among%` operator.
#'
#' The internal representation of a `CnfAtom` may change in the future.
#'
#' @param symbol ([`CnfSymbol`]) \cr
#'   The symbol to which the atom refers.
#' @param values (`character`) \cr
#'   The values that the symbol can take.
#' @param e1 (`CnfSymbol`) \cr
#'   Left-hand side of the `%among%` operator.
#'   Passed as `symbol` to `CnfAtom()`.
#' @param e2 (`character`) \cr
#'   Right-hand side of the `%among%` operator.
#'   Passed as `values` to `CnfAtom()`.
#' @param x (any) \cr
#'   The object to be coerced to a `CnfAtom` by `as.CnfAtom`.
#'   Only `logical(1)` and `CnfAtom` itself are currently supported.
#' @return A new `CnfAtom` object.
#' @examples
#' u = CnfUniverse()
#' X = CnfSymbol(u, "X", c("a", "b", "c"))
#'
#' CnfAtom(X, c("a", "b"))
#' X %among% "a"
#' X %among% character(0)
#' X %among% c("a", "b", "c")
#'
#' as.logical(X %among% character(0))
#' as.CnfAtom(TRUE)
#'
#' !(X %among% "a")
#'
#' X %among% "a" | X %among% "b"  # creates a CnfClause
#'
#' X %among% "a" & X %among% c("a", "b")  # creates a CnfFormula
#' @family CNF representation objects
#' @keywords internal
#' @export
CnfAtom = function(symbol, values) {
  assert_class(symbol, "CnfSymbol")
  domain = attr(symbol, "universe")[[symbol]]
  assert_subset(values, domain)
  if (all(domain %in% values)) {
    entry = TRUE
  } else if (length(values) == 0) {
    entry = FALSE
  }  else {
    entry = list(symbol = c(symbol), values = unique(values))
  }
  structure(
    entry,
    universe = attr(symbol, "universe"),
    class = "CnfAtom"
  )
}

#' @export
print.CnfAtom = function(x, ...) {
  if (isTRUE(x)) {
    cat("CnfAtom: <TRUE>\n")
  } else if (isFALSE(x)) {
    cat("CnfAtom: <FALSE>\n")
  } else {
    cat(sprintf("CnfAtom: %s \U2208 {%s}.\n", x$symbol, paste(x$values, collapse = ", ")))
  }
  invisible(x)
}

#' @export
format.CnfAtom = function(x, ...) {
  if (isTRUE(x)) {
    return("CnfAtom: T")
  } else if (isFALSE(x)) {
    return("CnfAtom: F")
  } else {
    return(sprintf("CnfAtom(%s)", x$symbol))
  }
}

# construct CnfAtom with `X %among% c("a", "b", "c")`
# we cannot overload `%in%`, unfortunately
#' @export
#' @rdname CnfAtom
`%among%` = function(e1, e2) {
  UseMethod("%among%")
}

#' @export
`%among%.CnfSymbol` = function(e1, e2) {
  CnfAtom(e1, e2)
}

#' @export
`%among%.default` = function(e1, e2) {
  stop("%among% operation not defined for LHS. %among% should typically be used with a CnfSymbol.")
}


#' @rdname CnfAtom
#' @export
as.CnfAtom = function(x) {
  UseMethod("as.CnfAtom")
}

#' @export
as.CnfAtom.default = function(x) {
  stop("Cannot convert object to CnfAtom.")
}

#' @export
as.CnfAtom.logical = function(x) {
  assert_flag(x)
  structure(
    x,
    universe = attr(x, "universe"),
    class = "CnfAtom"
  )
}

#' @export
as.CnfAtom.CnfAtom = function(x) {
  x
}

#' @export
as.logical.CnfAtom = function(x) {
  if (is.logical(x)) {
    return(unclass(x))
  }
  return(NA)
}

#' @export
all.equal.CnfAtom = function(target, current, ...) {
  if (is.logical(target) && is.logical(current)) {
    # compare truth-values directly, even if they disagree on universe
    # (since logical atoms sometimes have universe set to NULL)
    if (identical(c(target), c(current))) {
      return(TRUE)
    }
    return("target and current are both logicals but not equal")
  }
  if (is.logical(target) || is.logical(current)) {
    return("target and current are not both logicals")
  }
  if (!inherits(current, "CnfAtom")) {
    return("current is not a CnfAtom")
  }
  target$values = sort(target$values)
  current$values = sort(current$values)
  all.equal.list(target, current, ...)
}

#' @rawNamespace if (getRversion() >= "4.3.0") S3method(chooseOpsMethod,CnfAtom)
chooseOpsMethod.CnfAtom <- function(x, y, mx, my, cl, reverse) TRUE

#' @export
`&.CnfAtom` = function(e1, e2) {
  # Will return a CnfFormula, so we can just delegate to there.
  # `&.CnfFormula` handles conversion.
  `&.CnfFormula`(e1, e2)
}

#' @export
`|.CnfAtom` = function(e1, e2) {
  if (inherits(e2, "CnfFormula")) {
    # `|.CnfFormula` handles conversion
    return(`|.CnfFormula`(e1, e2))
  } else if (is.logical(e2) || inherits(e2, "CnfAtom")) {
    if (isFALSE(e1) || isTRUE(e2)) return(as.CnfAtom(e2))
    if (isFALSE(e2) || isTRUE(e1)) return(e1)
  }
  # either two proper CnfAtoms, or e2 is a CnfClause.
  CnfClause(list(e1, e2))
}

#' @export
`!.CnfAtom` = function(x) {
  if (is.logical(x)) {
    return(as.CnfAtom(!unclass(x)))
  }
  structure(
    list(symbol = x$symbol, values = setdiff(attr(x, "universe")[[x$symbol]], x$values)),
    universe = attr(x, "universe"),
    class = "CnfAtom"
  )
}
