# A clause is a disjunction of atoms.
# X %among% c("a", "b", "c") | Y %among% c("d", "e", "f")

#' @title Clauses in CNF Formulas
#'
#' @description
#' A `CnfClause` is a disjunction of [`CnfAtom`] objects. It represents a statement
#' that is true if at least one of the atoms is true. These are for example of the form
#' ```r
#'   X %among% c("a", "b", "c") | Y %among% c("d", "e", "f") | ...
#' ```
#'
#' `CnfClause` objects can be constructed explicitly, using the `CnfClause()` constructor,
#' or implicitly, by using the `|` operator on [`CnfAtom`]s or other `CnfClause` objects.
#'
#' `CnfClause` objects which are not tautologies or contradictions are named lists;
#' the value ranges of each symbol can be accessed using `[[`, and these clauses
#' can be subset using `[` to get clauses containing only the indicated symbols.
#' However, to get a list of [`CnfAtom`] objects, use `as.list()`.
#' Note that the simplified form of a clause containing a contradiction is the empty list.
#'
#' Upon construction, the `CnfClause` is simplified by (1) removing contradictions, (2) unifying
#' atoms that refer to the same symbol, and (3) evaluating to `TRUE` if any atom is `TRUE`.
#' Note that the order of atoms in a clause is not preserved.
#'
#' Using `CnfClause()` on lists that contain other `CnfClause` objects will create
#' a clause that is the disjunction of all atoms in all clauses.
#'
#' If a `CnfClause` contains no atoms, or only `FALSE` atoms, it evaluates to `FALSE`.
#' If it contains at least one atom that is always true, the clause evaluates to `TRUE`.
#' These values can be converted to, and from, `logical(1)` values using `as.logical()`
#' and `as.CnfClause()`.
#'
#' `CnfClause` objects can be negated using the `!` operator, and combined using the
#' `&` operator. Both of these operations return a [`CnfFormula`], even if the result
#' could in principle be represented as a single `CnfClause`.
#'
#' This is part of the CNF representation tooling, which is currently considered
#' experimental; it is for internal use.
#'
#' @details
#' We are undecided whether it is a better idea to have `as.list()` return a named list
#' or an unnamed one. Calling `as.list()` on a `CnfClause` with a tautology returns
#' a tautology-atom, which does not have a name. We currently return a named list
#' for other clauses, as this makes subsetting by name commute with `as.list()`.
#' However, this behaviour may change in the future.
#'
#' @param atoms (`list` of ([`CnfAtom`] | `CnfClause`)) \cr
#'   A list of [`CnfAtom`] or other `CnfClause` objects.
#'   The clause represents the disjunction of these atoms.
#' @param x (any) \cr
#'   The object to be coerced to a `CnfClause` by `as.CnfClause`.
#'   Only `logical(1)`, [`CnfAtom`], and `CnfClause` itself are currently supported.
#' @return A new `CnfClause` object.
#' @examples
#' u = CnfUniverse()
#' X = CnfSymbol(u, "X", c("a", "b", "c"))
#' Y = CnfSymbol(u, "Y", c("d", "e", "f"))
#'
#' CnfClause(list(X %among% c("a", "b"), Y %among% c("d", "e")))
#' cls = X %among% c("a", "b") | Y %among% c("d", "e")
#' cls
#'
#' as.list(cls)
#'
#' as.CnfClause(X %among% c("a", "b"))
#'
#' # The same symbols are unified
#' X %among% "a" | Y %among% "d" | X %among% "b"
#'
#' # tautology evaluates to TRUE
#' X %among% "a" | X %among% "b" | X %among% "c"
#'
#' # contradictions are removed
#' X %among% "a" | Y %among% character(0)
#'
#' # create CnfFormula:
#' !(X %among% "a" | Y %among% "d")
#'
#' # also a CnfFormula, even if it contains a single clause:
#' !CnfClause(list(X %among% "a"))
#' (X %among% c("a", "c") | Y %among% "d") &
#'   (X %among% c("a", "b") | Y %among% "d")
#' @family CNF representation objects
#' @keywords internal
#' @export
CnfClause = function(atoms) {
  assert_list(atoms, types = c("CnfAtom", "CnfClause"))
  if (!length(atoms)) return(as.CnfClause(FALSE))

  entries = list()
  universe = attr(atoms[[1]], "universe")
  for (a in atoms) {
    a_bare = c(a)
    if (!identical(attr(a, "universe"), universe)) {
      stop("All symbols must be in the same universe.")
    }
    if (isTRUE(a_bare)) {
      entries = TRUE
      break
    }
    if (isFALSE(a_bare)) {
      next
    }
    if (inherits(a, "CnfAtom")) {
      entries[[a$symbol]] = unique(c(entries[[a$symbol]], a$values))
      if (all(universe[[a$symbol]] %in% entries[[a$symbol]])) {
        entries = TRUE
        break
      }
    } else {
      # union  with another CnfClause objects
      for (sym in names(a)) {
        entries[[sym]] = unique(c(entries[[sym]], a[[sym]]))
        if (all(universe[[sym]] %in% entries[[sym]])) {
          entries = TRUE
          break
        }
      }
      if (identical(entries, TRUE)) break
    }
  }
  structure(
    if (!length(entries)) FALSE else entries,
    universe = universe,
    class = "CnfClause"
  )
}

#' @export
print.CnfClause = function(x, ...) {
  x_bare = c(x)
  if (isTRUE(x_bare)) {
    cat("CnfClause: TRUE\n")
  } else if (isFALSE(x_bare)) {
    cat("CnfClause: FALSE\n")
  } else {
    cat("CnfClause:\n")
    elements = map_chr(names(x), function(sym) {
      sprintf("%s \U2208 {%s}", sym, paste(x[[sym]], collapse = ", "))
    })
    cat(strwrap(paste(elements, collapse = " | "), prefix = "  "), sep = "\n")
  }
  invisible(x)
}

#' @export
format.CnfClause = function(x, ...) {
  x_bare = c(x)
  if (isTRUE(x_bare)) {
    return("CnfClause: T")
  } else if (isFALSE(x_bare)) {
    return("CnfClause: F")
  } else {
    return(sprintf("CnfClause(%s)", length(x)))
  }
}

#' @rdname CnfClause
#' @export
as.CnfClause = function(x) {
  UseMethod("as.CnfClause")
}

#' @export
as.CnfClause.default = function(x) {
  stop("Cannot convert object to CnfClause.")
}

#' @export
as.CnfClause.logical = function(x) {
  assert_flag(x)
  return(structure(
    x,
    universe = attr(x, "universe"),
    class = "CnfClause"
  ))
}

#' @export
as.CnfClause.CnfAtom = function(x) {
  CnfClause(list(x))
}

#' @export
as.CnfClause.CnfClause = function(x) {
  x
}

#' @export
as.list.CnfClause = function(x) {
  if (isFALSE(x)) return(list())
  x = unclass(x)
  if (isTRUE(x)) return(as.CnfAtom(x))
  lapply(names(x), function(sym) {
    structure(list(symbol = sym, values = x[[sym]]), universe = attr(x, "universe"), class = "CnfAtom")
  })
}

#' @export
as.logical.CnfClause = function(x, ...) {
  if (is.logical(x)) {
    return(unclass(x))
  }
  return(NA)
}

#' @export
`[.CnfClause` = function(x, i) {
  if (missing(i)) return(x)
  assert_atomic(i)
  i = c(i)
  x_bare = unclass(x)
  true_length = if (isFALSE(x_bare)) 0 else length(x_bare)

  if (is.numeric(i)) {
    i = unique(floor(i))
  } else if (is.character(i)) {
    i = unique(i)
  } else if (!is.logical(i) && length(i) != 0) {
    # we explicitly allow length(i) == 0 (empty subset)
    stop("Invalid index type.")
  }
  if (length(i) == 0 || identical(i, 0) || (identical(i, FALSE) && !isFALSE((x_bare)))) {
    return(as.CnfClause(structure(FALSE, universe = attr(x, "universe"))))
  }
  if (isFALSE(x_bare)) {
    stop("Cannot subset a FALSE clause with anything other than 0, [], or 0-length-vector.")
  }
  if (isTRUE(x_bare)) {
    if (identical(i, 1) || identical(i, TRUE)) return(x)
    stop("Cannot subset a TRUE clause with anything other than 0, 1, [], or 0-length-vector.")
  }
  assert(
    check_numeric(i, lower = 0, upper = true_length, any.missing = FALSE),
    check_subset(i, names(x_bare)),
    check_logical(i, len = true_length),
    .var.name = "i"
  )
  entries = unclass(x)[i]
  structure(
    if (length(entries)) entries else FALSE,
    universe = attr(x, "universe"),
    class = "CnfClause"
  )
}

#' @export
all.equal.CnfClause = function(target, current, ...) {
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
  if (!inherits(current, "CnfClause")) {
    return("current is not a CnfClause")
  }

  normalize = function(clause) {
    # []-assign to preserve class and attributes
    clause[] = lapply(unclass(clause)[order(names(clause))], sort)
  }

  target = normalize(target)
  current = normalize(current)

  all.equal.list(target, current, ...)
}

#' @rawNamespace if (getRversion() >= "4.3.0") S3method(chooseOpsMethod,CnfClause)
chooseOpsMethod.CnfClause <- function(x, y, mx, my, cl, reverse) TRUE

#' @export
`&.CnfClause` = function(e1, e2) {
  # Will return a CnfFormula, so we can just delegate to there.
  # `&.CnfFormula` handles conversion.
  `&.CnfFormula`(e1, e2)
}

#' @export
`|.CnfClause` = function(e1, e2) {
  if (inherits(e2, "CnfFormula")) {
    # `|.CnfFormula` handles conversion
    return(`|.CnfFormula`(e1, e2))
  }
  e1_bare = c(e1)
  e2_bare = c(e2)
  if (isFALSE(e1_bare) || isTRUE(e2_bare)) return(as.CnfClause(e2))
  if (isFALSE(e2_bare) || isTRUE(e1_bare)) return(e1)
  CnfClause(list(e1, e2))
}

#' @export
`!.CnfClause` = function(x) {
  !as.CnfFormula(x)
}

