
### CNF representation of branch activation conditions
### Where doing it man. Where making this hapen
# For every position in a Graph, we want to express which PipeOpBranch needs
# to have which output active in order to activate this PipeOp.
# If a normal PipeOp has multipel inputs, then it is active when all of its inputs are active,
# corresponding to a logical AND.
# If a PipeOpUnbranch has multiple active inputs, then it is active when exactly one of its
# input is active, corresponding to a logical OR.
# We handle cases where a normal PipeOp with mixed inputs gives an error, or where
# a PipeOpUnbranch with multiple active inputs gives an error, by also representing
# the conditions under which an error is thrown.

# Container object for possible branch conditions.

# A formula is a conjunction of clauses.
# (X %among% c("a", "b", "c") | Y %among% c("d", "e", "f")) & (Z %among% c("g", "h", "i"))

#' @title CNF Formulas
#'
#' @description
#' A `CnfFormula` is a conjunction of [`CnfClause`] objects. It represents a statement
#' that is true if all of the clauses are true. These are for example of the form
#' ```r
#'   (X %among% "a" | Y %among% "d") & Z %among% "g"
#' ```
#'
#' `CnfFormula` objects can be constructed explicitly, using the `CnfFormula()` constructor,
#' or implicitly, by using the `&` operator on [`CnfAtom`]s, [`CnfClause`]s, or other `CnfFormula` objects.
#'
#' To get individual clauses from a formula, `[[` should not be used; instead, use `as.list()`.
#' Note that the simplified form of a formula containing a tautology is the empty list.
#'
#' Upon construction, the `CnfFormula` is simplified by using various heuristics.
#' This includes unit propagation, subsumption elimination, and self/hidden subsumption elimination
#' (see examples).
#' Note that the order of clauses in a formula is not preserved.
#'
#' Using `CnfFormula()` on lists that contain other `CnfFormula` objects will create
#' a formula that is the conjunction of all clauses in all formulas.
#' This may be somewhat more efficient than applying `&` many times in a row.
#'
#' If a `CnfFormula` contains no clauses, or only `TRUE` clauses, it evaluates to `TRUE`.
#' If it contains at least one clause that is, by itself, always false, the formula evaluates to `FALSE`.
#' Not all contradictions between clauses are recognized, however.
#' These values can be converted to, and from, `logical(1)` values using `as.logical()`
#' and `as.CnfFormula()`.
#'
#' `CnfFormula` objects can be negated using the `!` operator. Beware that this
#' may lead to an exponential blow-up in the number of clauses.
#'
#' This is part of the CNF representation tooling, which is currently considered
#' experimental; it is for internal use.
#'
#' @param clauses (`list` of [`CnfClause`]) \cr
#'   A list of [`CnfClause`] objects. The formula represents the conjunction of these clauses.
#' @param x (any) \cr
#'   The object to be coerced to a `CnfFormula` by `as.CnfFormula`.
#'   Only `logical(1)`, [`CnfAtom`], [`CnfClause`], and `CnfFormula` itself are currently supported.
#' @return A new `CnfFormula` object.
#' @examples
#' u = CnfUniverse()
#' X = CnfSymbol(u, "X", c("a", "b", "c"))
#' Y = CnfSymbol(u, "Y", c("d", "e", "f"))
#' Z = CnfSymbol(u, "Z", c("g", "h", "i"))
#'
#' frm = (X %among% c("a", "b") | Y %among% c("d", "e")) &
#'   Z %among% c("g", "h")
#' frm
#'
#' # retrieve individual clauses
#' as.list(frm)
#'
#' # Negation of a formula
#' # Note the parentheses, otherwise `!` would be applied to the first clause only.
#' !((X %among% c("a", "b") | Y %among% c("d", "e")) &
#'    Z %among% c("g", "h"))
#'
#' ## unit propagation
#' # The second clause can not be satisfied when X is "b", so "b" can be
#' # removed from the possibilities in the first clause.
#' (X %among% c("a", "b") | Y %among% c("d", "e")) &
#'   X %among% c("a", "c")
#'
#' ## subsumption elimination
#' # The first clause is a subset of the second clause; whenever the
#' # first clause is satisfied, the second clause is satisfied as well, so the
#' # second clause can be removed.
#' (X %among% "a" | Y %among% c("d", "e")) &
#'   (X %among% c("a", "b") | Y %among% c("d", "e") | Z %among% "g")
#'
#' ## self subsumption elimination
#' # If the first clause is satisfied but X is not "a", then Y must be "e".
#' # The `Y %among% "d"` part of the first clause can therefore be removed.
#' (X %among% c("a", "b") | Y %among% "d") &
#'   (X %among% "a" | Y %among% "e")
#'
#' ## hidden subsumption elimination
#' # The first two statements can only be satisfied if Y is either "d" or "e",
#' # since when X is "a" then Y must be "e", and when X is "b" then Y must be "d".
#' # The third statement is therefore implied by the first two, and can be
#' # removed.
#' (X %among% "a" | Y %among% "d") &
#'   (X %among% "b" | Y %among% "e") &
#'   (Y %among% c("d", "e"))
#'
#' ## Simple contradictions are recognized:
#' (X %among% "a") & (X %among% "b")
#' # Tautologies are preserved
#' (X %among% c("a", "b", "c")) & (Y %among% c("d", "e", "f"))
#'
#' # But not all contradictions are recognized.
#' # Builtin heuristic CnfFormula preprocessing is not a SAT solver.
#' contradiction = (X %among% "a" | Y %among% "d") &
#'   (X %among% "b" | Y %among% "e") &
#'   (X %among% "c" | Y %among% "f")
#' contradiction
#'
#' # Negation of a contradiction results in a tautology, which is recognized
#' # and simplified to TRUE. However, note that this operation (1) generally has
#' # exponential complexity in the number of terms and (2) is currently also not
#' # particularly well optimized
#' !contradiction
#' @family CNF representation objects
#' @keywords internal
#' @export
CnfFormula = function(clauses) {
  assert_list(clauses, types = c("CnfClause", "CnfFormula"))
  if (!length(clauses)) return(as.CnfFormula(TRUE))

  entries = list()
  other_entries = list()
  universe = attr(clauses[[1]], "universe")
  for (cl in clauses) {
    cl_bare = c(cl)
    if (isFALSE(cl_bare)) {
      entries = FALSE
      break
    }
    if (isTRUE(cl_bare)) {
      next
    }
    if (!identical(attr(cl, "universe"), universe)) {
      # if clauses[[1]] is FALSE, then it is possible that it has no
      # universe; however, in that case we will break before coming here.
      stop("All clauses must be in the same universe.")
    }

    ## don't unclass() here, since we check the class in the next line!
    if (inherits(cl, "CnfClause")) {
      entries[[length(entries) + 1]] = c(cl)
    } else {
      # union with another CnfFormula object
      other_entries[[length(other_entries) + 1]] = cl
    }
  }
  if (length(other_entries)) {
    entries = c(entries, unlist(other_entries, recursive = FALSE))
  }
  simplify_cnf(entries, universe)
}

#' @export
print.CnfFormula = function(x, ...) {
  x_bare = c(x)
  if (isTRUE(x_bare)) {
    cat("CnfFormula: TRUE\n")
  } else if (isFALSE(x_bare)) {
    cat("CnfFormula: FALSE\n")
  } else {
    cat("CnfFormula:\n     (")
    clauses = map_chr(unclass(x), function(clause) {
      elements = map_chr(names(clause), function(sym) {
        sprintf("%s \U2208 {%s}", sym, paste(clause[[sym]], collapse = ", "))
      })
      paste(elements, collapse = " | ")
    })
    clause_paragraphs = map_chr(strwrap(clauses, exdent = 4, simplify = FALSE), paste, collapse = "\n  ")
    cat(paste0(paste(clause_paragraphs, collapse = ")\n   & ("), ")\n"))
  }
  invisible(x)
}

#' @export
format.CnfFormula = function(x, ...) {
  x_bare = c(x)
  if (isTRUE(x_bare)) {
    return("CnfFormula: T")
  } else if (isFALSE(x_bare)) {
    return("CnfFormula: F")
  } else {
    return(sprintf("CnfFormula(%s)", length(x)))
  }
}

#' @rdname CnfFormula
#' @export
as.CnfFormula = function(x) {
  UseMethod("as.CnfFormula")
}

#' @export
as.CnfFormula.default = function(x) {
  stop("Cannot convert object to CnfFormula.")
}

#' @export
as.CnfFormula.logical = function(x) {
  assert_flag(x)
  return(structure(
    x,
    universe = attr(x, "universe"),
    class = "CnfFormula"
  ))
}

#' @export
as.CnfFormula.CnfAtom = function(x) {
  as.CnfFormula(as.CnfClause(x))
}

#' @export
as.CnfFormula.CnfClause = function(x) {
  CnfFormula(list(x))
}

#' @export
as.CnfFormula.CnfFormula = function(x) {
  x
}

#' @export
as.list.CnfFormula = function(x, ...) {
  x_bare = c(x)
  if (isTRUE(x_bare)) return(list())
  lapply(x_bare, structure, class = "CnfClause", universe = attr(x, "universe"))
}

#' @export
as.logical.CnfFormula = function(x, ...) {
  if (is.logical(x)) {
    return(unclass(x))
  }
  return(NA)
}

#' @export
all.equal.CnfFormula = function(target, current, ...) {
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
  if (!inherits(current, "CnfFormula")) {
    return("current is not a CnfFormula")
  }

  normalize = function(formula) {
    formula[] = lapply(unclass(formula), function(clause) {
      lapply(unclass(clause)[order(names(clause))], sort)
    })
    # sort by symbol names, then by hash
    # note we had to sort elements internally first before we can do this!
    reorder = order(map_chr(unclass(formula), function(clause) {
      paste0(paste(names(clause), collapse = ".__."), digest::digest(c(clause), algo = "xxhash64"))
    }))
    formula[] = formula[reorder]
    # formula should not have names, but in case this ever changes:
    # change the names in the same order as the clauses
    names(formula) = names(formula)[reorder]

    formula
  }
  target = normalize(target)
  current = normalize(current)

  all.equal.list(target, current, ...)
}

#' @rawNamespace if (getRversion() >= "4.3.0") S3method(chooseOpsMethod,CnfFormula)
chooseOpsMethod.CnfFormula <- function(x, y, mx, my, cl, reverse) TRUE

#' @export
`&.CnfFormula` = function(e1, e2) {
  e1_bare = c(e1)
  e2_bare = c(e2)
  e1 = as.CnfFormula(e1)
  e2 = as.CnfFormula(e2)
  if (isTRUE(e1_bare) || isFALSE(e2_bare)) return(e2)
  if (isTRUE(e2_bare) || isFALSE(e1_bare)) return(e1)
  if (!identical(attr(e1, "universe"), attr(e2, "universe"))) {
    stop("Both formulas must be in the same universe.")
  }
  simplify_cnf(c(e1, e2), attr(e1, "universe"))
}

#' @export
`|.CnfFormula` = function(e1, e2) {
  e1_bare = c(e1)
  e2_bare = c(e2)
  e1 = as.CnfFormula(e1)
  e2 = as.CnfFormula(e2)
  if (isFALSE(e1_bare) || isTRUE(e2_bare)) return(e2)
  if (isFALSE(e2_bare) || isTRUE(e1_bare)) return(e1)
  if (!identical(attr(e1, "universe"), attr(e2, "universe"))) {
    stop("Both formulas must be in the same universe.")
  }
  universe = attr(e1, "universe")
  if (length(e1) > length(e2)) {
    # we want the outer loop to be over the shorter of the two
    tmp = e1
    e1 = e2
    e2 = tmp
  }
  # distribute || into clauses
  distributed = lapply(unclass(e1), function(e1_clause) {
    eliminated = logical(length(e2))
    for (i2 in seq_along(e2)) {
      e2_clause = e2[[i2]]
      for (sym in names(e1_clause)) {
        # add the symbols from e1_clause to e2_clause
        # (if e2_clause does not contain a symbol, it is added)
        e2_clause[[sym]] = unique(c(e1_clause[[sym]], e2_clause[[sym]]))
        # faster than test_set_equal
        if (all(universe[[sym]] %in% e2_clause[[sym]])) {
          eliminated[[i2]] = TRUE
          break
        }
      }
      e2[[i2]] = e2_clause
    }
    e2[!eliminated]
  })
  simplify_cnf(unlist(distributed, recursive = FALSE), universe)
}


#' @export
`!.CnfFormula` = function(x) {
  if (is.logical(x)) {
    return(as.CnfFormula(!unclass(x)))
  }
  universe = attr(x, "universe")
  negated_formulae = lapply(unclass(x), function(clause) {
    CnfFormula(lapply(names(clause), function(sym) {
      # !CnfAtom(universe[[sym]], clause[[sym]])
      structure(
        list(setdiff(universe[[sym]], clause[[sym]])),
        names = sym,
        universe = universe,
        class = "CnfClause"
      )
    }))
  })
  # can't Reduce(`|`, negated_formulae) here, since for this we'd have to register the `|` S3 method
  Reduce(function(x, y) x | y, negated_formulae)
}
