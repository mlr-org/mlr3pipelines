
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



#' @title Symbol Table for CNF Formulas
#'
#' @description
#' A symbol table for CNF formulas. The `CnfUniverse` is a by-reference object
#' that stores the domain of each symbol. Symbols are created with [`CnfSymbol()`]
#' and can be retrieved with `[[` or `$`.
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
#' X %among% c("a", "b")
#' X %among% "a"
#' X %among% character(0)
#' X %among% c("a", "b", "c")
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

# We allow retrieving symbols from the universe by name.
#' @export
`[[.CnfUniverse` = function(universe, name) {
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
`$.CnfUniverse` = `[[.CnfUniverse`

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
  domain = get(symbol, attr(symbol, "universe"))
  assert_subset(values, domain)
  if (all(domain %in% values)) {
    structure(
      TRUE,
      universe = attr(symbol, "universe"),
      class = "CnfAtom"
    )
  } else if (length(values) == 0) {
    structure(
      FALSE,
      universe = attr(symbol, "universe"),
      class = "CnfAtom"
    )
  }  else {
    structure(
      list(symbol = c(symbol), values = unique(values)),
      universe = attr(symbol, "universe"),
      class = "CnfAtom"
    )
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
#' Upon construction, the `CnfClause` is simplified by (1) removing contradictions, (2) unifying
#' atoms that refer to the same symbol, and (3) evaluating to `TRUE` if any atom is `TRUE`.
#' Note that the order of atoms in a clause is not preserved.
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
#' @param atoms (`list` of [`CnfAtom`]) \cr
#'   A list of [`CnfAtom`] objects. The clause represents the disjunction of these atoms.
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
#' X %among% c("a", "b") | Y %among% c("d", "e")
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
  assert_list(atoms, types = "CnfAtom")
  if (!length(atoms)) {
    return(structure(
      FALSE,
      universe = NULL,
      class = "CnfClause"
    ))
  }
  entries = list()
  universe = attr(atoms[[1]], "universe")
  for (a in atoms) {
    if (!identical(attr(a, "universe"), universe)) {
      stop("All symbols must be in the same universe.")
    }
    if (isTRUE(a)) {
      entries = TRUE
      break
    }
    if (isFALSE(a)) {
      next
    }
    entries[[a$symbol]] = unique(c(entries[[a$symbol]], a$values))
    if (all(get(a$symbol, universe) %in% entries[[a$symbol]])) {
      entries = TRUE
      break
    }
  }
  structure(
    if (!length(entries)) FALSE else entries,
    universe = universe,
    class = "CnfClause"
  )
}

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
#' Upon construction, the `CnfFormula` is simplified by using various heuristics.
#' This includes unit propagation, subsumption elimination, and hidden tautology elimination
#' (see examples).
#' Note that the order of clauses in a formula is not preserved.
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
#' (X %among% c("a", "b") | Y %among% c("d", "e")) &
#'   Z %among% c("g", "h")
#'
#' # Negation of a formula
#' !(X %among% c("a", "b") | Y %among% c("d", "e")) &
#'   Z %among% c("g", "h")
#'
#' ## unit propagation
#' # The second clause can not be satisfied when X is "b", so "b" it can be
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
#' ## hidden tautology elimination
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
#' contradiction <- (X %among% "a" | Y %among% "d") &
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
  assert_list(clauses, types = "CnfClause")
  if (!length(clauses)) {
    return(structure(
      TRUE,
      universe = NULL,
      class = "CnfFormula"
    ))
  }
  universe = attr(clauses[[1]], "universe")
  entries = list()
  for (c in clauses) {
    if (isFALSE(c)) {
      entries = FALSE
      break
    }
    if (isTRUE(c)) {
      next
    }
    if (!identical(attr(c, "universe"), universe)) {
      # if clauses[[1]] is FALSE, then it is possible that it has no
      # universe; however, in that case we will break before coming here.
      stop("All clauses must be in the same universe.")
    }
    attr(c, "universe") = NULL
    entries[[length(entries) + 1]] = c
  }
  simplify_cnf(entries, universe)
}

simplify_cnf = function(entries, universe) {
  return_false = structure(
    FALSE,
    universe = universe,
    class = "CnfFormula"
  )

  # can we do unit elimination?
  # if we are already TRUE or FALSE no simplification is necessary
  # this only works if there actually are units.
  can_simplify = !is.logical(entries) && any(lengths(entries) == 1)
  # likewise, if there is only one clause left, no simplification is necessary.

  units = list()

  while (can_simplify && length(entries) > 1) {
    # we do the following until we are sure there are no more simplifications to be made.
    # this is the case if we have not meaningfully simplified anything in the last iteration.
    can_simplify = FALSE
    # sort clauses by length, since (1) length-1-clauses are special, and (2) short clauses can only ever subsume longer ones
    entries = entries[order(lengths(entries))]
    # Let sum(A) be the symbols in clause A, and val(A, s) be the values of symbol s admitted in clause A.
    for (i in seq_along(entries)) {
      ei = entries[[i]]
      # If |sym(A)| == 1, sym(A) == {s} and s is in sym(B), then val(B, s) <- intersect(val(A, s), val(B, s)) ("unit propagation")
      # units is a named list of symbols in size-one-clauses, together with their values.
      # We iterate over all symbols in ei that are also in units, and intersect their values.
      for (unit_symbol in intersect(names(ei), names(units))) {
        length_before_ei = length(ei[[unit_symbol]])
        length_before_unit = length(units[[unit_symbol]])
        intersection = intersect(units[[unit_symbol]], ei[[unit_symbol]])
        ei[[unit_symbol]] = intersection
        if (length(ei) == 1 && length(intersection) != length_before_unit) {
          # we made a unit shorter, this means we need to simplify the entry from which units[[unit_symbol]] came
          can_simplify = TRUE
        }
        if (!length(ei[[unit_symbol]])) {
          ei[[unit_symbol]] = NULL
        }
        if (!length(ei)) {
          return(return_false)
        }
        if (length(intersection) != length_before_ei) {
          # need to store changed ei entry only if its length changed; otherwise we know the intersection did not do anything.
          entries[[i]] = ei
        }
      }
      if (length(ei) == 1) {
        # even if names(ei) is already in units, at this point ei[[1]] is the intersection of the values
        units[[names(ei)]] = ei[[1]]
      }
    }
  }
  if (!is.logical(entries) && length(entries) > 1) {
    entries = entries[order(lengths(entries))]  # removing units may have changed the order
    eliminated = logical(length(entries))
    for (i in seq_along(entries)) {
      if (eliminated[[i]]) next  # can only happen if we do the tautology elimination, which searches forward
      ei = entries[[i]]
      # If sym(A) is a subset of sym(B) and for each s in sym(A), val(A, s) is a subset of val(B, s), then A implies B, so B can be removed ("subsumption elimination").
      for (j in seq_len(i - 1)) {
        if (eliminated[[j]]) next
        ej = entries[[j]]
        name_overlap = names(ej) %in% names(ei)
        if (all(name_overlap) && all(sapply(names(ej), function(s) all(ej[[s]] %in% ei[[s]])))) {
          # can't do entries[[i]] = NULL, since we are iterating over entries; the entries[[i]] would break.
          eliminated[[i]] = TRUE
        }
        # simple tautology elimination
        # if s is in sym(A) and sym(B), and val(A, s) and val(B, s) are disjoint, then (A - s | B - s) is implied,
        # and all superset clauses of (A - s | B - s) can be removed.
        # we could also do this for higher order terms, intersection over X of val(X, s) == 0 etc., but this gets more complex.
        # tbh, I am not sure if this is actually worth it
        if (!eliminated[[i]]) {
          which_name_overlap = which(name_overlap)
          # - build the union of values of overlapping symbols
          # - in the innerloop we will check that most of this is a subset of any other clause
          # - "most of" here means: all but the one symbol s where the values are disjoint
          # - use delayedAssign to avoid computation if there is no overlap with empty intersect
          delayedAssign("cnames", union(names(ei), names(ej)))
          delayedAssign("cunion", sapply(cnames, function(s2) union(ei[[s2]], ej[[s2]]), simplify = FALSE))
          for (no in which_name_overlap) {
            s = names(ej)[[no]]
            # intersection is not 0 --> try next one
            if (length(intersect(ej[[s]], ei[[s]]))) next
            cnames_s = setdiff(cnames, s)
            # loop down from large to small, since then we can break once we find a clause with insufficient entries
            for (k in rev(seq_along(entries))) {
              if (k == i || k == j) next
              ek = entries[[k]]
              # all of cnames_s must be in ek, otherwise we can't eliminate
              # since we are looping down from large to small ek, we can break once this is not the case
              if (length(ek) < length(cnames) - 1) break
              if (all(cnames_s %in% names(ek)) && all(sapply(cnames_s, function(s2) all(cunion[[s2]] %in% ek[[s2]])))) {
                eliminated[[k]] = TRUE
              }
            }
          }
        }
      }
    }
    entries = entries[!eliminated]
  }
  structure(
    if (!length(entries)) TRUE else entries,
    universe = universe,
    class = "CnfFormula"
  )
}

#' @export
`&.CnfFormula` = function(e1, e2) {
  e1 = as.CnfFormula(e1)
  e2 = as.CnfFormula(e2)
  if (isTRUE(e1)) return(e2)
  if (isTRUE(e2)) return(e1)
  if (isFALSE(e1)) return(e1)
  if (isFALSE(e2)) return(e2)
  if (!identical(attr(e1, "universe"), attr(e2, "universe"))) {
    stop("Both formulas must be in the same universe.")
  }
  simplify_cnf(c(e1, e2), attr(e1, "universe"))
}

#' @export
`|.CnfFormula` = function(e1, e2) {
  e1 = as.CnfFormula(e1)
  e2 = as.CnfFormula(e2)
  if (isFALSE(e1)) return(e2)
  if (isFALSE(e2)) return(e1)
  if (isTRUE(e1)) return(e1)
  if (isTRUE(e2)) return(e2)
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
  distributed = lapply(e1, function(e1_clause) {
    eliminated = logical(length(e2))
    for (i2 in seq_along(e2)) {
      e2_clause = e2[[i2]]
      for (sym in names(e1_clause)) {
        # add the symbols from e1_clause to e2_clause
        # (if e2_clause does not contain a symbol, it is added)
        e2_clause[[sym]] = union(e1_clause[[sym]], e2_clause[[sym]])
        # faster than test_set_equal
        if (all(get(sym, universe) %in% e2_clause[[sym]])) {
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
`&.CnfClause` = function(e1, e2) {
  if (isFALSE(e1)) return(e1)
  if (isFALSE(e2)) return(e2)
  if (isTRUE(e1)) return(e2)
  if (isTRUE(e2)) return(e1)
  if (inherits(e2, "CnfAtom")) {
    e2 = CnfClause(list(e2))
  }
  if (inherits(e2, "CnfClause")) {
    CnfFormula(list(e1, e2))
  } else {
    # e2 is anything else -- go the lazy route
    as.CnfFormula(e1) & as.CnfFormula(e2)
  }
}

#' @export
`|.CnfClause` = function(e1, e2) {
  if (isFALSE(e1)) return(e2)
  if (isFALSE(e2)) return(e1)
  if (isTRUE(e1)) return(e1)
  if (isTRUE(e2)) return(e2)
  if (inherits(e2, "CnfAtom")) {
    e2 = CnfClause(list(e2))
  }
  if (inherits(e2, "CnfClause")) {
    for (sym in names(e2)) {
      e1[[sym]] = union(e1[[sym]], e2[[sym]])
      if (all(get(sym, attr(e1, "universe")) %in% e1[[sym]])) {
        return(structure(
          TRUE,
          universe = attr(e1, "universe"),
          class = "CnfClause"
        ))
      }
    }
    return(e1)
  }
  # e2 is anything else -- go the lazy route
  as.CnfFormula(e1) | as.CnfFormula(e2)
}

#' @export
`&.CnfAtom` = function(e1, e2) {
  if (isFALSE(e1)) return(e1)
  if (isFALSE(e2)) return(e2)
  if (isTRUE(e1)) return(e2)
  if (isTRUE(e2)) return(e1)
  if (inherits(e2, "CnfAtom")) {
    e2 = CnfClause(list(e2))
  }
  if (inherits(e2, "CnfClause")) {
    CnfFormula(list(CnfClause(list(e1)), e2))
  } else {
    # e2 is anything else -- go the lazy route
    as.CnfFormula(e1) & as.CnfFormula(e2)
  }
}

#' @export
`|.CnfAtom` = function(e1, e2) {
  if (isFALSE(e1)) return(e2)
  if (isFALSE(e2)) return(e1)
  if (isTRUE(e1)) return(e1)
  if (isTRUE(e2)) return(e2)
  if (inherits(e2, "CnfAtom")) {
    CnfClause(list(e1, e2))
  } else {
    # e2 is anything else -- go the lazy route
    as.CnfFormula(e1) | as.CnfFormula(e2)
  }
}

#' @export
`!.CnfAtom` = function(x) {
  if (is.logical(x)) {
    not_x = if (x) FALSE else TRUE  # can't use '!' here...
    attributes(not_x) = attributes(x)  # keep class and universe
    return(not_x)
  }
  structure(
    list(symbol = x$symbol, values = setdiff(get(x$symbol, attr(x, "universe")), x$values)),
    universe = attr(x, "universe"),
    class = "CnfAtom"
  )
}

#' @export
`!.CnfClause` = function(x) {
  if (is.logical(x)) {
    return(as.CnfClause(!unclass(x)))
  }
  !as.CnfFormula(x)
}

#' @export
`!.CnfFormula` = function(x) {
  universe = attr(x, "universe")
  negated_formulae = lapply(x, function(clause) {
    CnfFormula(lapply(names(clause), function(sym) {
      # !CnfAtom(universe[[sym]], clause[[sym]])
      structure(
        list(setdiff(get(sym, universe), clause[[sym]])),
        names = sym,
        universe = universe,
        class = "CnfClause"
      )
    }))
  })
  # can't Reduce(`|`, negated_formulae) here, since for this we'd have to register the `|` S3 method
  Reduce(function(x, y) x | y, negated_formulae)
}

#' @rdname CnfFormula
#' @export
as.CnfFormula = function(x) {
  UseMethod("as.CnfFormula")
}

#' @rdname CnfClause
#' @export
as.CnfClause = function(x) {
  UseMethod("as.CnfClause")
}

#' @rdname CnfAtom
#' @export
as.CnfAtom = function(x) {
  UseMethod("as.CnfAtom")
}

#' @rawNamespace if (getRversion() >= "4.3.0") S3method(chooseOpsMethod,CnfFormula)
chooseOpsMethod.CnfFormula <- function(x, y, mx, my, cl, reverse) TRUE
#' @rawNamespace if (getRversion() >= "4.3.0") S3method(chooseOpsMethod,CnfClause)
chooseOpsMethod.CnfClause <- function(x, y, mx, my, cl, reverse) TRUE
#' @rawNamespace if (getRversion() >= "4.3.0") S3method(chooseOpsMethod,CnfAtom)
chooseOpsMethod.CnfAtom <- function(x, y, mx, my, cl, reverse) TRUE

#' @export
as.CnfFormula.logical = function(x) {
  assert_flag(x)
  return(structure(
    x,
    universe = NULL,
    class = "CnfFormula"
  ))
}

#' @export
as.CnfFormula.CnfAtom = function(x) {
  as.CnfFormula(CnfClause(list(x)))
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
as.CnfAtom.logical = function(x) {
  assert_flag(x)
  return(structure(
    x,
    universe = attr(x, "universe"),
    class = "CnfAtom"
  ))
}

#' @export
as.CnfAtom.CnfAtom = function(x) {
  x
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
as.CnfFormula.logical = function(x) {
  assert_flag(x)
  structure(
    x,
    universe = attr(x, "universe"),
    class = "CnfFormula"
  )
}

#' @export
as.CnfAtom.default = function(x) {
  stop("Cannot convert object to CnfAtom.")
}

#' @export
as.CnfClause.default = function(x) {
  stop("Cannot convert object to CnfClause.")
}

#' @export
as.CnfFormula.default = function(x) {
  stop("Cannot convert object to CnfFormula.")
}

#' @export
as.logical.CnfFormula = function(x) {
  if (is.logical(x)) {
    return(unclass(x))
  }
  return(NA)
}

#' @export
as.logical.CnfClause = function(x) {
  if (is.logical(x)) {
    return(unclass(x))
  }
  return(NA)
}

#' @export
as.logical.CnfAtom = function(x) {
  if (is.logical(x)) {
    return(unclass(x))
  }
  return(NA)
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
print.CnfSymbol = function(x, ...) {
  cat(sprintf("CnfSymbol '%s' with domain {%s}.\n", c(x), paste(get(x, attr(x, "universe")), collapse = ", ")))
  invisible(x)
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
    return("CnfAtom(FALSE)")
  } else if (isFALSE(x)) {
    return("CnfAtom(TRUE)")
  } else {
    return(sprintf("CnfAtom(%s ...)", x$symbol))
  }
}

#' @export
print.CnfClause = function(x, ...) {
  if (isTRUE(x)) {
    cat("CnfClause: TRUE\n")
  } else if (isFALSE(x)) {
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
  if (isTRUE(x)) {
    return("CnfClause(TRUE)")
  } else if (isFALSE(x)) {
    return("CnfClause(FALSE)")
  } else {
    return(sprintf("CnfClause(%s)", str_collapse(names(x), sep = "|", n = 2, ellipsis = "..")))
  }
}

#' @export
format.CnfClause = function(x, ...) {
  if (isTRUE(x)) {
    return("<CnfClause T>")
  } else if (isFALSE(x)) {
    return("<CnfClause F>")
  } else {
    return("<CnfClause ..>")
  }
}

#' @export
print.CnfFormula = function(x, ...) {
  if (isTRUE(x)) {
    cat("CnfFormula: TRUE\n")
  } else if (isFALSE(x)) {
    cat("CnfFormula: FALSE\n")
  } else {
    cat("CnfFormula:\n     (")
    clauses = map_chr(x, function(clause) {
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



## to test:
# subsumption
# unit elimination
# unit elimination makes large clause smaller, leading to additional u.e.
# unit elimination makes large clause smaller, leading to subsumption

# branch with totune is swallowed by single unbranch
# branch with totune is swallowed by multiple unbranch
# branches with totune can possibly lead to conflicts, but not always (multiple active inputs to unbranch, or mixed inputs to normal pipeops)
# branches with totune always lead to conflicts (multiple active inputs to unbranch, or mixed inputs to normal pipeops, or choice between both)
# can we recognize that a certain choice would lead to a conflict, making only other choices possible?

# why does this blow up?
#

# profvis::profvis(replicate(3000, { !!(((u$A %among% "T" | u$B %among% "F") & (u$A %among% "F" | u$C %among% "T") & (u$B %among% "T" | u$C %among% "F"))) ; NULL }) -> ., simplify = FALSE)

# This is because we don't eliminate (C | A) & (!C | B) & (B | A) by removing (B | A)
# so, if  clause X and clause Y have a symbol in common where the values are disjoint, the disjunction of the rest is implied?


# This is not right

# !(X %among% c("a", "b") | Y %among% c("d", "e")) &
#   Z %among% c("g", "h")
# CnfFormula:
#      (X ∈ {c})
#    & (Y ∈ {f})
#    & (Z ∈ {g, h})
