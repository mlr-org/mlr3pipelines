
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
CnfUniverse = function() structure(new.env(parent = emptyenv()), class = "CnfUniverse")

# We represent symbols through a name and a pointer to their universe.
# We only allow operations between symbols that are in the same universe.
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

`$.CnfUniverse` = `[[.CnfUniverse`

# An expression of the form "X %in% {x1, x2, x3}"
CnfAtom = function(symbol, values) {
  assert_class(symbol, "CnfSymbol")
  domain = get(symbol, attr(symbol, "universe"))
  assert_subset(values, domain)
  if (test_set_equal(values, domain)) {
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
`%among%` = CnfAtom

# A clause is a disjunction of atoms.
# X %among% c("a", "b", "c") | Y %among% c("d", "e", "f")
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
    if (test_set_equal(entries[[a$symbol]], get(a$symbol, universe))) {
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

  # if we are already TRUE or FALSE no simplification is necessary
  can_simplify = !is.logical(entries)
  # likewise, if there is only one clause left, no simplification is necessary.

  units = list()

  while (can_simplify && length(entries) > 1) {
    # we do the following until we are sure there are no more simplifications to be made.
    # this is the case if we have not meaningfully simplified anything in the last iteration.
    can_simplify = FALSE
    # sort clauses by length, since (1) length-1-clauses are special, and (2) short clauses can only ever subsume longer ones
    entries = entries[order(lengths(entries))]
    eliminated = logical(length(entries))
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
          can_simplify = TRUE  # could have new subset-relationships now
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
      # If sym(A) is a subset of sym(B) and for each s in sym(A), val(A, s) is a subset of val(B, s), then A implies B, so B can be removed ("subsumption elimination").
      for (j in seq_len(i - 1)) {
        ej = entries[[j]]
        if (all(names(ej) %in% names(ei)) && all(sapply(names(ej), function(s) all(ej[[s]] %in% ei[[s]])))) {
          # can't do entries[[i]] = NULL, since we are iterating over entries; the entries[[i]] would break.
          eliminated[[i]] = TRUE
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
        e2_clause[[sym]] = unique(c(e1_clause[[sym]], e2_clause[[sym]]))
        if (test_set_equal(e2_clause[[sym]], get(sym, universe))) {
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

`&.CnfClause` = function(e1, e2) {
  as.CnfFormula(e1) & as.CnfFormula(e2)
}

`|.CnfClause` = function(e1, e2) {
  as.CnfFormula(e1) | as.CnfFormula(e2)
}

`&.CnfAtom` = function(e1, e2) {
  as.CnfFormula(e1) & as.CnfFormula(e2)
}

`|.CnfAtom` = function(e1, e2) {
  as.CnfFormula(e1) | as.CnfFormula(e2)
}

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

`!.CnfClause` = function(x) {
  !as.CnfFormula(x)
}

`!.CnfFormula` = function(x) {
  universe = attr(x, "universe")
  negated_formulae = lapply(x, function(clause) {
    CnfFormula(lapply(names(clause), function(sym) {
      CnfClause(list(!CnfAtom(universe[[sym]], clause[[sym]])))
    }))
  })
  # can't Reduce(`|`, negated_formulae) here, since for this we'd have to register the `|` S3 method
  Reduce(function(x, y) x | y, negated_formulae)
}

as.CnfFormula = function(x) {
  UseMethod("as.CnfFormula")
}

chooseOpsMethod.CnfFormula <- function(x, y, mx, my, cl, reverse) TRUE
chooseOpsMethod.CnfClause <- function(x, y, mx, my, cl, reverse) TRUE
chooseOpsMethod.CnfAtom <- function(x, y, mx, my, cl, reverse) TRUE

as.CnfFormula.logical = function(x) {
  assert_flag(x)
  return(structure(
    x,
    universe = NULL,
    class = "CnfFormula"
  ))
}

as.CnfFormula.CnfAtom = function(x) {
  as.CnfFormula(CnfClause(list(x)))
}

as.CnfFormula.CnfClause = function(x) {
  CnfFormula(list(x))
}

as.CnfFormula.CnfFormula = function(x) {
  x
}

as.CnfFormula.default = function(x) {
  stop("Cannot convert object to CnfFormula.")
}

as.logical.CnfFormula = function(x) {
  if (isTRUE(x)) {
    return(TRUE)
  }
  if (isFALSE(x)) {
    return(FALSE)
  }
  return(NA)
}

as.logical.CnfClause = function(x) {
  as.logical(as.CnfFormula(x))
}

as.logical.CnfAtom = function(x) {
  as.logical(as.CnfFormula(x))
}

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

print.CnfSymbol = function(x, ...) {
  cat(sprintf("CnfSymbol '%s' with domain {%s}.\n", c(x), paste(get(x, attr(x, "universe")), collapse = ", ")))
  invisible(x)
}

print.CnfAtom = function(x, ...) {
  if (isTRUE(x)) {
    cat("TRUE\n")
  } else if (isFALSE(x)) {
    cat("FALSE\n")
  } else {
    cat(sprintf("%s %%in%% {%s}.\n", x$symbol, paste(x$values, collapse = ", ")))
  }
  invisible(x)
}

print.CnfClause = function(x, ...) {
  if (isTRUE(x)) {
    cat("TRUE\n")
  } else if (isFALSE(x)) {
    cat("FALSE\n")
  } else {
    cat("CnfClause with entries:\n")
    for (sym in names(x)) {
      cat(sprintf("  %s %%in%% {%s}\n", sym, paste(x[[sym]], collapse = ", ")))
    }
  }
  invisible(x)
}

print.CnfFormula = function(x, ...) {
  if (isTRUE(x)) {
    cat("TRUE\n")
  } else if (isFALSE(x)) {
    cat("FALSE\n")
  } else {
    cat("CnfFormula with entries:\n")
    for (i in seq_along(x)) {
      cat(sprintf("  Clause %d:\n", i))
      print(x[[i]])
    }
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
