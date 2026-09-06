# Observe source-level decisions without changing the production file.
# The observer records helper entry, every if outcome, scalar short-circuit
# operand, all/any result, comparison result, and for/repeat traversal.
# In particular, it does not project away differences in scheduling order.

symmetry_sites = list()
symmetry_trace = list()
symmetry_record = function(id, value = NULL) {
  force(value)
  symmetry_trace[[length(symmetry_trace) + 1L]] <<- list(id = id, value = value)
  value
}

symmetry_instrument = function(fun) {
  symmetry_sites <<- list()
  add_site = function(kind, owner, expr) {
    id = length(symmetry_sites) + 1L
    symmetry_sites[[id]] <<- list(id = id, kind = kind, owner = owner,
      expression = paste(deparse(expr, width.cutoff = 500L), collapse = " "))
    id
  }
  wrap = function(expr, kind, owner, original = expr) {
    as.call(list(as.name("symmetry_record"), add_site(kind, owner, original), expr))
  }
  prepend = function(expr, statement) {
    if (is.call(expr) && identical(expr[[1L]], as.name("{"))) {
      as.call(c(list(as.name("{"), statement), as.list(expr)[-1L]))
    } else {
      as.call(list(as.name("{"), statement, expr))
    }
  }
  visit = function(expr, owner) {
    if (!is.call(expr)) return(expr)
    original = expr
    op = as.character(expr[[1L]])
    if (length(op) != 1L) stop("Unexpected computed call head")
    if (op %in% c("=", "<-") && length(expr) == 3L && is.call(expr[[3L]]) &&
        identical(expr[[3L]][[1L]], as.name("function"))) {
      expr[[3L]] = visit(expr[[3L]], as.character(expr[[2L]]))
      return(expr)
    }
    if (op == "function") {
      # Current helpers have no executable default arguments (source audit).
      expr[[3L]] = prepend(visit(expr[[3L]], owner),
        as.call(list(as.name("symmetry_record"), add_site("enter", owner, as.name(owner)))))
      return(expr)
    }
    if (op == "for") {
      expr[[3L]] = wrap(visit(expr[[3L]], owner), "for_sequence", owner, original[[3L]])
      expr[[4L]] = prepend(visit(expr[[4L]], owner),
        wrap(expr[[2L]], "for_iteration", owner, expr[[2L]]))
      return(expr)
    }
    if (op == "repeat") {
      expr[[2L]] = prepend(visit(expr[[2L]], owner),
        as.call(list(as.name("symmetry_record"), add_site("repeat_iteration", owner, original[[1L]]))))
      return(expr)
    }
    for (i in seq_along(expr)[-1L]) {
      if (is.call(expr[[i]])) expr[[i]] = visit(expr[[i]], owner)
    }
    if (op == "if") expr[[2L]] = wrap(expr[[2L]], "if", owner, original[[2L]])
    if (op %in% c("&&", "||")) {
      expr[[2L]] = wrap(expr[[2L]], paste0(op, "_left"), owner, original[[2L]])
      expr[[3L]] = wrap(expr[[3L]], paste0(op, "_right"), owner, original[[3L]])
    }
    if (op %in% c("all", "any", "==", "!=", ">", ">=", "<", "<=")) {
      expr = wrap(expr, op, owner, original)
    }
    expr
  }
  observed = fun
  body(observed) = visit(body(fun), "simplify_cnf")
  observed
}

symmetry_bare = function(output) {
  attr(output, "universe") = NULL
  attr(output, "class") = NULL
  output
}

symmetry_universe = function(domains) {
  universe = CnfUniverse()
  for (symbol in names(domains)) CnfSymbol(universe, symbol, domains[[symbol]])
  universe
}

# Construct through the public functions, retaining the chosen literal order.
# `$` on CnfUniverse returns a symbol: use the explicit S3 method for dynamic names.
symmetry_public_clauses = function(clauses, universe) {
  lapply(clauses, function(clause) CnfClause(lapply(seq_along(clause), function(i) {
    CnfAtom(`$.CnfUniverse`(universe, names(clause)[[i]]), clause[[i]])
  })))
}

symmetry_run = function(case, observed, check_public = TRUE) {
  universe = symmetry_universe(case$domains)
  baseline = plain_simplify(case$clauses, universe)
  if (check_public && is.list(case$clauses) && length(case$clauses)) {
    public_clauses = symmetry_public_clauses(case$clauses, universe)
    stopifnot(identical(lapply(public_clauses, symmetry_bare), case$clauses),
      identical(CnfFormula(public_clauses), baseline))
  }
  symmetry_trace <<- list()
  result = observed(case$clauses, universe)
  stopifnot(identical(result, baseline))
  list(output = symmetry_bare(result), trace = symmetry_trace)
}

# A map contains one output-label vector per old value, with nonempty,
# pairwise-disjoint fibers. Fibers may be permuted independently in each range.
symmetry_transform = function(case, fibers, reorder = FALSE) {
  transform_range = function(values, symbol) {
    out = unlist(fibers[[symbol]][values], use.names = FALSE)
    if (is.null(out)) out = character()
    if (reorder && length(out) > 1L) out = out[sample.int(length(out))]
    out
  }
  domains = case$domains
  for (symbol in names(domains)) domains[[symbol]] = transform_range(domains[[symbol]], symbol)
  clauses = case$clauses
  if (is.list(clauses)) {
    clauses = lapply(clauses, function(clause) {
      for (i in seq_along(clause)) clause[[i]] = transform_range(clause[[i]], names(clause)[[i]])
      clause
    })
  }
  list(label = case$label, domains = domains, clauses = clauses)
}

symmetry_fibers = function(case, split = FALSE, rename = FALSE) {
  lapply(case$domains, function(domain) {
    out = lapply(seq_along(domain), function(i) {
      if (split) paste0("cell_", i, "_", seq_len(1L + (i %% 4L))) else {
        if (rename) paste0("fresh_", length(domain) - i + 1L, "_", i %% 2L) else domain[[i]]
      }
    })
    setNames(out, domain)
  })
}

symmetry_project = function(output, fibers) {
  if (is.logical(output)) return(output)
  lapply(output, function(clause) {
    for (i in seq_along(clause)) {
      map = fibers[[names(clause)[[i]]]]
      values = clause[[i]]
      # Check saturation first. Merely projecting would conceal partial fibers.
      present = vapply(map, function(fiber) any(fiber %in% values), FALSE)
      stopifnot(all(vapply(map[present], function(fiber) all(fiber %in% values), FALSE)),
        all(values %in% unlist(map, use.names = FALSE)), !anyDuplicated(values))
      clause[[i]] = sort(names(map)[present])
    }
    clause
  })
}

symmetry_quotient = function(case) {
  fibers = lapply(names(case$domains), function(symbol) {
    domain = case$domains[[symbol]]
    occurrences = unlist(lapply(case$clauses, function(clause) {
      unname(clause[names(clause) == symbol])
    }), recursive = FALSE)
    patterns = vapply(domain, function(value) {
      paste0(as.integer(vapply(occurrences, function(range) value %in% range, FALSE)), collapse = "")
    }, "")
    # One representative per nonempty cell, including the all-zero cell.
    cells = unique(patterns)
    out = lapply(cells, function(cell) domain[patterns == cell])
    setNames(out, paste0("q", seq_along(cells)))
  })
  names(fibers) = names(case$domains)
  inverse = lapply(fibers, function(map) {
    setNames(rep(names(map), lengths(map)), unlist(map, use.names = FALSE))
  })
  project_range = function(range, symbol) unique(unname(inverse[[symbol]][range]))
  clauses = lapply(case$clauses, function(clause) {
    for (i in seq_along(clause)) clause[[i]] = project_range(clause[[i]], names(clause)[[i]])
    clause
  })
  list(case = list(label = paste0(case$label, ":quotient"),
    domains = lapply(fibers, names), clauses = clauses), fibers = fibers)
}

symmetry_trace_difference = function(one, two) {
  stopifnot(!identical(one, two))
  n = min(length(one), length(two))
  equal = vapply(seq_len(n), function(i) identical(one[[i]], two[[i]]), FALSE)
  if (all(equal)) n + 1L else which(!equal)[[1L]]
}
