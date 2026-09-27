# Local observational instrumentation. This copies the production closure and
# changes its body in memory only. No CNF all.equal method is called.
identity_sites = list()
identity_events = list()
identity_record = function(site, value = NULL) {
  force(value)
  identity_events[[length(identity_events) + 1L]] <<- list(site = site, value = value)
  value
}

identity_observer = function(fun) {
  identity_sites <<- list()
  site = function(kind, owner, expr) {
    id = length(identity_sites) + 1L
    identity_sites[[id]] <<- list(id = id, kind = kind, owner = owner,
      expression = paste(deparse(expr, width.cutoff = 500L), collapse = " "))
    id
  }
  wrap = function(expr, kind, owner, original = expr) {
    as.call(list(as.name("identity_record"), site(kind, owner, original), expr))
  }
  prepend = function(expr, statement) {
    if (is.call(expr) && identical(expr[[1L]], as.name("{"))) {
      as.call(c(list(as.name("{"), statement), as.list(expr)[-1L]))
    } else as.call(list(as.name("{"), statement, expr))
  }
  visit = function(expr, owner) {
    if (!is.call(expr)) return(expr)
    original = expr
    op = as.character(expr[[1L]])
    stopifnot(length(op) == 1L)
    if (op %in% c("=", "<-") && length(expr) == 3L && is.call(expr[[3L]]) &&
        identical(expr[[3L]][[1L]], as.name("function"))) {
      expr[[3L]] = visit(expr[[3L]], as.character(expr[[2L]]))
      return(expr)
    }
    if (op == "function") {
      expr[[3L]] = prepend(visit(expr[[3L]], owner),
        as.call(list(as.name("identity_record"), site("helper", owner, as.name(owner)))))
      return(expr)
    }
    if (op == "for") {
      expr[[3L]] = wrap(visit(expr[[3L]], owner), "for_sequence", owner, original[[3L]])
      expr[[4L]] = prepend(visit(expr[[4L]], owner), wrap(expr[[2L]], "for_iteration", owner))
      return(expr)
    }
    if (op == "repeat") {
      expr[[2L]] = prepend(visit(expr[[2L]], owner),
        as.call(list(as.name("identity_record"), site("repeat", owner, original[[1L]]))))
      return(expr)
    }
    for (i in seq_along(expr)[-1L]) if (is.call(expr[[i]])) expr[[i]] = visit(expr[[i]], owner)
    if (op == "if") expr[[2L]] = wrap(expr[[2L]], "if", owner, original[[2L]])
    if (op %in% c("&&", "||")) {
      expr[[2L]] = wrap(expr[[2L]], paste0(op, "_left"), owner, original[[2L]])
      expr[[3L]] = wrap(expr[[3L]], paste0(op, "_right"), owner, original[[3L]])
    }
    if (op %in% c("all", "any", "==", "!=", ">", ">=", "<", "<=")) expr = wrap(expr, op, owner, original)
    expr
  }
  result = fun
  body(result) = visit(body(fun), "simplify_cnf")
  result
}

# Exact positional comparison: replace a scalar by its code-point sequence,
# preserving all list order, names, range order, multiplicities and attributes
# except the deliberately removed outer CNF universe/class attributes.
identity_strings = function(x) unname(vapply(enc2utf8(x), function(s) paste(utf8ToInt(s), collapse = ":"), ""))
identity_tree = function(x) {
  at = attributes(x)
  if (is.character(x)) x = identity_strings(x)
  else if (is.list(x)) x = lapply(x, identity_tree)
  if (!is.null(at$names)) at$names = identity_strings(at$names)
  if (!is.null(at$dimnames)) at$dimnames = lapply(at$dimnames, function(n) if (is.null(n)) NULL else identity_strings(n))
  attributes(x) = at
  x
}
identity_bare = function(x) {
  attr(x, "class") = NULL
  attr(x, "universe") = NULL
  x
}
identity_payload = function(x) identity_tree(identity_bare(x))

identity_encode = function(values, profile, offset = 0L) {
  values = enc2utf8(values)
  latin = iconv(values, from = "UTF-8", to = "latin1")
  latin[is.na(latin)] = values[is.na(latin)]
  native = enc2native(values)
  Encoding(native) = "unknown"
  if (profile == "utf8") return(values)
  if (profile == "latin1") return(latin)
  if (profile == "native") return(native)
  stopifnot(profile == "mixed")
  for (i in seq_along(values)) values[[i]] = list(values, latin, native)[[(i + offset) %% 3L + 1L]][[i]]
  values
}

# The truth oracle uses abstract integer IDs, preserving distinct Unicode
# spellings. It has no CNF helper dependency and does not compare by collation.
identity_eval_spec = function(clauses, assignments) {
  result = rep(TRUE, nrow(assignments))
  for (clause in clauses) {
    disjunction = rep(FALSE, nrow(assignments))
    for (symbol in names(clause)) {
      symbol_id = as.integer(sub("s", "", symbol, fixed = TRUE))
      disjunction = disjunction | assignments[, symbol_id] %in% clause[[symbol]]
    }
    result = result & disjunction
  }
  result
}
identity_eval_output = function(output, assignments, symbol_names, values) {
  output = identity_bare(output)
  if (is.logical(output)) return(rep(output, nrow(assignments)))
  names_id = identity_strings(symbol_names)
  values_id = identity_strings(values)
  result = rep(TRUE, nrow(assignments))
  for (clause in output) {
    disjunction = rep(FALSE, nrow(assignments))
    for (i in seq_along(clause)) {
      symbol = match(identity_strings(names(clause)[[i]]), names_id)
      range = match(identity_strings(clause[[i]]), values_id)
      stopifnot(!is.na(symbol), !anyNA(range), !anyDuplicated(range))
      disjunction = disjunction | assignments[, symbol] %in% range
    }
    result = result & disjunction
  }
  result
}
