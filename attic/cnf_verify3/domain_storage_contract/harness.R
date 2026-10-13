# Independent domain-storage audit. Source only unchanged production R files.
suppressPackageStartupMessages(library(checkmate))
stopf = function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)
map_chr = function(.x, .f, ...) vapply(.x, .f, character(1), ...)
for (name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(name, ".R")))
}
domain_plain = simplify_cnf
domain_here = "attic/cnf_verify3/domain_storage_contract"
domain_bare = function(x) {
  attr(x, "universe") = NULL
  attr(x, "class") = NULL
  x
}
domain_flat = function(x) {
  attributes(x) = NULL
  x
}
domain_support = function(x) unique(domain_flat(x))

domain_observer = new.env(parent = emptyenv())
domain_reset = function() {
  domain_observer$trace = new.env(parent = emptyenv(), hash = TRUE)
  domain_observer$trace_length = 0L
  domain_observer$hla = list()
  domain_observer$entry = NULL
}
domain_record = function(site, value = NULL) {
  force(value)
  domain_observer$trace_length = domain_observer$trace_length + 1L
  domain_observer$trace[[as.character(domain_observer$trace_length)]] = list(site, value)
  value
}
domain_take_trace = function() {
  unname(mget(as.character(seq_len(domain_observer$trace_length)), domain_observer$trace, inherits = FALSE))
}

# The HLA checks use a positional, freshly recomputed membership relation,
# independently of the source's cached rows or the canonical paired execution.
domain_hla = function(kind, frame) {
  symbol = get("symbol", frame)
  target = get("clause", frame)
  entries = get("entries", frame, inherits = TRUE)
  donor = entries[[get("clause_idx_other", frame)]]
  stored = get("universe", frame)[[symbol]]
  flat = domain_flat(stored)
  keys = unique(flat)
  old = get("range_old", frame)
  new = get("range_new", frame)
  if (kind == "nonunit") {
    bits = get("is_not_subset_of", frame)[[get("meta_idx_other", frame)]][get("meta_idx", frame), ]
    count = get("not_subset_count_current", frame)[[get("hla_clause_idx", frame)]]
  } else {
    bits = get("is_not_subset_entry", frame)
    count = get("not_subset_count", frame)[[get("hla_clause_idx", frame)]]
  }
  expected = vapply(names(bits), function(s) !all(donor[[s]] %in% target[[s]]), FALSE)
  missing = donor[[symbol]][!donor[[symbol]] %in% old]
  multiplicity = function(x) tabulate(match(domain_flat(x), keys), nbins = length(keys))
  stopifnot(length(symbol) == 1L, length(missing) > 0L,
    identical(unname(bits), unname(expected)), sum(bits) == 1L, count == 1L,
    all(missing %in% flat), !any(missing %in% new), all(new %in% flat),
    all(multiplicity(old) <= multiplicity(flat)), all(multiplicity(new) <= multiplicity(flat)),
    length(new) < length(flat), !anyNA(new), is.null(dim(new)))
  support_expected = unique(c(domain_support(old), keys[!keys %in% c(old, donor[[symbol]])]))
  stopifnot(setequal(domain_support(new), support_expected))
  record = list(kind = kind, clause_idx = get("clause_idx", frame),
    donor_idx = get("clause_idx_other", frame), symbol = symbol,
    domain = stored, old = old, new = new, donor = donor[[symbol]],
    missing = missing, named = !is.null(names(new)), duplicate = anyDuplicated(new) > 0L)
  domain_observer$hla[[length(domain_observer$hla) + 1L]] = record
  invisible(NULL)
}

# Before any virtual mutation, require raw-exact live pair rows and counts,
# and physical (nonstrict) containment inside each final unit.
domain_hla_entry = function(frame) {
  entries = get("entries", frame)
  alive = which(!get("eliminated", frame))
  units = get("unit_domains", frame)
  nonunits = alive[!get("is_unit", frame)[alive]]
  inv = get("available_inverse", frame)
  rows = get("is_not_subset_of", frame)
  counts = get("not_subset_count", frame)
  comparisons = 0L
  unit_checks = 0L
  for (a in nonunits) {
    for (s in names(units)) {
      if (s %in% names(entries[[a]])) {
        stopifnot(all(entries[[a]][[s]] %in% units[[s]]))
        unit_checks = unit_checks + 1L
      }
    }
    for (b in nonunits[nonunits != a]) {
      bits = rows[[inv[[a]]]][inv[[b]], ]
      expected = vapply(names(bits), function(s) !all(entries[[a]][[s]] %in% entries[[b]][[s]]), FALSE)
      stopifnot(identical(unname(bits), unname(expected)), counts[inv[[a]], inv[[b]]] == sum(expected))
      comparisons = comparisons + 1L
    }
  }
  domain_observer$entry = c(pair_rows = comparisons, unit_containments = unit_checks)
  invisible(NULL)
}

# A separately written AST pass catalogs all conditions, scalar short circuit
# operands, structural loop sequences/iterations, helper entries, and actual
# range writes. Values in these events are compared by identical(), not hashes.
domain_instrument = function(fun) {
  sites = list()
  add = function(kind, owner, expr) {
    n = length(sites) + 1L
    sites[[n]] <<- list(id = n, kind = kind, owner = owner,
      expression = paste(deparse(expr, width.cutoff = 500L), collapse = " "))
    n
  }
  record = function(expr, kind, owner, original = expr) {
    as.call(list(as.name("domain_record"), add(kind, owner, original), expr))
  }
  prepend = function(expr, before) as.call(list(as.name("{"), before, expr))
  root_name = function(expr) {
    if (is.symbol(expr)) return(as.character(expr))
    if (is.call(expr) && as.character(expr[[1L]]) %in% c("[[", "[", "$")) return(root_name(expr[[2L]]))
    ""
  }
  walk = function(expr, owner) {
    if (!is.call(expr)) return(expr)
    original = expr
    op = as.character(expr[[1L]])
    stopifnot(length(op) == 1L)
    if (op == "function") {
      expr[[3L]] = prepend(walk(expr[[3L]], owner), record(NULL, "helper_entry", owner))
      return(expr)
    }
    if (op %in% c("=", "<-") && is.call(expr[[3L]]) && identical(expr[[3L]][[1L]], as.name("function"))) {
      expr[[3L]] = walk(expr[[3L]], as.character(expr[[2L]]))
      return(expr)
    }
    if (op == "for") {
      expr[[3L]] = record(walk(expr[[3L]], owner), "for_sequence", owner, original[[3L]])
      expr[[4L]] = prepend(walk(expr[[4L]], owner), record(expr[[2L]], "for_iteration", owner))
      return(expr)
    }
    if (op == "repeat") {
      expr[[2L]] = prepend(walk(expr[[2L]], owner), record(NULL, "repeat_iteration", owner))
      return(expr)
    }
    # Recurse only through executable expressions, retaining missing subscripts.
    for (i in seq_along(expr)[-1L]) if (is.call(expr[[i]])) expr[[i]] = walk(expr[[i]], owner)
    if (op == "if") {
      expr[[2L]] = record(expr[[2L]], "if", owner, original[[2L]])
      if (identical(original[[2L]], quote(length(range_new) == length(universe[[symbol]])))) {
        # The two identical source predicates are distinguished by traversal order.
        kind = if (sum(vapply(sites, function(s) s$kind == "hla_hook", FALSE)) == 0L) "nonunit" else "unit"
        add("hla_hook", owner, original[[2L]])
        expr = prepend(expr, as.call(list(as.name("domain_hla"), kind, quote(environment()))))
      }
    }
    if (op %in% c("&&", "||")) {
      expr[[2L]] = record(expr[[2L]], paste0(op, "_left"), owner, original[[2L]])
      expr[[3L]] = record(expr[[3L]], paste0(op, "_right"), owner, original[[3L]])
    }
    if (op %in% c("all", "any", "==", "!=", "<", "<=", ">", ">=")) {
      expr = record(expr, paste0("decision_", op), owner, original)
    }
    if (op %in% c("=", "<<-", "<-")) {
      if (root_name(original[[2L]]) == "entries") expr = record(expr, "actual_write", owner, original[[2L]])
      if (identical(original[[2L]], as.name("remaining_entries"))) {
        expr = prepend(expr, quote(domain_hla_entry(environment())))
      }
    }
    expr
  }
  observed = fun
  body(observed) = walk(body(fun), "simplify_cnf")
  list(fun = observed, sites = sites)
}

domain_build = function(clauses, domains) {
  universe = CnfUniverse()
  for (s in names(domains)) CnfSymbol(universe, s, domains[[s]])
  if (is.logical(clauses) || !length(clauses)) return(list(universe = universe, clauses = NULL))
  public = lapply(clauses, function(clause) CnfClause(lapply(seq_along(clause), function(i) {
    CnfAtom(`$.CnfUniverse`(universe, names(clause)[[i]]), clause[[i]])
  })))
  stopifnot(identical(lapply(public, domain_bare), clauses))
  list(universe = universe, clauses = public)
}

domain_truth = function(entries, assignment) {
  if (is.logical(entries)) return(as.vector(entries))
  all(vapply(entries, function(clause) any(vapply(seq_along(clause), function(i) {
    value = assignment[[names(clause)[[i]]]]
    any(vapply(domain_flat(clause[[i]]), function(v) identical(value, v), FALSE))
  }, FALSE)), FALSE))
}

domain_run = function(case, domains, observed) {
  built = domain_build(case$clauses, domains)
  baseline = domain_plain(case$clauses, built$universe)
  if (!is.null(built$clauses)) stopifnot(identical(CnfFormula(built$clauses), baseline))
  domain_reset()
  result = observed(case$clauses, built$universe)
  stopifnot(identical(result, baseline), identical(attr(result, "universe"), built$universe),
    identical(lapply(names(domains), function(s) built$universe[[s]]), unname(domains)))
  list(output = domain_bare(result), trace = domain_take_trace(),
    hla = domain_observer$hla, entry = domain_observer$entry)
}

domain_variants = function(domains) {
  labels = c("plain", "reordered", "repeated", "named", "repeated_named", "matrix", "array",
    "repeated_matrix", "repeated_array", "attributed", "inert_class")
  out = lapply(labels, function(kind) lapply(domains, function(x) {
    x = domain_support(x)
    if (kind == "reordered") x = rev(x)
    if (kind %in% c("repeated", "repeated_named", "repeated_matrix", "repeated_array", "attributed", "inert_class")) {
      x = rev(rep(x, 1L + seq_along(x) %% 4L))
    }
    if (kind %in% c("matrix", "repeated_matrix")) {
      if (kind == "repeated_matrix") x = rep(x, 2L)
      x = matrix(x, nrow = if (kind == "matrix") 1L else 2L)
      dimnames(x) = lapply(dim(x), function(n) rep("same", n))
    }
    if (kind %in% c("array", "repeated_array")) {
      x = array(x, dim = c(length(x), 1L, 1L))
      dimnames(x) = list(rep("", length(x)), "first", "second")
    }
    if (kind %in% c("named", "repeated_named", "repeated_matrix", "repeated_array", "attributed")) {
      names(x) = rep(c(NA_character_, "", "same", "other"), length.out = length(x))
    }
    if (kind %in% c("attributed", "repeated_matrix", "repeated_array")) {
      attr(x, "audit_tag") = list(text = "inert", values = c(1, 3, 7))
      comment(x) = "Domain storage metadata"
    }
    if (kind == "inert_class") class(x) = "cnf_storage_no_methods_exist"
    x
  }))
  names(out) = labels
  out
}
