# Independent observer: only production source is imported.
review_dir = "attic/cnf_verify3/normalization_component_review"
source("R/CnfFormula_simplify.R")
review_plain = simplify_cnf

bare_output = function(x) {
  attr(x, "universe") = NULL
  attr(x, "class") = NULL
  x
}

flat_values = function(x) {
  if (is.null(x)) return(character())
  attributes(x) = NULL
  x
}

set_support = function(x) sort(unique(flat_values(x)))

truth = function(formula, assignment) {
  if (is.logical(formula)) return(formula)
  for (clause in formula) {
    satisfied = FALSE
    for (symbol in names(clause)) {
      for (value in clause[[symbol]]) {
        if (assignment[[symbol]] == value) satisfied = TRUE
      }
    }
    if (!satisfied) return(FALSE)
  }
  TRUE
}

make_universe = function(domains) list2env(domains, parent = emptyenv())

instrument = function(fun) {
  sites = list()
  site = function(kind, expr) {
    sites[[length(sites) + 1L]] <<- list(kind = kind, text = paste(deparse(expr), collapse = " "))
    length(sites)
  }
  rec = function(x, helper = "simplify_cnf") {
    if (!is.call(x)) return(x)
    op = as.character(x[[1L]])[[1L]]
    original = x
    if (op == "function") {
      x[[3L]] = rec(x[[3L]], helper)
      x[[3L]] = substitute({.observer$enter(NAME, environment()); BODY},
        list(NAME = helper, BODY = x[[3L]]))
      return(x)
    }
    if (op %in% c("=", "<-", "<<-") && is.call(x[[3L]]) && identical(x[[3L]][[1L]], as.name("function"))) {
      x[[3L]] = rec(x[[3L]], as.character(x[[2L]]))
      return(x)
    }
    if (op == "if") {
      id = site("if", x[[2L]])
      x[[2L]] = substitute(.observer$record(ID, VALUE), list(ID = id, VALUE = rec(x[[2L]], helper)))
      for (i in seq.int(3L, length(x))) x[[i]] = rec(x[[i]], helper)
      return(x)
    }
    if (op == "for") {
      id = site("for", x[[3L]])
      x[[3L]] = substitute(.observer$record(ID, VALUE), list(ID = id, VALUE = rec(x[[3L]], helper)))
      x[[4L]] = rec(x[[4L]], helper)
      return(x)
    }
    if (op %in% c("&&", "||")) {
      for (i in 2:3) {
        id = site(paste0(op, i - 1L), x[[i]])
        x[[i]] = substitute(.observer$record(ID, VALUE), list(ID = id, VALUE = rec(x[[i]], helper)))
      }
      return(x)
    }
    for (i in seq_along(x)[-1L]) {
      if (is.call(x[[i]])) x[[i]] = rec(x[[i]], helper)
    }
    if (op == "<<-" && startsWith(paste(deparse(original[[2L]]), collapse = ""), "entries[[")) {
      return(substitute({EXPR; .observer$write(environment())}, list(EXPR = x)))
    }
    if (op == "=" && identical(original[[2L]], as.name("range_new"))) {
      return(substitute({EXPR; .observer$hla(environment())}, list(EXPR = x)))
    }
    if (op == "=" && identical(original[[2L]], as.name("remaining_entries"))) {
      return(substitute({.observer$boundary(environment()); EXPR}, list(EXPR = x)))
    }
    x
  }
  body(fun) = rec(body(fun))
  list(fun = fun, sites = sites)
}

observed_source = instrument(review_plain)

run_observed = function(entries, domains, groups = NULL, local_ids = NULL, fun = observed_source$fun) {
  original = entries
  if (is.null(groups)) groups = rep("A", length(entries))
  if (is.null(local_ids)) local_ids = seq_along(entries)
  sorted = if (is.logical(entries)) seq_along(entries) else order(lengths(entries))
  ghost = paste(groups, local_ids, sep = ":")[sorted]
  owners = groups[sorted]
  observer = new.env(parent = emptyenv())
  observer$events = list()
  observer$local = setNames(lapply(unique(groups), function(x) list()), unique(groups))
  observer$hla_events = list()
  observer$counts = c(events = 0L, writes = 0L, pair_rows = 0L, unit_containment = 0L,
    nonunit_hla = 0L, unit_hla = 0L, repeated_virtual = 0L, named_virtual = 0L,
    cross_helpers = 0L, metadata_entries = 0L, cross_rows = 0L)
  observer$phase = "pre"
  observer$record = function(id, value) {
    force(value)
    observer$events[[length(observer$events) + 1L]] = list(id, value)
    observer$counts[["events"]] = observer$counts[["events"]] + 1L
    value
  }
  add_local = function(group, event) {
    j = length(observer$local[[group]]) + 1L
    observer$local[[group]][[j]] = event
  }
  observer$enter = function(name, frame) {
    observer$record(paste0("helper:", name), NULL)
    if (name %in% c("char_intersect", "char_setdiff", "char_union", "return_entries")) return(invisible(NULL))
    direct_args = switch(name,
      register_unit = "unit_idx",
      apply_domain_restriction = "clause_idx",
      eliminate_symbol_from_clause = "clause_idx",
      eliminate_clause_update_sr = "clause_idx", character())
    meta_args = switch(name,
      on_updated_subset_relations = c("meta_idx", "meta_idx_other"),
      on_update_range = "meta_idx",
      handle_sse_2nd_order_oneend = c("meta_idx", "meta_idx_target"),
      handle_sse_2nd_order_twoend = c("meta_idx", "meta_idx_target"),
      try_sse_2nd_order = c("meta_idx_oneend", "meta_idx_twoends", "meta_idx_target"), character())
    idx = vapply(direct_args, function(a) get(a, frame, inherits = FALSE), 0L)
    if (length(meta_args)) idx = c(idx, vapply(meta_args,
      function(a) get("available", frame, inherits = TRUE)[[get(a, frame, inherits = FALSE)]], 0L))
    if (!length(idx)) return(invisible(NULL))
    group = unique(owners[idx])
    if (length(group) > 1L) {
      stopifnot(name %in% c("on_updated_subset_relations", "handle_sse_2nd_order_twoend"))
      observer$counts[["cross_helpers"]] = observer$counts[["cross_helpers"]] + 1L
      source = get("entries", frame, inherits = TRUE)[[idx[[1L]]]]
      target = get("entries", frame, inherits = TRUE)[[idx[[2L]]]]
      stopifnot(length(source) >= 2L, !any(names(source) %in% names(target)))
      return(invisible(NULL))
    }
    args = list(name = name, identities = unname(ghost[idx]))
    for (arg in c("symbol", "symbol_intersect", "symbol_target", "second_order_only", "is_unit_propagation", "restringent")) {
      if (exists(arg, frame, inherits = FALSE)) args[arg] = list(get(arg, frame, inherits = FALSE))
    }
    # Project the current global initialization bound onto this component.
    if (observer$phase == "pre" && !is.null(get("is_not_subset_of", frame, inherits = TRUE))) {
      av = get("available", frame, inherits = TRUE)
      bound = get("meta_idx_outer", frame, inherits = TRUE)
      args$initialized_prefix = sum(owners[av[seq_len(bound)]] == group)
      observer$counts[["metadata_entries"]] = observer$counts[["metadata_entries"]] + 1L
    }
    add_local(group, args)
    invisible(NULL)
  }
  observer$write = function(frame) {
    entries_now = get("entries", frame, inherits = TRUE)
    idx = if (exists("ur", frame, inherits = FALSE)) get("ur", frame) else get("clause_idx", frame)
    observer$record("write", list(ghost[[idx]], entries_now[[idx]]))
    observer$counts[["writes"]] = observer$counts[["writes"]] + 1L
    add_local(owners[[idx]], list(name = "write", identity = ghost[[idx]], clause = entries_now[[idx]]))
    invisible(NULL)
  }
  observer$boundary = function(frame) {
    observer$phase = "hla"
    entries_now = get("entries", frame)
    live = which(!get("eliminated", frame) & !get("is_unit", frame))
    av = get("available", frame)
    inv = get("available_inverse", frame)
    bits = get("is_not_subset_of", frame)
    counts = get("not_subset_count", frame)
    for (a in live) {
      for (b in live[live != a]) {
        row = bits[[inv[[a]]]][inv[[b]], ]
        expected = vapply(names(row), function(s) {
          !all(entries_now[[a]][[s]] %in% entries_now[[b]][[s]])
        }, TRUE)
        stopifnot(identical(unname(row), unname(expected)), counts[inv[[a]], inv[[b]]] == sum(expected))
        observer$counts[["pair_rows"]] = observer$counts[["pair_rows"]] + 1L
        if (owners[[a]] != owners[[b]]) {
          stopifnot(sum(row) == length(entries_now[[a]]))
          observer$counts[["cross_rows"]] = observer$counts[["cross_rows"]] + 1L
        }
      }
      units = get("unit_domains", frame)
      for (s in intersect(names(entries_now[[a]]), ls(units, all.names = TRUE))) {
        stopifnot(all(entries_now[[a]][[s]] %in% units[[s]]))
        observer$counts[["unit_containment"]] = observer$counts[["unit_containment"]] + 1L
      }
    }
    invisible(NULL)
  }
  observer$hla = function(frame) {
    symbol = get("symbol", frame)
    target = get("clause", frame)
    idx = get("clause_idx", frame)
    donor_idx = get("clause_idx_other", frame)
    donor = get("entries", frame)[[donor_idx]]
    old = get("range_old", frame)
    new = get("range_new", frame)
    stored = get("universe", frame)[[symbol]]
    values = unique(flat_values(stored))
    exceptions = names(donor)[vapply(names(donor), function(s) !all(donor[[s]] %in% target[[s]]), TRUE)]
    missing = donor[[symbol]][!donor[[symbol]] %in% old]
    capacity = vapply(values, function(v) sum(flat_values(stored) == v), 0L)
    old_count = vapply(values, function(v) sum(flat_values(old) == v), 0L)
    new_count = vapply(values, function(v) sum(flat_values(new) == v), 0L)
    predicted = ifelse(old_count > 0L, old_count, ifelse(values %in% donor[[symbol]], 0L, capacity))
    stopifnot(identical(exceptions, symbol), length(missing) > 0L,
      !any(missing %in% new), all(old_count <= capacity), all(new_count <= capacity),
      all(new_count == predicted), all(new %in% values), length(new) < length(stored),
      owners[[idx]] == owners[[donor_idx]])
    kind = if (get("is_unit", frame)[[idx]]) "unit_hla" else "nonunit_hla"
    observer$counts[[kind]] = observer$counts[[kind]] + 1L
    observer$counts[["repeated_virtual"]] = observer$counts[["repeated_virtual"]] + (anyDuplicated(flat_values(new)) > 0L)
    observer$counts[["named_virtual"]] = observer$counts[["named_virtual"]] + !is.null(names(new))
    event = list(kind = kind, target = ghost[[idx]], donor = ghost[[donor_idx]], symbol = symbol,
      old = set_support(old), new = set_support(new), missing = set_support(missing))
    observer$hla_events[[length(observer$hla_events) + 1L]] = event
    add_local(owners[[idx]], event)
    invisible(NULL)
  }
  run_env = new.env(parent = environment(fun))
  run_env$.observer = observer
  environment(fun) = run_env
  output = bare_output(fun(entries, make_universe(domains)))
  plain = bare_output(review_plain(original, make_universe(domains)))
  stopifnot(identical(output, plain))
  list(output = output, events = observer$events, local = observer$local,
    hla = observer$hla_events, counts = observer$counts)
}
