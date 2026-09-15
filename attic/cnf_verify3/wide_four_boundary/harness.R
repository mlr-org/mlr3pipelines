# Observe core helper transitions and virtual HLA writes. Padding-only column
# scans/registry maintenance are intentionally not erased from the proof, but
# are outside this finite trace: their iteration counts vary with padding.
padding_trace = list()
padding_names = character()
padding_root = NULL

padding_core = function(clauses) {
  if (is.logical(clauses)) return(clauses)
  lapply(clauses, function(clause) clause[!names(clause) %in% padding_names])
}
padding_event = function(kind, owner, frame, top = FALSE) {
  root = if (top) frame else parent.env(frame)
  entries = get("entries", envir = root, inherits = FALSE)
  eliminated = if (exists("eliminated", root, inherits = FALSE)) get("eliminated", root) else rep(FALSE, length(entries))
  if (length(padding_names)) {
    # These public-built families keep every padding literal verbatim.
    for (clause in entries) {
      stopifnot(all(padding_names %in% names(clause)))
      for (symbol in padding_names) stopifnot(identical(clause[[symbol]], "on"))
    }
    stopifnot(all(lengths(padding_core(entries)[!eliminated]) >= 1L))
  }
  core_matrices = NULL
  if (exists("is_not_subset_of", root, inherits = FALSE) && !is.null(get("is_not_subset_of", root))) {
    matrices = get("is_not_subset_of", root)
    counts = get("not_subset_count", root)
    for (i in seq_along(matrices)) {
      if (is.null(matrices[[i]])) next
      initialized = which(!is.na(counts[i, ]))
      if (length(padding_names) && length(initialized)) {
        stopifnot(!any(matrices[[i]][initialized, padding_names, drop = FALSE]))
      }
    }
    core_matrices = lapply(matrices, function(matrix) {
      if (is.null(matrix)) return(NULL)
      matrix[, !colnames(matrix) %in% padding_names, drop = FALSE]
    })
  }
  arguments = list()
  for (name in c("clause_idx", "unit_idx", "symbol", "restringent", "is_unit_propagation", "meta_idx",
      "meta_idx_other", "second_order_only", "meta_idx_target", "meta_idx_oneend", "meta_idx_twoends",
      "symbol_intersect", "symbol_target")) {
    if (exists(name, frame, inherits = FALSE)) arguments[[name]] = get(name, frame, inherits = FALSE)
  }
  structural = list()
  for (name in c("is_unit", "available", "available_inverse", "not_subset_count", "second_order_enabled",
      "second_order_enabled_matrix", "meta_idx_outer")) {
    if (exists(name, root, inherits = FALSE)) structural[[name]] = get(name, root, inherits = FALSE)
  }
  if (length(padding_names)) stopifnot(!any(structural$is_unit))
  units = if (exists("unit_domains", root, inherits = FALSE)) get("unit_domains", root) else NULL
  if (length(padding_names)) stopifnot(!length(units))
  registry = if (exists("symbol_registry", root, inherits = FALSE)) get("symbol_registry", root) else NULL
  core_registry = if (!is.null(registry)) lapply(c("X0", "X1"), function(symbol) registry[[symbol]]) else NULL
  pad_registry = if (length(padding_names) && !is.null(registry)) registry[[padding_names[[1L]]]] else NULL
  if (length(padding_names) && !is.null(registry)) {
    stopifnot(all(vapply(padding_names, function(symbol) identical(registry[[symbol]], pad_registry), FALSE)))
  }
  event = list(kind = kind, owner = owner, arguments = arguments, entries = padding_core(entries),
    eliminated = eliminated, structural = structural, core_matrices = core_matrices,
    unit_domains = if (is.null(units)) NULL else as.list.environment(units, all.names = TRUE),
    core_registry = core_registry, padding_registry = pad_registry)
  padding_trace[[length(padding_trace) + 1L]] <<- event
  invisible(NULL)
}

padding_hla_event = function(frame) {
  symbol = get("symbol", frame)
  stopifnot(!symbol %in% padding_names)
  virtual = get("clause", frame)
  for (pad in padding_names) stopifnot(identical(virtual[[pad]], "on"))
  padding_trace[[length(padding_trace) + 1L]] <<- list(kind = "hla", owner = "simplify_cnf",
    target = get("clause_idx", frame), donor = get("clause_idx_other", frame), symbol = symbol,
    virtual_core = virtual[!names(virtual) %in% padding_names], new_range = get("range_new", frame))
  invisible(NULL)
}

padding_observer = function(fun) {
  handlers = c("register_unit", "apply_domain_restriction", "eliminate_symbol_from_clause",
    "on_updated_subset_relations", "on_update_range", "handle_sse_2nd_order_oneend",
    "handle_sse_2nd_order_twoend", "try_sse_2nd_order", "eliminate_clause_update_sr")
  visit = function(expr) {
    if (!is.call(expr)) return(expr)
    head = as.character(expr[[1L]])
    if (length(head) != 1L) stop("Computed source call head")
    if (head %in% c("=", "<-") && length(expr) == 3L && is.call(expr[[3L]]) &&
        identical(expr[[3L]][[1L]], as.name("function"))) {
      owner = as.character(expr[[2L]])
      if (owner %in% handlers) {
        original = expr[[3L]][[3L]]
        expr[[3L]][[3L]] = substitute({
          padding_event("enter", OWNER, environment())
          on.exit(padding_event("exit", OWNER, environment()), add = TRUE)
          BODY
        }, list(OWNER = owner, BODY = original))
      }
      return(expr)
    }
    for (i in seq_along(expr)[-1L]) if (is.call(expr[[i]])) expr[[i]] = visit(expr[[i]])
    if (head == "=" && identical(expr[[2L]], as.name("range_new"))) {
      return(substitute({EXPR; padding_hla_event(environment())}, list(EXPR = expr)))
    }
    expr
  }
  result = fun
  body(result) = visit(body(result))
  result
}

# Direct ordered donor/target rule scan, independent of the kernel caches.
padding_sse1 = function(clauses) {
  opportunities = list()
  if (is.logical(clauses)) return(opportunities)
  for (donor in seq_along(clauses)) for (target in seq_along(clauses)) {
    if (donor == target) next
    one = clauses[[donor]]
    two = clauses[[target]]
    exceptional = names(one)[vapply(seq_along(one), function(i) {
      any(!one[[i]] %in% two[[names(one)[[i]]]])
    }, FALSE)]
    if (length(exceptional) != 1L) next
    symbol = exceptional[[1L]]
    removed = two[[symbol]][!two[[symbol]] %in% one[[symbol]]]
    if (length(removed)) opportunities[[length(opportunities) + 1L]] = list(
      donor = donor, target = target, symbol = symbol, removed = removed)
  }
  opportunities
}

padding_truth = function(clauses, assignments) {
  if (is.logical(clauses)) return(rep(clauses, nrow(assignments)))
  truth = rep(TRUE, nrow(assignments))
  for (clause in clauses) {
    value = rep(FALSE, nrow(assignments))
    for (i in seq_along(clause)) {
      # Scalar values are tested by ordinary membership in a literal, with
      # an explicit positional assignment column for each known symbol.
      column = match(names(clause)[[i]], names(assignments))
      stopifnot(!is.na(column))
      value = value | assignments[[column]] %in% clause[[i]]
    }
    truth = truth & value
  }
  truth
}
