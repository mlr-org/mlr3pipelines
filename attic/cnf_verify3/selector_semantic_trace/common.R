# Observation-only trace of the unchanged simplifier. Run from repository root.
if (nzchar(Sys.getenv("CNF_REP_RLIB"))) .libPaths(c(Sys.getenv("CNF_REP_RLIB"), .libPaths()))
suppressPackageStartupMessages(library(checkmate))
trace_dir = "attic/cnf_verify3/selector_semantic_trace"
source_paths = file.path("R", paste0(c("CnfUniverse", "CnfSymbol", "CnfAtom",
  "CnfClause", "CnfFormula", "CnfFormula_simplify"), ".R"))

new_cnf_environment = function() {
  env = new.env(parent = globalenv())
  env$stopf = function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)
  env$map_chr = function(.x, .f, ...) vapply(.x, .f, character(1), ...)
  for (path in source_paths) sys.source(path, envir = env)
  env
}

# Each physical position remains a disjunct. The separate projection explicitly
# chooses the first position of each name; it never reads a clause by name.
truth_clause = function(clause, assignments, projection = FALSE) {
  if (is.logical(clause)) return(rep(as.vector(clause), nrow(assignments)))
  answer = rep(FALSE, nrow(assignments))
  positions = seq_along(clause)
  if (projection) positions = positions[!duplicated(names(clause))]
  for (position in positions) {
    symbol = names(clause)[[position]]
    stopifnot(!is.na(symbol), symbol %in% names(assignments))
    for (row in seq_len(nrow(assignments))) {
      answer[[row]] = answer[[row]] || assignments[[symbol]][[row]] %in% clause[[position]]
    }
  }
  answer
}

truth_formula = function(clauses, assignments, projection = FALSE) {
  if (is.logical(clauses)) return(rep(as.vector(clauses), nrow(assignments)))
  answer = rep(TRUE, nrow(assignments))
  for (position in seq_along(clauses)) {
    answer = answer & truth_clause(clauses[[position]], assignments, projection)
  }
  answer
}

bare = function(x) {
  if (is.logical(x)) return(as.vector(x))
  answer = unclass(x)
  attr(answer, "universe") = NULL
  answer
}

make_case = function(spec, cnf, subsetter = cnf$`[.CnfClause`) {
  universe = cnf$CnfUniverse()
  symbols = setNames(lapply(names(spec$domains), function(name) {
    cnf$CnfSymbol(universe, name, spec$domains[[name]])
  }), names(spec$domains))
  clauses = lapply(seq_along(spec$raw), function(i) {
    raw = spec$raw[[i]]
    clause = cnf$CnfClause(lapply(seq_along(raw), function(j) {
      cnf$CnfAtom(symbols[[names(raw)[[j]]]], raw[[j]])
    }))
    subsetter(clause, spec$selectors[[i]])
  })
  list(universe = universe, clauses = clauses)
}

case_specs = list(
  four_clause = list(
    domains = list(X = letters[1:3], Y = letters[1:3]),
    raw = list(list(X = c("b", "c")), list(X = "b", Y = "c"),
      list(X = "c", Y = "b"), list(X = "a", Y = "a")),
    selectors = list(matrix(c(1L, 1L), nrow = 1L), 1:2, 1:2, 1:2)
  ),
  stale_range = list(
    domains = list(X = letters[1:2], Y = letters[1:3]),
    raw = list(list(X = "a", Y = c("b", "c")), list(X = "a", Y = c("c", "a"))),
    selectors = list(2:1, matrix(c(2L, 1L, 2L), nrow = 1L))
  ),
  unit_hla_error = list(
    domains = list(X = letters[1:3]),
    raw = list(list(X = "a"), list(X = c("a", "b"))),
    selectors = list(matrix(c(1L, 1L), nrow = 1L), 1L)
  )
)

word_spec = function(words) {
  symbol_count = max(abs(unlist(words)))
  domains = setNames(rep(list(c("0", "1")), symbol_count), paste0("S", seq_len(symbol_count)))
  raw = lapply(words, function(word) {
    literals = unique(word)
    setNames(lapply(literals, function(literal) as.character(as.integer(literal > 0))),
      paste0("S", abs(literals)))
  })
  selectors = lapply(words, function(word) matrix(match(word, unique(word)), nrow = 1L))
  list(domains = domains, raw = raw, selectors = selectors)
}
case_specs$hla_virtual_control = word_spec(list(c(1L, 2L), c(1L, 3L), c(-3L, 4L)))
case_specs$hla_orphan_deletion_control = word_spec(list(c(-1L, 2L), c(1L, 1L, 2L, 3L),
  c(1L, -2L), c(-2L, -3L)))

attempt = function(expr) tryCatch(list(value = force(expr), error = NULL),
  error = function(e) list(error = conditionMessage(e)))

make_traced_environment = function(assignments) {
  cnf = new_cnf_environment()
  store = new.env(parent = emptyenv())
  store$events = list()
  store$previous = NULL
  cnf$trace_event = function(line, frame, kind = "observe") {
    root = get(".trace_root", frame, inherits = TRUE)
    take = function(env, name) {
      if (exists(name, env, inherits = FALSE)) get(name, env, inherits = FALSE) else NULL
    }
    entries = take(root, "entries")
    eliminated = take(root, "eliminated")
    if (is.null(eliminated)) eliminated = rep(FALSE, length(entries))
    live = if (is.logical(entries)) entries else entries[!eliminated]
    state_names = c("is_unit", "available", "available_inverse", "meta_idx_outer",
      "is_not_subset_of", "not_subset_count", "second_order_enabled",
      "second_order_enabled_matrix", "remaining_nonunit_entries", "remaining_unit_entries")
    state = setNames(lapply(state_names, function(name) take(root, name)), state_names)
    for (name in c("unit_registry", "unit_domains", "symbol_registry")) {
      value = take(root, name)
      state[[name]] = if (is.environment(value)) as.list(value, all.names = TRUE) else value
    }
    locals = c("unit_idx", "unit", "nu", "ur", "unit_isct", "use_inso", "inso_column",
      "s_clause_idx", "s_clause_idx_meta", "clause_idx", "clause", "symbol", "symbol_idx",
      "restringent", "is_unit_propagation", "clause_symbol_length_before", "meta_idx",
      "meta_idx_other", "second_order_only", "rowsum", "symbol_to_restrict",
      "meta_idx_oneend", "meta_idx_twoends", "meta_idx_target", "idx_oneend", "idx_twoends",
      "idx_target", "symbol_intersect", "symbol_target", "clause_oneend", "clause_twoends",
      "symbols_twoend", "symbols_twoend_idx", "symbol_inner", "symbol_other",
      "clause_idx_inner", "meta_idx_inner", "range_inner", "range_outer", "symbol_idx_inner",
      "symbol_idx_outer", "range_old", "range_new", "unitsymbol", "hla_clause_idx",
      "clause_idx_other", "clause_other", "is_not_subset_entry", "not_subset_count_current",
      "is_not_subset_of_unit", "was_used", "remaining_other_entries")
    contexts = list()
    for (stack_frame in sys.frames()) {
      if (identical(stack_frame, root) || identical(parent.env(stack_frame), root)) {
        context = setNames(lapply(locals, function(name) take(stack_frame, name)), locals)
        contexts[[length(contexts) + 1L]] = context[!vapply(context, is.null, logical(1))]
      }
    }
    truth = list(positional = truth_formula(live, assignments),
      projection = truth_formula(live, assignments, TRUE))
    event = list(event = length(store$events) + 1L, line = line, kind = kind,
      entries = entries, eliminated = eliminated, live = live, state = state,
      contexts = contexts, truth = truth)
    event$local = setNames(lapply(locals, function(name) take(frame, name)), locals)
    event$local = event$local[!vapply(event$local, is.null, logical(1))]
    if (kind == "return") event$returned = take(frame, "entries")
    if (!is.null(store$previous)) {
      event$changed = !identical(entries, store$previous$entries) ||
        !identical(eliminated, store$previous$eliminated)
      event$delta = Map(function(before, after) which(before != after), store$previous$truth, truth)
    } else {
      event$changed = FALSE
      event$delta = list(positional = integer(), projection = integer())
    }
    store$events[[event$event]] = event
    store$previous = event
    invisible(NULL)
  }

  lines = readLines("R/CnfFormula_simplify.R")
  # Insert observations after complete original statements. No predicate,
  # original assignment or return expression is replaced.
  after = c(48L, 50L, 51L, 62L, 88L, 92L, 93L, 95L, 101L, 103L, 104L,
    147L, 152L, 167L, 185L, 186L, 188L, 221L, 222L, 237L, 238L, 242L, 244L,
    265L, 266L, 271L, 272L, 297L, 313L, 439L, 451L, 452L, 458L,
    474L, 477L, 527L, 537L, 540L, 560L, 569L, 600L, 603L, 608L, 610L,
    654L, 655L, 656L, 660L, 684L, 688L, 689L, 691L, 693L, 709L, 711L, 722L,
    732L, 738L, 750L, 753L, 755L, 759L, 773L, 774L, 777L, 784L)
  # Line 185 is a multi-line decision's opening brace, so the hook observes
  # only its TRUE body. Other observation points before returns are explicit.
  before = c(467L, 788L)
  generated = character()
  for (i in seq_along(lines)) {
    if (i == 4L) generated = c(generated,
      "  .trace_root = environment()", "  trace_event(3L, environment(), 'entry')")
    if (i %in% before) {
      generated = c(generated, sprintf("  trace_event(%dL, environment(), 'before')", i))
    }
    generated = c(generated, lines[[i]])
    if (i %in% after) {
      generated = c(generated, sprintf("trace_event(%dL, environment())", i))
    }
  }
  # Return observations happen before the original constructor builds the
  # return value, including scalar contradictions.
  return_start = match("  return_entries = function(entries) {", generated)
  generated = append(generated, "    trace_event(34L, environment(), 'return')", after = return_start)
  path = file.path(trace_dir, "instrumented_simplify.R")
  stripped = generated[!grepl("^[[:space:]]*(trace_event\\(|\\.trace_root = environment\\(\\))", generated)]
  stopifnot(identical(stripped, lines))
  writeLines(generated, path)
  sys.source(path, envir = cnf)
  list(cnf = cnf, store = store)
}

private_selector_candidate = function(cnf) {
  text = paste(deparse(cnf$`[.CnfClause`, width.cutoff = 500L), collapse = "\n")
  stopifnot(grepl("i = unclass(i)", text, fixed = TRUE),
    grepl("check_logical(i, len = true_length)", text, fixed = TRUE))
  text = sub("i = unclass(i)", "i = as.vector(unclass(i))", text, fixed = TRUE)
  text = sub("check_logical(i, len = true_length)",
    "check_logical(i, len = true_length, any.missing = FALSE)", text, fixed = TRUE)
  eval(parse(text = text), cnf)
}

local_change = function(before, after, context, assignments) {
  result = list(before = before, after = after, context = context)
  units = context[lengths(context) == 1L]
  result$checks = lapply(c(FALSE, TRUE), function(projection) {
    a = truth_clause(before, assignments, projection)
    b = truth_clause(after, assignments, projection)
    all_live = truth_formula(context, assignments, projection)
    live_units = truth_formula(units, assignments, projection)
    list(raw_delta = which(a != b), unit_context_delta = which((a & live_units) != (b & live_units)),
      live_context_delta = which((a & all_live) != (b & all_live)),
      before_models = which(a & all_live), after_models = which(b & all_live))
  })
  names(result$checks) = c("positional", "projection")
  result
}

analyze_events = function(events, assignments) {
  commits = list()
  proposals = list()
  units = list()
  for (i in seq_along(events)) {
    event = events[[i]]
    if (i > 1L && event$changed && event$line != 48L) {
      previous = events[[i - 1L]]
      changed = which(vapply(seq_along(event$entries), function(j) {
        !identical(event$entries[[j]], previous$entries[[j]]) ||
          event$eliminated[[j]] != previous$eliminated[[j]]
      }, logical(1)))
      for (clause_idx in changed) {
        context = previous$entries[!previous$eliminated & seq_along(previous$entries) != clause_idx]
        before = if (previous$eliminated[[clause_idx]]) TRUE else previous$entries[[clause_idx]]
        after = if (event$eliminated[[clause_idx]]) TRUE else event$entries[[clause_idx]]
        commits[[length(commits) + 1L]] = c(list(event = i, line = event$line, clause_idx = clause_idx),
          local_change(before, after, context, assignments))
      }
    }
    if (event$line %in% c(693L, 755L)) {
      context = event$contexts[[1L]]
      virtual = context$clause
      symbol_position = match(context$symbol, names(virtual))
      if (is.na(symbol_position)) {
        virtual = c(virtual, setNames(list(context$range_new), context$symbol))
      } else {
        virtual[[symbol_position]] = context$range_new
      }
      others = event$entries[!event$eliminated & seq_along(event$entries) != context$clause_idx]
      proposals[[length(proposals) + 1L]] = c(list(event = i, line = event$line,
        clause_idx = context$clause_idx, donor = context$clause_idx_other, symbol = context$symbol),
        local_change(context$clause, virtual, others, assignments))
    }
    if (event$line %in% c(93L, 103L)) {
      symbol = event$local$nu
      unit_idx = event$state$unit_registry[[symbol]]
      clause = setNames(list(event$state$unit_domains[[symbol]]), symbol)
      stopifnot(!event$eliminated[[unit_idx]], identical(event$entries[[unit_idx]], clause))
      units[[length(units) + 1L]] = c(list(event = i, line = event$line, clause_idx = unit_idx),
        local_change(TRUE, clause, event$live, assignments))
    }
  }
  list(commits = commits, proposals = proposals, units = units)
}

run_trace_case = function(spec, candidate = FALSE) {
  assignments = expand.grid(spec$domains, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  selected_raw = Map(function(clause, selector) clause[as.vector(selector)], spec$raw, spec$selectors)
  expected = truth_formula(selected_raw, assignments)
  production = new_cnf_environment()
  original_subset = production$`[.CnfClause`
  subsetter = if (candidate) private_selector_candidate(production) else original_subset
  built = make_case(spec, production, subsetter)
  stopifnot(identical(expected, truth_formula(lapply(built$clauses, bare), assignments)),
    identical(expected, truth_formula(lapply(built$clauses, bare), assignments, TRUE)))
  baseline = attempt(production$CnfFormula(built$clauses))
  traced = make_traced_environment(assignments)
  result = attempt(traced$cnf$CnfFormula(built$clauses))
  stopifnot(identical(result$error, baseline$error))
  if (is.null(result$error)) stopifnot(identical(bare(result$value), bare(baseline$value)))
  events = traced$store$events
  if (candidate) stopifnot(all(vapply(events, function(event) {
    if (is.logical(event$entries)) return(TRUE)
    all(vapply(event$entries, function(clause) !anyNA(names(clause)) && !anyDuplicated(names(clause)), logical(1)))
  }, logical(1))))
  evidence = analyze_events(events, assignments)
  list(spec = spec, assignments = assignments, expected = expected,
    input = lapply(built$clauses, bare), output = if (is.null(result$error)) bare(result$value) else NULL,
    error = result$error, events = events, evidence = evidence,
    production_subset_unchanged = identical(original_subset, production$`[.CnfClause`))
}
