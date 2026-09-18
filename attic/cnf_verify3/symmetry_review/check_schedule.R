# Independent source observer and selector-invariant checks. This sources only
# the unchanged kernel and uses ordinary environments, without API rebuilding.
source("R/CnfFormula_simplify.R")
plain = simplify_cnf
here = "attic/cnf_verify3/symmetry_review"
source_text = paste(readLines("R/CnfFormula_simplify.R"), collapse = "\n")
probe_counts = integer()
trace_events = list()
site_catalog = list()

selector_probe = function(kind, count, bits) {
  probe_counts[[kind]] <<- if (kind %in% names(probe_counts)) probe_counts[[kind]] + 1L else 1L
  if (kind == "unit_hla") {
    stopifnot(count == 1L, sum(bits) <= count)
    if (!any(bits)) probe_counts[["empty_unit_hla"]] <<-
      if ("empty_unit_hla" %in% names(probe_counts)) probe_counts[["empty_unit_hla"]] + 1L else 1L
  } else {
    stopifnot(!is.na(count), count == sum(bits))
  }
  invisible(NULL)
}

insert_after = function(old, new) {
  occurrences = gregexpr(old, source_text, fixed = TRUE)[[1L]]
  stopifnot(length(occurrences) == 1L, occurrences[[1L]] > 0L)
  source_text <<- sub(old, paste(old, new, sep = "\n"), source_text, fixed = TRUE)
}
insert_after("    nu = names(unit)", "    stopifnot(length(nu) == 1L)")
insert_after("    rowsum = not_subset_count[meta_idx, meta_idx_other]",
  "    selector_probe('handler_count', rowsum, is_not_subset_of[[meta_idx]][meta_idx_other, ])")
insert_after("    symbol_to_restrict = colnames(is_not_subset_of[[meta_idx]])[is_not_subset_of[[meta_idx]][meta_idx_other, ]]",
  "    stopifnot(length(symbol_to_restrict) == 1L)")
insert_after("      symbol_target = colnames(is_not_subset_of[[meta_idx_twoend]])[is_not_subset_of[[meta_idx_twoend]][meta_idx_target, ]]",
  "      selector_probe('oneend_two_columns', 2L, is_not_subset_of[[meta_idx_twoend]][meta_idx_target, ])")
insert_after("    symbols_twoend = colnames(is_not_subset_of[[meta_idx]])[inso_symbols_cols]",
  "    selector_probe('twoend_two_columns', 2L, is_not_subset_of[[meta_idx]][meta_idx_target, ])")
insert_after("      symbol = colnames(is_not_subset_of[[meta_idx_other]])[is_not_subset_of[[meta_idx_other]][meta_idx, ]]",
  "      selector_probe('nonunit_hla', not_subset_count_current[[hla_clause_idx]], is_not_subset_of[[meta_idx_other]][meta_idx, ])\n      stopifnot(length(symbol) == 1L)")
insert_after("      symbol = names(is_not_subset_entry)[is_not_subset_entry]",
  "      selector_probe('unit_hla', not_subset_count[[hla_clause_idx]], is_not_subset_entry)\n      stopifnot(length(symbol) <= 1L)")
eval(parse(text = source_text))
probed = simplify_cnf

record = function(site, value = NULL) {
  force(value)
  trace_events[[length(trace_events) + 1L]] <<- list(site, value)
  value
}
site = function(kind, expression) {
  site_catalog[[length(site_catalog) + 1L]] <<- list(kind = kind,
    expression = paste(deparse(expression), collapse = " "))
  length(site_catalog)
}
wrap = function(expression, kind, original = expression) {
  as.call(list(as.name("record"), site(kind, original), expression))
}
block = function(before, after) as.call(list(as.name("{"), before, after))
visit = function(expression) {
  if (!is.call(expression)) return(expression)
  op = as.character(expression[[1L]])
  stopifnot(length(op) == 1L)
  original = expression
  if (op == "function") {
    expression[[3L]] = block(wrap(NULL, "helper_entry", original[[1L]]), visit(expression[[3L]]))
    return(expression)
  }
  # Preserve empty subscripts and formal/default arguments by traversing calls
  # only. No inserted function forces a production argument before its use.
  for (i in seq_along(expression)[-1L]) {
    if (is.call(expression[[i]])) expression[[i]] = visit(expression[[i]])
  }
  if (op == "if") expression[[2L]] = wrap(expression[[2L]], "if", original[[2L]])
  if (op %in% c("&&", "||")) {
    expression[[2L]] = wrap(expression[[2L]], paste0(op, "left"), original[[2L]])
    expression[[3L]] = wrap(expression[[3L]], paste0(op, "right"), original[[3L]])
  }
  if (op == "for") {
    expression[[3L]] = wrap(expression[[3L]], "sequence", original[[3L]])
    expression[[4L]] = block(wrap(expression[[2L]], "iteration"), expression[[4L]])
  }
  if (op == "repeat") expression[[2L]] = block(wrap(NULL, "repeat"), expression[[2L]])
  expression
}
observed = probed
body(observed) = visit(body(probed))

bare = function(result) {
  attr(result, "class") = NULL
  attr(result, "universe") = NULL
  result
}
capture = function(fun, case, universe) {
  warnings = character()
  outcome = tryCatch(withCallingHandlers(list(result = bare(fun(case$clauses, universe))),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), error = function(e) list(error = conditionMessage(e)))
  c(outcome, list(warnings = warnings))
}
run = function(case) {
  universe = list2env(case$domains, parent = emptyenv())
  actual = capture(plain, case, universe)
  trace_events <<- list()
  instrumented = capture(observed, case, universe)
  if (!identical(actual, instrumented)) {
    saveRDS(list(case = case, actual = actual, instrumented = instrumented), file.path(here, "FAILED_probe.rds"))
    stop("Independent probes/observer changed the outcome")
  }
  list(outcome = actual, trace = trace_events)
}
normal = function(clauses) {
  if (is.logical(clauses)) return(clauses)
  lapply(clauses, function(clause) lapply(clause, sort))
}
map_case = function(case, fibers, shuffle = FALSE) {
  map_range = function(range, symbol) {
    result = unlist(fibers[[symbol]][match(range, names(fibers[[symbol]]))], use.names = FALSE)
    if (shuffle && length(result) > 1L) result = result[sample.int(length(result))]
    result
  }
  domains = lapply(names(case$domains), function(s) map_range(case$domains[[s]], s))
  names(domains) = names(case$domains)
  clauses = case$clauses
  if (is.list(clauses)) clauses = lapply(clauses, function(clause) {
    for (i in seq_along(clause)) clause[[i]] = map_range(clause[[i]], names(clause)[[i]])
    clause
  })
  list(domains = domains, clauses = clauses)
}
project = function(clauses, fibers) {
  if (is.logical(clauses)) return(clauses)
  lapply(clauses, function(clause) {
    for (i in seq_along(clause)) {
      map = fibers[[names(clause)[[i]]]]
      range = clause[[i]]
      stopifnot(!anyDuplicated(range), all(range %in% unlist(map, use.names = FALSE)))
      intersects = vapply(map, function(fiber) any(fiber %in% range), FALSE)
      included = vapply(map, function(fiber) all(fiber %in% range), FALSE)
      stopifnot(identical(intersects, included))
      clause[[i]] = sort(names(map)[included])
    }
    clause
  })
}
quotient = function(case) {
  maps = list()
  for (symbol in names(case$domains)) {
    occurrences = list()
    for (clause in case$clauses) {
      for (i in which(names(clause) == symbol)) occurrences[[length(occurrences) + 1L]] = clause[[i]]
    }
    signature = vapply(case$domains[[symbol]], function(value) {
      paste(vapply(occurrences, function(range) as.integer(value %in% range), 0L), collapse = "")
    }, "")
    cells = unique(signature)
    map = lapply(cells, function(key) case$domains[[symbol]][signature == key])
    names(map) = paste0("c", seq_along(map))
    maps[[symbol]] = map
  }
  list(case = list(domains = lapply(maps, names), clauses = project(case$clauses, maps)), fibers = maps)
}
make_fibers = function(case, split) {
  lapply(case$domains, function(domain) {
    result = lapply(seq_along(domain), function(i) {
      paste0("renamed_", i, "_", seq_len(if (split) c(1L, 5L, 2L, 3L)[[1L + i %% 4L]] else 1L))
    })
    setNames(result, domain)
  })
}

set.seed(906202692L)
cases = list(
  list(domains = list(X = letters[1:3]), clauses = TRUE),
  list(domains = list(X = letters[1:3]), clauses = FALSE),
  list(domains = list(X = letters[1:3]), clauses = list()),
  list(domains = list(X = letters[1:3]),
    clauses = list(setNames(list("a", "a"), c("X", "X")), list(X = c("a", "b")))),
  list(domains = list(V1 = paste0("q", 1:4), V2 = paste0("q", 1:3)),
    clauses = list(list(V1 = "q1", V2 = "q1"), list(V2 = "q1", V1 = "q2"),
      list(V1 = "q1", V2 = "q3"), list(V1 = "q3", V2 = c("q3", "q1")))))
for (case_id in seq_len(1800L)) {
  n_symbols = sample.int(4L, 1L)
  symbols = paste0("S", seq_len(n_symbols))
  domains = setNames(lapply(symbols, function(s) letters[seq_len(1L + sample.int(5L, 1L))]), symbols)
  clauses = lapply(seq_len(sample.int(9L, 1L)), function(i) {
    if (case_id %% 2L == 0L) {
      selected = symbols[sample.int(n_symbols, sample.int(6L, 1L), replace = TRUE)]
    } else {
      selected = symbols[sample.int(n_symbols, sample.int(n_symbols, 1L))]
    }
    setNames(lapply(selected, function(symbol) {
      domain = domains[[symbol]]
      domain[sample.int(length(domain), sample.int(length(domain) - 1L, 1L))]
    }), selected)
  })
  cases[[length(cases) + 1L]] = list(domains = domains, clauses = clauses)
}

stats = c(inputs = 0L, duplicate_inputs = 0L, error_inputs = 0L, transformations = 0L, observed_events = 0L)
errors = list()
for (case_id in seq_along(cases)) {
  case = cases[[case_id]]
  original = run(case)
  stats[["inputs"]] = stats[["inputs"]] + 1L
  has_duplicates = any(vapply(case$clauses, function(clause) anyDuplicated(names(clause)) > 0L, FALSE))
  stats[["duplicate_inputs"]] = stats[["duplicate_inputs"]] + has_duplicates
  if (!is.null(original$outcome$error)) {
    stopifnot(has_duplicates)
    stats[["error_inputs"]] = stats[["error_inputs"]] + 1L
    if (length(errors) < 5L) errors[[length(errors) + 1L]] = list(case = case, error = original$outcome$error)
  }
  split_map = make_fibers(case, TRUE)
  rename_map = make_fibers(case, FALSE)
  collapsed = quotient(case)
  domain_order = case
  domain_order$domains = lapply(case$domains, rev)
  variants = list(
    list(kind = "split_permuted", case = map_case(case, split_map, TRUE), map = split_map),
    list(kind = "fixed_blocks", case = map_case(case, split_map), map = split_map),
    list(kind = "renamed", case = map_case(case, rename_map), map = rename_map),
    list(kind = "domain_only", case = domain_order),
    list(kind = "collapsed", case = collapsed$case, map = collapsed$fibers))
  for (variant in variants) {
    transformed = run(variant$case)
    stopifnot(identical(original$trace, transformed$trace),
      identical(original$outcome$error, transformed$outcome$error),
      identical(original$outcome$warnings, transformed$outcome$warnings))
    if (is.null(original$outcome$error)) {
      if (variant$kind == "domain_only") {
        stopifnot(identical(original$outcome$result, transformed$outcome$result))
      } else if (variant$kind == "collapsed") {
        stopifnot(identical(project(original$outcome$result, variant$map), normal(transformed$outcome$result)))
      } else {
        stopifnot(identical(normal(original$outcome$result), project(transformed$outcome$result, variant$map)))
        if (variant$kind %in% c("fixed_blocks", "renamed")) {
          expected = map_case(list(domains = case$domains, clauses = original$outcome$result), variant$map)$clauses
          stopifnot(identical(expected, transformed$outcome$result))
        }
      }
    }
    stats[["transformations"]] = stats[["transformations"]] + 1L
    stats[["observed_events"]] = stats[["observed_events"]] + length(transformed$trace)
  }
  stats[["observed_events"]] = stats[["observed_events"]] + length(original$trace)
  if (case_id %% 300L == 0L) cat("Completed", case_id, "inputs\n")
}
report = list(version = R.version.string, stats = stats, selector_probes = probe_counts,
  source_sites = length(site_catalog), source_site_kinds = table(vapply(site_catalog, `[[`, "", "kind")),
  error_examples = errors,
  source_hash = system2("sha256sum", "R/CnfFormula_simplify.R", stdout = TRUE), success = TRUE)
suffix = if (getRversion() >= "4.6") "r46" else "r36"
saveRDS(report, file.path(here, paste0("schedule_results_", suffix, ".rds")))
dput(report, file.path(here, paste0("schedule_results_", suffix, ".R")))
print(stats)
print(probe_counts)
cat("PASS: independent trace and selector checks\n")
