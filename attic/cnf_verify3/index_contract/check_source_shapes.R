# Independent pre-access / pre-condition instrumentation of the exact kernel.
# Base R only; production source is never edited. Run from the repository root.
#
# CNF_SHAPE_TRIALS=3000 Rscript attic/cnf_verify3/index_contract/check_source_shapes.R
# Optional CNF_SHAPE_TAG chooses the result filename suffix.

audit_dir = "attic/cnf_verify3/index_contract"
source_path = "R/CnfFormula_simplify.R"
source(source_path)
plain_simplify = simplify_cnf
parsed = parse(source_path, keep.source = TRUE)
pd = getParseData(parsed, includeText = TRUE)
source_text = readLines(source_path)
audit = new.env(parent = emptyenv())
audit$counts = list()
audit$examples = list()
audit$sites = list()
audit$phase = "startup"
audit$input = NULL
audit$site_counts = integer()

bump = function(key, n = 1L) {
  old = audit$counts[[key]]
  if (is.null(old)) old = 0L
  audit$counts[[key]] = old + n
}

record = function(key) {
  bump(key)
  if (is.null(audit$examples[[key]])) audit$examples[[key]] = audit$input
}

fail = function(message, site = NA_integer_) {
  detail = if (is.na(site)) "" else sprintf(" at source line %s: %s",
    audit$sites[[site]]$line, audit$sites[[site]]$expression)
  stop(paste0(message, detail), call. = FALSE)
}

check = function(ok, message, site = NA_integer_) {
  if (!isTRUE(ok)) fail(message, site)
}

call_key = function(x) paste(deparse(x, width.cutoff = 500L), collapse = " ")
location_map = new.env(parent = emptyenv())
for (i in which(pd$token == "expr")) {
  item = tryCatch(parse(text = pd$text[[i]])[[1L]], error = function(e) NULL)
  if (is.null(item)) next
  if (!is.call(item) || !as.character(item[[1L]]) %in% c("[", "[[", "if", "&&", "||")) next
  key = call_key(item)
  location_map[[key]] = c(location_map[[key]], pd$line1[[i]])
}
for (key in names(location_map)) location_map[[key]] = sort(location_map[[key]])
location_used = new.env(parent = emptyenv())

new_site = function(x, kind) {
  key = call_key(x)
  used = location_used[[key]]
  if (is.null(used)) used = 0L
  locations = location_map[[key]]
  line = if (length(locations) > used) locations[[used + 1L]] else NA_integer_
  location_used[[key]] = used + 1L
  id = length(audit$sites) + 1L
  audit$sites[[id]] = list(id = id, kind = kind, line = line, expression = key)
  id
}

is_missing = function(x) identical(x, quote(expr = ))
is_index_call = function(x) is.call(x) && as.character(x[[1L]]) %in% c("[", "[[")

# Read checks distinguish ordinary absent named-list lookup (defined NULL) from
# absent atomic/matrix names and from the unintended recursive [[vector]] form.
validate_index = function(x, index, extent, label_names, kind, write, site,
    matrix_dimension = FALSE, allow_append = FALSE) {
  if (is_missing(index)) {
    check(kind == "[", "Missing [[ index", site)
    return(invisible(NULL))
  }
  if (is.null(index)) {
    check(kind == "[", "NULL [[ index", site)
    return(invisible(NULL))
  }
  if (kind == "[[") check(length(index) == 1L,
    "[[ index is not a scalar (recursive indexing is outside the kernel contract)", site)
  if (is.character(index)) {
    check(!anyNA(index) && all(nzchar(index)), "Missing or empty character index", site)
    if (matrix_dimension || (!is.list(x) && !is.environment(x) && !is.null(x))) {
      check(all(index %in% label_names), "Absent atomic or matrix name", site)
    }
    return(invisible(NULL))
  }
  if (is.logical(index) && kind == "[") {
    check(length(index) == extent, "Logical selector has the wrong extent", site)
    if (matrix_dimension || write) check(!anyNA(index), "Missing matrix or write selector", site)
    if (anyNA(index)) record("intentional_missing_vector_selector")
    return(invisible(NULL))
  }
  check(is.numeric(index) && !anyNA(index) && all(is.finite(index)) &&
    all(index == floor(index)), "Nonintegral or missing numeric subscript", site)
  lower = if (kind == "[") 0L else 1L
  upper = extent + as.integer(write && allow_append)
  check(all(index >= lower & index <= upper), "Numeric subscript outside its extent", site)
}

validate_access = function(x, indices, kind, write, site, expr) {
  check(is.null(x) || is.list(x) || is.atomic(x) || is.environment(x),
    "Indexing an object outside the ordinary vector/list/environment contract", site)
  if (is.environment(x)) {
    check(kind == "[[" && length(indices) == 1L && is.character(indices[[1L]]) &&
      length(indices[[1L]]) == 1L && !is.na(indices[[1L]]) && nzchar(indices[[1L]]),
      "Invalid environment index", site)
    return(invisible(NULL))
  }
  if (length(indices) == 2L) {
    check(kind == "[" && length(dim(x)) == 2L,
      "Two-dimensional access did not receive a matrix", site)
    for (j in 1:2) validate_index(x, indices[[j]], dim(x)[[j]], dimnames(x)[[j]],
      kind, write, site, matrix_dimension = TRUE)
  } else {
    check(length(indices) == 1L, "Unexpected index arity", site)
    allow_append = write && startsWith(call_key(expr), "symbol_registry[[symbol]][[")
    validate_index(x, indices[[1L]], length(x), names(x), kind, write, site,
      allow_append = allow_append)
  }
}

evaluate_indices = function(expr, env) {
  parts = as.list(expr)[-(1:2)]
  lapply(parts, function(index) if (is_missing(index)) quote(expr = ) else eval(index, env))
}

shape_read = function(site, expr, env) {
  x = eval(expr[[2L]], env)
  indices = evaluate_indices(expr, env)
  kind = as.character(expr[[1L]])
  validate_access(x, indices, kind, FALSE, site, expr)
  audit$site_counts[[site]] = audit$site_counts[[site]] + 1L
  bump("checked_reads")
  do.call(get(kind, baseenv()), c(list(x), indices))
}

shape_write = function(site, lhs, env) {
  # Original assignment evaluation is retained. Its pure index expressions are
  # evaluated here first to reject an invalid write before R can extend/recycle.
  resolve = function(expr) {
    if (!is_index_call(expr)) return(eval(expr, env))
    x = resolve(expr[[2L]])
    indices = evaluate_indices(expr, env)
    id = attr(expr, "shape_site")
    validate_access(x, indices, as.character(expr[[1L]]), FALSE, id, expr)
    audit$site_counts[[id]] = audit$site_counts[[id]] + 1L
    bump("checked_write_ancestors")
    do.call(get(as.character(expr[[1L]]), baseenv()), c(list(x), indices))
  }
  x = resolve(lhs[[2L]])
  validate_access(x, evaluate_indices(lhs, env), as.character(lhs[[1L]]), TRUE, site, lhs)
  audit$site_counts[[site]] = audit$site_counts[[site]] + 1L
  bump("checked_writes")
  invisible(NULL)
}

shape_condition = function(site, value, env) {
  check((is.logical(value) || is.numeric(value)) && length(value) == 1L && !is.na(value),
    "Condition or short-circuit operand is not a defined scalar", site)
  audit$site_counts[[site]] = audit$site_counts[[site]] + 1L
  bump("checked_conditions")
  line = audit$sites[[site]]$line
  if (!is.na(line) && line %in% c(176L, 261L) && isTRUE(value)) record("future_matrix_guard")
  value
}

shape_enter = function(name, env) {
  bump(paste0("helper_", name))
  if (name == "apply_domain_restriction") {
    idx = get("clause_idx", env)
    symbol = get("symbol", env)
    check(is.character(symbol) && length(symbol) == 1L && !is.na(symbol) && nzchar(symbol),
      "Restriction pivot is not a valid scalar name")
    entries = get("entries", env)
    flags = get("is_unit", env)
    if (flags[[idx]]) {
      check(!symbol %in% names(entries[[idx]]),
        "A stale propagation snapshot tried to change a registered unit")
      record("snapshot_became_other_symbol_unit")
    }
  }
  if (name == "register_unit") {
    idx = get("unit_idx", env)
    entries = get("entries", env)
    check(length(entries[[idx]]) == 1L && length(names(entries[[idx]])) == 1L,
      "Registration did not receive a singleton")
    key = as.character(idx)
    check(!key %in% audit$birth_indices, "A clause index was registered twice")
    audit$birth_indices = c(audit$birth_indices, key)
    if (!is.null(get("is_not_subset_of", env))) {
      inverse = get("available_inverse", env)
      check(!is.na(inverse[[idx]]), "Post-allocation unit lacks an available inverse")
      if (inverse[[idx]] > get("meta_idx_outer", env)) record("future_unit_registration")
    }
  }
  if (name %in% c("on_updated_subset_relations", "try_sse_2nd_order", "on_update_range",
    "handle_sse_2nd_order_oneend", "handle_sse_2nd_order_twoend")) {
    check(is.matrix(get("not_subset_count", env)), "Pair callback consumed the unit-HLA count vector")
  }
}

shape_phase = function(name, env) {
  audit$phase = name
  if (name == "available") {
    n = length(get("available", env))
    if (n <= 1L) record(paste0("available_", n))
  }
  if (name == "unit_hla") {
    entries = get("entries", env)
    survivors = get("remaining_nonunit_entries", env)
    registry = get("symbol_registry", env)
    check(all(unlist(as.list(registry), use.names = FALSE) %in% survivors),
      "Unit-HLA registry has an index absent from remaining_nonunit_entries")
    audit$unit_hla_survivors = survivors
  }
  if (name == "unit_hla_selection") {
    survivors = get("remaining_nonunit_entries", env)
    check(identical(survivors, audit$unit_hla_survivors), "Lazy inverse domain changed")
    counts = get("not_subset_count", env)
    rows = get("is_not_subset_of_unit", env)
    entries = get("entries", env)
    unit_symbol = get("unitsymbol", env)
    check(is.null(dim(counts)) && length(counts) == length(survivors) && !anyNA(counts),
      "Unit-HLA count vector has wrong shape")
    for (j in seq_along(survivors)) {
      row = rows[[j]]
      if (is.null(row)) {
        row = names(entries[[survivors[[j]]]]) != unit_symbol
        bump("unallocated_unit_hla_rows")
      } else bump("allocated_unit_hla_rows")
      check(is.logical(row) && !anyNA(row) && counts[[j]] == sum(row),
        "Unit-HLA pivot-count bookkeeping failed")
    }
  }
}

# First annotate original sites, including assignment ancestors. This produces
# an inventory tied to the exact source lines, separate from the transformation.
annotate = function(x) {
  if (!is.call(x)) return(x)
  op = as.character(x[[1L]])
  if (op %in% c("[", "[[", "if", "&&", "||")) attr(x, "shape_site") = new_site(x, op)
  if (length(x) > 1L) for (i in seq.int(2L, length(x))) if (!is_missing(x[[i]])) x[i] = list(annotate(x[[i]]))
  x
}
original = annotate(parsed[[1L]])
audit$site_counts = integer(length(audit$sites))

instrument = function(x) {
  if (!is.call(x)) return(x)
  op = as.character(x[[1L]])
  site = attr(x, "shape_site")
  if (op %in% c("=", "<-", "<<-")) {
    lhs = x[[2L]]
    rhs = instrument(x[[3L]])
    x[3L] = list(rhs)
    if (is_index_call(lhs)) {
      return(as.call(list(as.name("{"),
        substitute(shape_write(SITE, quote(LHS), environment()),
          list(SITE = attr(lhs, "shape_site"), LHS = lhs)), x)))
    }
    lhs_name = as.character(lhs)
    if (length(lhs_name) == 1L && lhs_name == "available_inverse") {
      return(substitute({ ASSIGN; shape_phase("available", environment()) }, list(ASSIGN = x)))
    }
    if (is.call(x[[3L]]) && identical(x[[3L]][[1L]], as.name("function")) &&
        lhs_name %in% c("register_unit", "apply_domain_restriction", "eliminate_symbol_from_clause",
          "on_updated_subset_relations", "on_update_range", "handle_sse_2nd_order_oneend",
          "handle_sse_2nd_order_twoend", "try_sse_2nd_order", "eliminate_clause_update_sr")) {
      old_body = x[[3L]][[3L]]
      x[[3L]][[3L]] = substitute({ shape_enter(NAME, environment()); BODY },
        list(NAME = lhs_name, BODY = old_body))
    }
    return(x)
  }
  if (length(x) > 1L) for (i in seq.int(2L, length(x))) if (!is_missing(x[[i]])) x[i] = list(instrument(x[[i]]))
  if (op %in% c("[", "[[")) {
    return(substitute(shape_read(SITE, quote(EXPR), environment()), list(SITE = site, EXPR = x)))
  }
  if (op == "if") x[[2L]] = substitute(shape_condition(SITE, CONDITION, environment()),
    list(SITE = site, CONDITION = x[[2L]]))
  if (op %in% c("&&", "||")) {
    for (i in 2:3) x[[i]] = substitute(shape_condition(SITE, CONDITION, environment()),
      list(SITE = site, CONDITION = x[[i]]))
  }
  if (op == "delayedAssign" && identical(x[[2L]], "roe_inverse")) {
    return(substitute({ shape_phase("unit_hla", environment()); DELAY }, list(DELAY = x)))
  }
  if (op == "repeat" && grepl("not_subset_count == 1L", call_key(x[[2L]]), fixed = TRUE)) {
    x[[2L]] = substitute({ shape_phase("unit_hla_selection", environment()); BODY }, list(BODY = x[[2L]]))
  }
  x
}
eval(instrument(original))
observed_simplify = simplify_cnf

models = function(entries, grid) {
  if (is.logical(entries)) return(rep(entries, nrow(grid)))
  out = rep(TRUE, nrow(grid))
  for (clause in entries) {
    row = rep(FALSE, nrow(grid))
    for (symbol in names(clause)) row = row | grid[[symbol]] %in% clause[[symbol]]
    out = out & row
  }
  out
}

run_case = function(entries, universe, label) {
  audit$input = list(entries = entries, universe = universe, label = label)
  audit$birth_indices = character()
  audit$phase = "startup"
  result = observed_simplify(entries, universe)
  plain = plain_simplify(entries, universe)
  check(identical(result, plain), "Instrumentation changed returned storage")
  if (prod(lengths(universe)) <= 100000L) {
    grid = expand.grid(universe, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
    check(identical(models(result, grid), models(entries, grid)), "Truth table changed")
    bump("truth_tables")
  }
  bump("cases")
  invisible(result)
}

binary = list(X = c("0", "1"), Y = c("0", "1"), Z = c("0", "1"))
run_case(TRUE, binary, "true")
run_case(FALSE, binary, "false")
run_case(list(), binary, "empty_conjunction")
run_case(list(list(X = "0")), binary, "single_unit")
run_case(list(list(X = "0"), list(X = "0")), binary, "duplicate_unit")
run_case(list(list(X = "0"), list(X = "1")), binary, "contradictory_units")
run_case(list(list(X = "0", Y = "0")), binary, "one_nonunit")
run_case(list(list(X = "0"), list(X = "1", Y = "0")), binary, "no_available_after_propagation")
run_case(list(list(X = "0"), list(Y = "0", Z = "0")), binary, "unit_with_unallocated_two_symbol_donor")
run_case(list(list(X = "0"), list(X = "1", Y = "0"), list(Y = "1", Z = "0")),
  binary, "preprocessing_cascade")
run_case(list(list(X0 = c("2", "6"), X1 = "6"),
  list(X0 = "2", X1 = c("6", "3")), list(X0 = "5", X1 = c("3", "0")),
  list(X1 = "3", X0 = c("5", "2")), list(X0 = "1", X1 = c("6", "5"))),
  list(X0 = c("1", "2", "5", "6"), X1 = c("0", "3", "5", "6")), "deferred_registration")
run_case(list(list(X = "2", Y = "0"), list(Y = "1", X = "0"),
  list(X = "2", Z = "0"), list(X = c("0", "1"), W = "0"),
  list(X = c("0", "1"), W = "1")),
  list(X = c("0", "1", "2"), Y = c("0", "1"), Z = c("0", "1"), W = c("0", "1")),
  "snapshot_became_other_symbol_unit")

# A complete small boundary space: all ordered lists of at most three clauses
# over two Boolean symbols. Every literal is a nonempty proper singleton.
small_universe = binary[c("X", "Y")]
clauses = list(list(X = "0"), list(X = "1"), list(Y = "0"), list(Y = "1"),
  list(X = "0", Y = "0"), list(X = "0", Y = "1"),
  list(X = "1", Y = "0"), list(X = "1", Y = "1"))
for (k in 1:3) {
  indices = expand.grid(rep(list(seq_along(clauses)), k), KEEP.OUT.ATTRS = FALSE)
  for (i in seq_len(nrow(indices))) run_case(clauses[as.integer(indices[i, ])],
    small_universe, sprintf("exhaustive_%i_%i", k, i))
}

seed = as.integer(Sys.getenv("CNF_SHAPE_SEED", "19073"))
set.seed(seed)
trials = as.integer(Sys.getenv("CNF_SHAPE_TRIALS", "3000"))
for (trial in seq_len(trials)) {
  n_symbols = sample.int(5L, 1L) + 1L
  symbol_names = paste0("s", seq_len(n_symbols))
  universe = setNames(lapply(seq_len(n_symbols), function(i) as.character(seq_len(sample.int(4L, 1L) + 1L))), symbol_names)
  n_clauses = sample.int(18L, 1L)
  entries = lapply(seq_len(n_clauses), function(i) {
    width = sample.int(n_symbols, 1L)
    selected = sample.int(n_symbols, width)
    setNames(lapply(selected, function(j) {
      domain = universe[[j]]
      domain[sample.int(length(domain), sample.int(length(domain) - 1L, 1L))]
    }), symbol_names[selected])
  })
  run_case(entries, universe, sprintf("random_%i", trial))
  if (trial %% 500L == 0L) cat(sprintf("random trial %i: %i checked reads\n", trial, audit$counts$checked_reads))
}

# Negative controls ask the checker itself to reject the exact failure classes.
# These do not claim that the unmodified source can reach the supplied states.
reject = function(label, expr, text) {
  err = tryCatch({ force(expr); NULL }, error = identity)
  check(inherits(err, "error") && grepl(text, conditionMessage(err), fixed = TRUE),
    paste("Negative control did not reject", label))
  bump("negative_controls")
}
site = 1L
reject("missing numeric [[", validate_access(1:2, list(NA_integer_), "[[", FALSE, site, quote(x[[i]])), "missing numeric")
reject("vector-valued [[", validate_access(list(X = "0", Y = "0"), list(c("X", "Y")), "[[", FALSE, site, quote(x[[i]])), "not a scalar")
reject("absent matrix symbol", validate_access(matrix(TRUE, 2, 2, dimnames = list(NULL, c("X", "Y"))), list(1L, "Z"), "[", FALSE, site, quote(x[i,j])), "Absent atomic or matrix")
reject("unallocated matrix", validate_access(NULL, list(1L, "X"), "[", FALSE, site, quote(x[i,j])), "did not receive a matrix")
reject("vector condition", shape_condition(site, c(TRUE, FALSE), globalenv()), "not a defined scalar")
reject("NA condition", shape_condition(site, NA, globalenv()), "not a defined scalar")
reject("out-of-bounds write", validate_access(logical(2), list(3L), "[[", TRUE, site, quote(eliminated[[i]])), "outside its extent")
reject("wrong count rank", validate_access(integer(2), list(1L, 1L), "[", FALSE, site, quote(not_subset_count[i,j])), "did not receive a matrix")

sites = do.call(rbind, lapply(seq_along(audit$sites), function(i) {
  as.data.frame(c(audit$sites[[i]], list(evaluations = audit$site_counts[[i]])), stringsAsFactors = FALSE)
}))
tag = Sys.getenv("CNF_SHAPE_TAG", paste0("seed", seed))
write.csv(sites, file.path(audit_dir, paste0("sites_", tag, ".csv")), row.names = FALSE)
saveRDS(list(counts = audit$counts, examples = audit$examples, source_md5 = tools::md5sum(source_path),
  R_version = R.version.string, seed = seed, trials = trials, sites = sites),
  file.path(audit_dir, paste0("results_", tag, ".rds")))
print(audit$counts)
cat(sprintf("PASS: %i exact-source cases; %i negative controls.\n", audit$counts$cases, audit$counts$negative_controls))
