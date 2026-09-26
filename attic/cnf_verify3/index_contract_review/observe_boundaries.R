# Independent review observer. This does not source the author's instrumentation.
# Run from the repository root; no non-base packages are required.
source_file = "R/CnfFormula_simplify.R"
out_dir = "attic/cnf_verify3/index_contract_review"
tag = Sys.getenv("CNF_REVIEW_TAG", "r36")
source_lines = readLines(source_file)
parsed = parse(source_file, keep.source = TRUE)
pd = getParseData(parsed)
sites = pd[pd$terminal & pd$text %in% c("[", "[[", "if", "&&", "||", "stop"),
  c("line1", "col1", "line2", "col2", "token", "text")]
sites = sites[order(sites$line1, sites$col1), ]
write.csv(sites, file.path(out_dir, paste0("lexical_sites_", tag, ".csv")), row.names = FALSE)
stopifnot(sum(sites$text %in% c("[", "[[")) == 340L,
  sum(sites$text == "if") == 108L, sum(sites$text %in% c("&&", "||")) == 34L,
  sum(sites$text == "stop") == 1L)

observations = new.env(parent = emptyenv())
counts = new.env(parent = emptyenv())
fixtures = new.env(parent = emptyenv())
current = NULL
bump = function(key, n = 1L) {
  old = counts[[key]]
  counts[[key]] = if (is.null(old)) n else old + n
}
remember = function(key) {
  if (is.null(fixtures[[key]])) fixtures[[key]] = current
}
assert = function(ok, what) {
  if (!isTRUE(ok)) stop(paste("INDEPENDENT REVIEW:", what), call. = FALSE)
}
valid_index = function(i, n) {
  is.numeric(i) && !anyNA(i) && all(i == trunc(i) & i >= 1L & i <= n)
}
lookup = function(e, x) get(x, envir = e, inherits = TRUE)

check_registry = function(e) {
  entries = lookup(e, "entries")
  eliminated = lookup(e, "eliminated")
  is_unit = lookup(e, "is_unit")
  sr = lookup(e, "symbol_registry")
  assert(is.logical(eliminated) && length(eliminated) == length(entries) && !anyNA(eliminated), "elimination shape")
  assert(is.logical(is_unit) && length(is_unit) == length(entries) && !anyNA(is_unit), "unit flag shape")
  for (symbol in names(sr)) {
    idx = sr[[symbol]]
    assert(is.null(idx) || (valid_index(idx, length(entries)) && !anyDuplicated(idx)), "registry index domain")
    for (i in idx) assert(!eliminated[[i]] && !is_unit[[i]] &&
      length(entries[[i]]) >= 2L && symbol %in% names(entries[[i]]), "registry live occurrence")
  }
  if (!is.null(lookup(e, "is_not_subset_of"))) {
    # register_unit's entry is an intentional transient: its pending singleton
    # has left the registry before receiving an inactive flag.
    live = which(!(eliminated | is_unit) & lengths(entries) > 1L)
    for (i in live) for (symbol in names(entries[[i]]))
      assert(i %in% sr[[symbol]], "missing live registry occurrence")
  }
  ur = lookup(e, "unit_registry")
  ud = lookup(e, "unit_domains")
  assert(setequal(names(ur), names(ud)), "unit registry/domain names")
  if (lookup(e, ".review_phase") != "unit") for (symbol in names(ur)) {
    i = ur[[symbol]]
    assert(length(i) == 1L && valid_index(i, length(entries)) && is_unit[[i]] && !eliminated[[i]], "unit representative lifecycle")
    assert(identical(names(entries[[i]]), symbol) && identical(entries[[i]][[1L]], ud[[symbol]]), "unit representative content")
  }
  bump("registry_boundaries")
}

check_pairs = function(e, delta = NULL) {
  mats = lookup(e, "is_not_subset_of")
  if (is.null(mats)) return(invisible(NULL))
  assert(lookup(e, ".review_phase") == "pair", "matrix helper after vector transition")
  a = lookup(e, "available")
  inv = lookup(e, "available_inverse")
  entries = lookup(e, "entries")
  cts = lookup(e, "not_subset_count")
  n = length(a)
  assert(valid_index(a, length(entries)) && !anyDuplicated(a), "available indices")
  assert(length(inv) == length(entries) && identical(inv[a], seq_along(a)), "fixed available inverse")
  assert(length(mats) == n && is.matrix(cts) && identical(dim(cts), c(n, n)), "pair matrix extent")
  assert(all(is.na(diag(cts))), "diagonal counts")
  for (k in seq_along(mats)) {
    mat = mats[[k]]
    if (is.null(mat)) {
      assert(all(is.na(cts[k, ])) && all(is.na(cts[, k])), "unallocated initialized pair")
      next
    }
    assert(is.matrix(mat) && is.logical(mat) && !anyNA(mat) && nrow(mat) == n && ncol(mat) >= 2L,
      "allocated matrix shape")
    assert(!anyDuplicated(colnames(mat)) && all(names(entries[[a[[k]]]]) %in% colnames(mat)), "frozen matrix names")
    assert(!any(mat[k, ]), "self-row exclusion")
    initialized = which(!is.na(cts[k, ]))
    expected = rowSums(mat)[initialized]
    if (!is.null(delta)) expected = expected + delta[k, initialized]
    assert(identical(as.numeric(cts[k, initialized]), as.numeric(expected)), "paired row/count synchronization")
    bump("initialized_pair_rows", length(initialized))
  }
  bump("pair_boundaries")
}

check_local_nonunit = function(e, pending = FALSE) {
  a = lookup(e, "available")
  inv = lookup(e, "available_inverse")
  mats = lookup(e, "is_not_subset_of")
  target = lookup(e, "clause_idx")
  donors = lookup(e, "remaining_other_entries")
  live = lookup(e, "remaining_nonunit_entries")
  ct = lookup(e, "not_subset_count_current")
  assert(identical(donors, live[live != target]), "nonunit donor snapshot")
  assert(length(ct) == length(donors) && !anyNA(ct) && is.null(dim(ct)), "nonunit local count extent")
  assert(length(lookup(e, "was_used")) == length(donors), "nonunit used extent")
  expected = vapply(donors, function(d) sum(mats[[inv[[d]]]][inv[[target]], ]), numeric(1))
  if (pending) {
    updated = a[[lookup(e, "updating_clause_meta_idx")]]
    i = match(updated, donors)
    assert(!is.na(i), "self donor reached local decrement")
    expected[[i]] = expected[[i]] + 1L
  }
  assert(identical(as.numeric(ct), as.numeric(expected)), "nonunit local row/count equality")
  bump(if (pending) "nonunit_pending_repairs" else "nonunit_selection_boundaries")
}

check_local_unit = function(e, pending = FALSE) {
  donors = lookup(e, "remaining_nonunit_entries")
  assert(identical(donors, lookup(e, ".review_fixed_nonunits")), "lazy inverse domain changed")
  assert(identical(as.list(lookup(e, "symbol_registry"), all.names = TRUE), lookup(e, ".review_fixed_registry")), "unit-HLA registry changed")
  ct = lookup(e, "not_subset_count")
  rows = lookup(e, "is_not_subset_of_unit")
  entries = lookup(e, "entries")
  unitsymbol = lookup(e, "unitsymbol")
  assert(is.character(unitsymbol) && length(unitsymbol) == 1L && !is.na(unitsymbol), "scalar unit symbol")
  assert(is.numeric(ct) && is.null(dim(ct)) && length(ct) == length(donors) && !anyNA(ct), "unit vector transition")
  assert(length(rows) == length(donors) && length(lookup(e, "was_used")) == length(donors), "lazy rows extent")
  for (i in seq_along(donors)) {
    nm = names(entries[[donors[[i]]]])
    row = rows[[i]]
    if (is.null(row)) {
      expected = sum(nm != unitsymbol)
      bump("unallocated_lazy_rows")
    } else {
      assert(is.logical(row) && !anyNA(row) && identical(names(row), nm), "allocated lazy row names")
      expected = sum(row)
      bump("allocated_lazy_rows")
    }
    if (pending && i == lookup(e, "updating_hla_clause_idx")) expected = expected + 1L
    assert(ct[[i]] == expected, "lazy row/count synchronization")
  }
  bump(if (pending) "unit_pending_repairs" else "unit_selection_boundaries")
}

.review_audit = function(kind, e = parent.frame()) {
  bump(paste0("hook_", kind))
  if (kind %in% c("register", "restrict", "delete_symbol", "pair", "range", "oneend", "twoend", "trial", "eliminate")) {
    check_registry(e)
    if (lookup(e, ".review_phase") == "pair") check_pairs(e)
  }
  if (kind == "register") {
    i = lookup(e, "unit_idx")
    entries = lookup(e, "entries")
    assert(length(i) == 1L && valid_index(i, length(entries)) && length(entries[[i]]) == 1L, "registration singleton")
    if (!is.null(lookup(e, "is_not_subset_of"))) {
      k = lookup(e, "available_inverse")[[i]]
      assert(length(k) == 1L && !is.na(k), "registration inverse")
      if (k > lookup(e, "meta_idx_outer")) { bump("future_registration"); remember("future_registration") }
    }
  }
  if (kind == "snapshot") {
    i = lookup(e, "s_clause_idx")
    if (!lookup(e, "eliminated")[[i]] && lookup(e, "is_unit")[[i]]) {
      assert(!lookup(e, "nu") %in% names(lookup(e, "entries")[[i]]), "retained stale unit still has propagated symbol")
      bump("stale_other_symbol_unit")
      remember("stale_other_symbol_unit")
    }
  }
  if (kind == "optional_matrix") {
    k = lookup(e, "s_clause_idx_meta")
    if (k <= lookup(e, "meta_idx_outer")) {
      mat = lookup(e, "is_not_subset_of")[[k]]
      assert(is.matrix(mat) && lookup(e, "nu") %in% colnames(mat), "optional snapshot matrix/column")
      bump("optional_allocated_matrix")
    } else { bump("optional_future_short_circuit"); remember("optional_future_short_circuit") }
  }
  if (kind == "pair") {
    i = lookup(e, "meta_idx")
    j = lookup(e, "meta_idx_other")
    assert(i != j && !is.na(lookup(e, "not_subset_count")[i, j]), "pair initialized/distinct")
    ids = lookup(e, "available")[c(i, j)]
    assert(!any(lookup(e, "eliminated")[ids] | lookup(e, "is_unit")[ids]), "live pair operands")
  }
  if (kind == "eliminate") assert(!lookup(e, "is_unit")[[lookup(e, "clause_idx")]], "explicit stop unreachable")
  if (kind %in% c("na_twoends", "na_oneends")) {
    values = lookup(e, if (kind == "na_twoends") "twoends" else "oneends")
    if (anyNA(values)) { bump("intermediate_na_candidate_vectors"); remember(kind) }
  }
  if (kind %in% c("consume_twoend", "consume_oneend")) {
    i = lookup(e, if (kind == "consume_twoend") "meta_idx_twoend" else "meta_idx_oneend")
    assert(length(i) == 1L && valid_index(i, length(lookup(e, "available"))), "filtered candidate consumption")
    bump("filtered_candidate_consumptions")
  }
  if (kind %in% c("reverse_pending", "forward_pending", "delete_pending", "deleted_reverse_pending")) {
    n = length(lookup(e, "available"))
    delta = matrix(0L, n, n)
    if (kind == "reverse_pending") delta[lookup(e, "other_meta_idx"), lookup(e, "meta_idx")] = -1L
    if (kind == "forward_pending") delta[lookup(e, "meta_idx"), lookup(e, "other_meta_idx")] = 1L
    if (kind == "delete_pending") delta[lookup(e, "meta_idx"), lookup(e, "rows_changed")] = 1L
    if (kind == "deleted_reverse_pending") delta[lookup(e, "meta_idx_other"), lookup(e, "meta_idx")] = -1L
    check_pairs(e, delta)
    remember(kind)
  }
  if (kind == "repaired") check_pairs(e)
  if (kind == "nonunit_select") check_local_nonunit(e)
  if (kind == "nonunit_pending") { check_local_nonunit(e, TRUE); remember(kind) }
  if (kind == "nonunit_repaired") check_local_nonunit(e)
  if (kind == "self_loop") {
    i = lookup(e, "updating_clause_meta_idx")
    j = lookup(e, "meta_idx")
    if (i == j) { assert(!lookup(e, "is_not_subset_of")[[i]][j, lookup(e, "symbol")], "self row entered update"); bump("self_guard"); remember("self_guard") }
  }
  if (kind == "unit_select") check_local_unit(e)
  if (kind == "unit_pending") { check_local_unit(e, TRUE); remember(kind) }
  if (kind == "unit_repaired") check_local_unit(e)
  if (kind == "inverse_before") {
    assert(lookup(e, "updating_clause_idx") %in% lookup(e, "remaining_nonunit_entries"), "forced inverse member")
    bump("lazy_inverse_consumptions")
  }
  if (kind == "inverse_after") assert(identical(lookup(e, "updating_hla_clause_idx"),
    match(lookup(e, "updating_clause_idx"), lookup(e, "remaining_nonunit_entries"))), "forced inverse value")
  if (kind == "hla_split") {
    live = which(!lookup(e, "eliminated"))
    nunit = sum(lookup(e, "is_unit")[live])
    assert(nunit == length(lookup(e, "unit_domains")), "unit split binding count")
    nnon = sum(!lookup(e, "is_unit")[live])
    if (nnon <= 1L) bump(paste0("hla_nonunit_count_", nnon))
  }
  invisible(NULL)
}

.review_condition = function(value, kind) {
  assert((is.logical(value) || is.numeric(value)) && length(value) == 1L && !is.na(value), "scalar condition")
  bump(paste0("condition_", kind))
  value
}
wrap_conditions = function(x) {
  if (!is.call(x)) return(x)
  p = as.list(x)
  op = as.character(p[[1L]])[[1L]]
  for (i in seq_along(p)[-1L]) {
    if (identical(p[i], list(quote(expr = )))) next
    p[i] = list(wrap_conditions(p[[i]]))
  }
  if (op == "if") p[2L] = list(call(".review_condition", p[[2L]], "if"))
  if (op %in% c("&&", "||")) for (i in 2:3)
    p[i] = list(call(".review_condition", p[[i]], op))
  as.call(p)
}

hooks = list()
before = function(line, expr) hooks[[as.character(line)]] <<- c(hooks[[as.character(line)]], expr)
before(43, '.review_phase = "pair"')
for (entry in c("88:register", "147:restrict", "237:delete_symbol", "298:pair", "348:range", "374:oneend", "408:twoend", "453:trial", "473:eliminate",
  "128:snapshot", "133:optional_matrix", "187:reverse_pending", "222:forward_pending", "266:delete_pending", "272:deleted_reverse_pending",
  "223:repaired", "267:repaired", "273:repaired", "189:repaired", "686:nonunit_select", "710:nonunit_pending", "712:nonunit_repaired",
  "707:self_loop", "745:unit_select", "774:unit_pending", "775:unit_repaired", "764:inverse_before", "765:inverse_after", "657:hla_split",
  "383:na_twoends", "425:na_oneends", "387:consume_twoend", "430:consume_oneend")) {
  z = strsplit(entry, ":", fixed = TRUE)[[1L]]
  before(z[[1L]], sprintf('.review_audit("%s")', z[[2L]]))
}
before(657, '.review_phase = "nonunit"')
before(730, '.review_phase = "unit"; .review_fixed_nonunits = remaining_nonunit_entries; .review_fixed_registry = as.list(symbol_registry, all.names = TRUE)')
make_observed = function(lines = source_lines) {
  annotated = unlist(lapply(seq_along(lines), function(i) c(hooks[[as.character(i)]], lines[[i]])), use.names = FALSE)
  env = new.env(parent = environment())
  eval(wrap_conditions(parse(text = annotated)[[1L]]), envir = env)
  env$simplify_cnf
}
original_env = new.env()
eval(parsed, envir = original_env)
original = original_env$simplify_cnf
observed = make_observed()

new_universe = function(sizes) {
  as.environment(setNames(lapply(sizes, function(n) as.character(seq_len(n) - 1L)), LETTERS[seq_along(sizes)]))
}
case = function(entries, universe, label) list(entries = entries, universe = universe, label = label)
run_case = function(x, fn = observed, compare = TRUE) {
  current <<- x
  ans = fn(x$entries, x$universe)
  if (compare) assert(identical(ans, original(x$entries, x$universe)), paste("observer altered result", x$label))
  bump("cases")
  invisible(ans)
}

u2 = new_universe(c(2L, 2L, 2L))
u3 = new_universe(c(3L, 2L, 2L, 2L))
stale = case(list(list(A = "2", B = "0"), list(B = "1", A = "0"), list(A = "2", C = "0"),
  list(A = c("0", "1"), D = "0"), list(A = c("0", "1"), D = "1")), u3, "stale different-symbol singleton")
directed = list(case(TRUE, u2, "TRUE"), case(FALSE, u2, "FALSE"), case(list(), u2, "empty conjunction"),
  case(list(list(A = "0")), u2, "one unit"), case(list(list(A = "0"), list(A = "0")), u2, "duplicate units"),
  case(list(list(A = "0"), list(A = "1")), u2, "contradictory units"),
  case(list(list(A = "0"), list(A = "0", B = "0")), u2, "zero remaining nonunits"),
  case(list(list(A = "0", B = "0")), u2, "one nonunit"), stale)
for (x in directed) run_case(x)

# All ordered lists of up to three canonical clauses on three Boolean symbols.
# The digit 0 is absent, 1 is value "0", 2 is value "1".
pool = lapply(seq_len(26L), function(code) {
  digits = (code %/% 3L^(0:2)) %% 3L
  keep = which(digits != 0L)
  setNames(lapply(digits[keep] - 1L, as.character), LETTERS[keep])
})
max_width = as.integer(Sys.getenv("CNF_REVIEW_EXHAUSTIVE_WIDTH", "3"))
for (width in seq_len(max_width)) {
  for (code in 0:(length(pool)^width - 1L)) {
    positions = (code %/% length(pool)^(seq_len(width) - 1L)) %% length(pool) + 1L
    run_case(case(pool[positions], u2, paste0("Boolean3/", width, "/", code)))
  }
  cat("Completed Boolean3 ordered width", width, "\n")
}

set.seed(202609061L)
trials = as.integer(Sys.getenv("CNF_REVIEW_TRIALS", "1500"))
for (trial in seq_len(trials)) {
  nsym = sample.int(6L, 1L) + 1L
  sizes = sample.int(4L, nsym, replace = TRUE) + 1L
  u = new_universe(sizes)
  nc = sample.int(20L, 1L)
  entries = lapply(seq_len(nc), function(j) {
    # Most clauses are nonunits, while some explicit units trigger early paths.
    width = if (runif(1L) < 0.09) 1L else sample.int(nsym - 1L, 1L) + 1L
    symbols = sample.int(nsym, width)
    setNames(lapply(symbols, function(k) {
      values = u[[LETTERS[[k]]]]
      values[sample.int(length(values), sample.int(length(values) - 1L, 1L))]
    }), LETTERS[symbols])
  })
  run_case(case(entries, u, paste0("random/", trial)))
}

positive_counts = as.list(counts, all.names = TRUE)
positive_fixtures = as.list(fixtures, all.names = TRUE)
controls = list()
control = function(name, fixture, line, replacement) {
  assert(!is.null(fixture), paste("missing control fixture", name))
  run_case(fixture)
  corrupted = source_lines
  corrupted[[line]] = replacement
  message = tryCatch({ run_case(fixture, make_observed(corrupted), FALSE); NA_character_ }, error = conditionMessage)
  assert(!is.na(message), paste("control undetected", name))
  controls[[name]] <<- message
}
control("stale missing-symbol guard", stale, 149L, "")
control("optional future-matrix guard", positive_fixtures$optional_future_short_circuit, 133L,
  "if (inso_column[[s_clause_idx_meta]] && !is_not_subset_of[[s_clause_idx_meta]][unit_idx_meta, nu]) next")
control("reverse count repair", positive_fixtures$reverse_pending, 188L,
  "not_subset_count[other_meta_idx, meta_idx] <<- not_subset_count[other_meta_idx, meta_idx] + 2L")
control("forward count repair", positive_fixtures$forward_pending, 222L,
  "not_subset_count[meta_idx, other_meta_idx] <<- (rowsum = not_subset_count[meta_idx, other_meta_idx] - 2L)")
control("deleted-column count repair", positive_fixtures$delete_pending, 266L,
  "not_subset_count[meta_idx, rows_changed] <<- not_subset_count[meta_idx, rows_changed] - 2L")
control("self-row exclusion", positive_fixtures$self_guard, 569L,
  "is_not_subset_of[[meta_idx_outer]][meta_idx_outer, ] = TRUE")
control("lazy row count repair", positive_fixtures$unit_pending, 774L,
  "not_subset_count[[updating_hla_clause_idx]] = not_subset_count[[updating_hla_clause_idx]] - 2L")
control("delayed inverse domain", positive_fixtures$unit_pending, 730L,
  'delayedAssign("roe_inverse", match(seq_along(entries), integer()))')

result = list(version = R.version.string, source_md5 = unname(tools::md5sum(source_file)),
  counts = positive_counts, fixtures = positive_fixtures, controls = controls,
  seed = 202609061L, trials = trials, exhaustive_width = max_width, directed = length(directed))
saveRDS(result, file.path(out_dir, paste0("observations_", tag, ".rds")))
print(sort(unlist(positive_counts)))
print(controls)
cat("PASS", positive_counts$cases, "original inputs;", length(controls), "corrupted-source controls rejected\n")
