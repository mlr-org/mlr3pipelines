# Independent Boolean occurrence indexing/termination observations.
# The original source is evaluated unchanged beside a private observed copy.
# Run from the repository root; output stays in this audit directory.
suppressPackageStartupMessages(library(checkmate))
stopf = function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)
map_chr = function(x, f, ...) vapply(x, f, character(1L), ...)
out_dir = "attic/cnf_verify3/boolean_occurrence_totality"
tag = Sys.getenv("CNF_TOTALITY_TAG", if (getRversion() < "4") "r36" else "r46")
source_paths = file.path("R", paste0(c("CnfUniverse", "CnfSymbol", "CnfAtom",
  "CnfClause", "CnfFormula", "CnfFormula_simplify"), ".R"))
cnf = new.env(parent = globalenv())
for (path in source_paths) sys.source(path, cnf)
original = cnf$simplify_cnf
source_lines = readLines(tail(source_paths, 1L))
parsed = parse(text = source_lines, keep.source = TRUE)
lex = getParseData(parsed)
lex = lex[lex$terminal & lex$text %in% c("[", "[[", "if", "&&", "||", "stop"),
  c("line1", "col1", "line2", "col2", "token", "text")]
lex = lex[order(lex$line1, lex$col1), ]
stopifnot(sum(lex$text == "if") == 108L, sum(lex$text %in% c("&&", "||")) == 34L,
  sum(lex$text %in% c("[", "[[")) == 340L)
write.csv(lex, file.path(out_dir, paste0("sites_", tag, ".csv")), row.names = FALSE)

totals = list()
examples = list()
runtime = new.env(parent = emptyenv())
bump = function(key, n = 1L) {
  old = totals[[key]]
  totals[[key]] <<- if (is.null(old)) n else old + n
}
need = function(ok, message) {
  if (isTRUE(ok)) return(invisible(NULL))
  runtime$failed = TRUE
  stop(paste("BOOLEAN TOTALITY:", message, "in", runtime$label), call. = FALSE)
}
remember = function(key, extra = NULL) {
  bump(key)
  if (is.null(examples[[key]])) examples[[key]] <<- list(words = runtime$words,
    label = runtime$label, extra = extra)
}
valid = function(i, n) is.numeric(i) && !anyNA(i) && all(i == trunc(i) & i >= 1L & i <= n)
getv = function(e, key) get(key, e, inherits = TRUE)
root = function(e) {
  while (!exists("eliminated", e, inherits = FALSE)) e = parent.env(e)
  e
}
weight = function(e) if (is.list(e$entries)) sum(lengths(e$entries)) else 0L

check_registry = function(e) {
  m = length(e$entries)
  need(is.logical(e$is_unit) && is.logical(e$eliminated) &&
    length(e$is_unit) == m && length(e$eliminated) == m &&
    !anyNA(e$is_unit) && !anyNA(e$eliminated), "lifecycle flag shape")
  for (symbol in names(e$symbol_registry)) {
    indices = e$symbol_registry[[symbol]]
    need(is.null(indices) || valid(indices, m), "registry numerical domain")
    need(length(indices) <= runtime$W0, "finite occurrence registry")
    if (anyDuplicated(indices)) remember("duplicate_registry_boundary")
    for (i in unique(indices)) {
      need(!e$is_unit[[i]] && !e$eliminated[[i]] && length(e$entries[[i]]) >= 2L &&
        symbol %in% names(e$entries[[i]]), "registry live physical occurrence")
      need(sum(indices == i) == sum(names(e$entries[[i]]) == symbol),
        "registered occurrence multiplicity")
    }
  }
  for (symbol in names(e$unit_registry)) {
    i = e$unit_registry[[symbol]]
    need(length(i) == 1L && valid(i, m) && e$is_unit[[i]] && !e$eliminated[[i]],
      "live registered unit")
    need(identical(names(e$entries[[i]]), symbol) &&
      identical(e$entries[[i]][[1L]], e$unit_domains[[symbol]]), "unit content")
  }
  bump("registry_boundaries")
}

check_pairs = function(e) {
  if (is.null(e$is_not_subset_of) || runtime$phase != "pair") return(invisible(NULL))
  a = e$available
  n = length(a)
  need(valid(a, length(e$entries)) && !anyDuplicated(a) &&
    identical(e$available_inverse[a], seq_along(a)), "fixed actual/meta inverse")
  need(is.matrix(e$not_subset_count) && identical(dim(e$not_subset_count), c(n, n)) &&
    all(is.na(diag(e$not_subset_count))), "count matrix/diagonal shape")
  for (i in seq_along(a)) {
    mat = e$is_not_subset_of[[i]]
    if (is.null(mat)) {
      need(all(is.na(e$not_subset_count[i, ])) && all(is.na(e$not_subset_count[, i])),
        "no initialized unallocated pair")
      next
    }
    need(is.matrix(mat) && is.logical(mat) && !anyNA(mat) && nrow(mat) == n &&
      ncol(mat) >= 2L && !any(mat[i, ]), "allocated matrix and self row")
    need(all(names(e$entries[[a[[i]]]]) %in% colnames(mat)), "frozen name coverage")
    trailing = which(duplicated(colnames(mat)))
    if (length(trailing)) need(all(mat[-i, trailing, drop = FALSE]), "permanent trailing columns")
    initialized = which(!is.na(e$not_subset_count[i, ]))
    need(identical(as.numeric(rowSums(mat)[initialized]),
      as.numeric(e$not_subset_count[i, initialized])), "physical row/count equality")
    bump("initialized_pair_rows", length(initialized))
    if (!e$is_unit[[a[[i]]]] && !e$eliminated[[a[[i]]]] && length(e$entries[[a[[i]]]]) > 1L) {
      for (symbol in unique(names(e$entries[[a[[i]]]]))) {
        if (!a[[i]] %in% e$symbol_registry[[symbol]]) {
          need(sum(colnames(mat) == symbol) >= 2L, "orphan duplicate birth columns")
          remember("orphan_boundary")
        }
      }
    }
  }
  bump("matrix_boundaries")
}

check_state = function(e) {
  check_registry(e)
  for (i in seq_along(e$entries)) {
    cl = e$entries[[i]]
    need(is.list(cl) && length(cl) > 0L && is.character(names(cl)) &&
      !anyNA(names(cl)) && all(nzchar(names(cl))), "stored physical clause shape")
    for (j in seq_along(cl)) need(is.character(cl[[j]]) && length(cl[[j]]) == 1L &&
      !is.na(cl[[j]]) && identical(cl[[j]], cl[[names(cl)[[j]]]]), "homogeneous singleton occurrences")
  }
  check_pairs(e)
}

helpers = c("char_intersect", "char_setdiff", "char_union", "return_entries", "register_unit",
  "apply_domain_restriction", "eliminate_symbol_from_clause", "on_updated_subset_relations",
  "on_update_range", "handle_sse_2nd_order_oneend", "handle_sse_2nd_order_twoend",
  "try_sse_2nd_order", "eliminate_clause_update_sr")
inference = helpers[5:13]
.bo_enter = function(fn, e, local) {
  w = weight(e)
  need(w <= runtime$last_weight, "stored occurrence potential increased")
  runtime$last_weight = w
  if (fn == "apply_domain_restriction") {
    for (ancestor in runtime$stack) if (ancestor$fn == fn) {
      need(w < ancestor$weight, "restriction ancestor lacks strict occurrence progress")
      bump("strict_restriction_ancestor_checks")
    }
    need(is.character(local$symbol) && length(local$symbol) == 1L &&
      !is.na(local$symbol) && nzchar(local$symbol), "scalar restriction symbol")
  }
  runtime$stack[[length(runtime$stack) + 1L]] = list(fn = fn, weight = w)
  runtime$max_depth = max(runtime$max_depth, length(runtime$stack))
  need(length(runtime$stack) <= 13L * (runtime$W0 + 1L), "conservative helper height")
  bump("helper_activations")
  if (fn %in% inference) check_state(e)
  if (fn == "on_update_range") need(FALSE, "Boolean nonempty range-update path reached")
  if (fn == "register_unit") {
    i = local$unit_idx
    need(length(i) == 1L && valid(i, length(e$entries)) && length(e$entries[[i]]) == 1L,
      "registration singleton index")
    need(!i %in% runtime$registered, "same clause registered twice")
    runtime$registered = c(runtime$registered, i)
    if (!is.null(e$is_not_subset_of)) {
      need(!is.na(e$available_inverse[[i]]), "new unit inverse")
      if (e$available_inverse[[i]] > e$meta_idx_outer) remember("future_unit_registration")
    }
  }
  if (fn == "eliminate_symbol_from_clause") {
    if (sum(names(e$entries[[local$clause_idx]]) == local$symbol) > 1L)
      remember("delete_copy_with_copies_remaining")
  }
  if (fn == "eliminate_clause_update_sr") need(!e$is_unit[[local$clause_idx]], "internal stop unreachable")
}
.bo_exit = function(fn, e) {
  if (runtime$failed) return(invisible(NULL))
  need(length(runtime$stack) > 0L && tail(runtime$stack, 1L)[[1L]]$fn == fn, "observer helper stack")
  runtime$stack = head(runtime$stack, -1L)
  need(weight(e) <= runtime$last_weight, "exit occurrence potential increased")
  runtime$last_weight = weight(e)
  if (fn %in% inference) check_state(e)
}
.bo_scalar = function(value, kind) {
  need((is.logical(value) || is.numeric(value)) && length(value) == 1L && !is.na(value),
    paste("scalar condition", kind))
  bump("scalar_conditions")
  value
}

check_hla = function(e) {
  donors = e$remaining_other_entries
  need(identical(donors, e$remaining_nonunit_entries[e$remaining_nonunit_entries != e$clause_idx]),
    "HLA donor set")
  expected = vapply(donors, function(i) {
    sum(e$is_not_subset_of[[e$available_inverse[[i]]]][e$meta_idx, ])
  }, numeric(1L))
  need(identical(as.numeric(e$not_subset_count_current), as.numeric(expected)) &&
    !anyNA(e$not_subset_count_current), "HLA physical local count equality")
  need(length(e$was_used) == length(donors) && is.logical(e$was_used) && !anyNA(e$was_used),
    "HLA used vector extent")
  bump("hla_local_count_boundaries")
}
.bo_hook = function(kind, e = parent.frame()) {
  s = root(e)
  if (kind == "birth") {
    for (name in names(s$clause)) need(s$clause_idx %in% s$symbol_registry[[name]], "no orphan at cache birth")
    bump("cache_births")
  }
  if (kind %in% c("first_pivot", "hla_pivot", "oneend_pivots", "twoend_pivots")) {
    v = getv(e, switch(kind, first_pivot = "symbol_to_restrict", hla_pivot = "symbol",
      oneend_pivots = "symbol_target", twoend_pivots = "symbols_twoend"))
    expected = if (kind %in% c("oneend_pivots", "twoend_pivots")) 2L else 1L
    need(is.character(v) && length(v) == expected && !anyNA(v) && all(nzchar(v)),
      paste("physical pivot cardinality", kind))
    bump(kind)
    if (expected == 2L && anyDuplicated(v)) remember("same_name_two_pivots")
  }
  if (kind == "two_match") {
    i = e$symbols_twoend_idx
    width = ncol(s$is_not_subset_of[[e$meta_idx_oneend]])
    need(length(i) == 2L && !anyNA(i) && all(i >= 0L & i <= width), "zero/repeated matched positions")
    if (any(i == 0L)) remember("zero_match_position")
    if (i[[1L]] > 0L && i[[1L]] == i[[2L]]) remember("repeated_first_match_position")
  }
  if (kind == "unit_visit") {
    i = e$s_clause_idx
    need(valid(i, length(s$entries)) && length(i) == 1L, "saved registry occurrence index")
    bump("unit_snapshot_visits")
    if (!s$eliminated[[i]] && s$is_unit[[i]]) {
      need(!e$nu %in% names(s$entries[[i]]), "old snapshot retained only other-symbol unit")
      remember("stale_other_symbol_unit")
    }
  }
  if (kind == "unit_optional") {
    i = e$s_clause_idx_meta
    need(length(e$nu) == 1L && length(e$inso_column) == length(s$available), "first-name unit column shape")
    if (i > s$meta_idx_outer) remember("optional_future_short_circuit") else {
      mat = s$is_not_subset_of[[i]]
      need(is.matrix(mat) && e$nu %in% colnames(mat), "optional old matrix and frozen column")
      if (e$inso_column[[i]] && !mat[e$unit_idx_meta, e$nu]) {
        need(!e$s_clause_idx %in% s$symbol_registry[[e$nu]], "unit skip only current orphan")
        remember("orphan_unit_skip")
      }
    }
  }
  if (kind == "pre_hla") {
    check_state(s)
    for (name in names(s$unit_domains)) need(!length(s$symbol_registry[[name]]),
      "no current nonunit registration for a unit symbol")
    need(sum(s$is_unit & !s$eliminated) == length(s$unit_domains), "unit split cardinality")
    runtime$phase = "nonunit"
  }
  if (kind == "hla_select") {
    check_hla(s)
    if (!identical(runtime$hla_target, s$clause_idx)) {
      runtime$hla_target = s$clause_idx
      runtime$hla_loops = 0L
      runtime$hla_used = s$was_used
    }
    runtime$hla_loops = runtime$hla_loops + 1L
    need(runtime$hla_loops <= length(s$remaining_other_entries) + 1L &&
      all(s$was_used[runtime$hla_used]), "finite nonunit HLA donor progress")
    runtime$hla_used = s$was_used
  }
  if (kind == "hla_update") {
    if (s$updating_clause_meta_idx == s$meta_idx) {
      need(!s$is_not_subset_of[[s$meta_idx]][s$meta_idx, s$symbol], "HLA self row exclusion")
      remember("hla_self_guard")
    }
  }
  if (kind == "hla_repaired") {
    need(length(s$roe_idx) == 1L && valid(s$roe_idx, length(s$remaining_other_entries)),
      "HLA donor inverse consumed")
    check_hla(s)
    remember("hla_decrement")
  }
  if (kind == "unit_hla") {
    runtime$phase = "unit"
    need(length(s$unitsymbol) == 1L && !length(s$symbol_registry[[s$unitsymbol]]),
      "unit HLA has no registered donor")
    need(identical(as.numeric(s$not_subset_count), as.numeric(lengths(s$entries[s$remaining_nonunit_entries]))) &&
      all(s$not_subset_count >= 2L), "unit HLA cannot select a starting donor")
    need(is.na(match(TRUE, s$not_subset_count == 1L & !s$was_used)), "all-false unit selection mask")
    for (j in seq_along(s$remaining_nonunit_entries)) {
      i = s$remaining_nonunit_entries[[j]]
      lazy = names(s$entries[[i]]) != s$unitsymbol
      if (s$not_subset_count[[j]] != sum(lazy)) remember("unreachable_lazy_count_mismatch")
      if (!any(lazy)) remember("unreachable_all_false_lazy_row", list(entries = s$entries,
        eliminated = s$eliminated, registry = as.list(s$symbol_registry), donor = i,
        unit = s$unitsymbol, count = s$not_subset_count[[j]], hypothetical_lazy = lazy))
    }
    bump("unit_hla_initializations")
  }
  if (kind == "unit_body") need(FALSE, "unit HLA body reached")
}

# Source positions are pinned by the saved source hash and lexical inventory.
# Hooks read source locals; the original statements and evaluation order stay.
hooks = list()
before = function(line, expression) hooks[[as.character(line)]] <<- c(hooks[[as.character(line)]], expression)
for (spec in c("128:unit_visit", "133:unit_optional", "315:first_pivot", "395:oneend_pivots",
  "415:twoend_pivots", "437:two_match", "561:birth", "657:pre_hla", "686:hla_select",
  "691:hla_pivot", "707:hla_update", "712:hla_repaired", "743:unit_hla", "754:unit_body")) {
  parts = strsplit(spec, ":", fixed = TRUE)[[1L]]
  before(parts[[1L]], sprintf('.bo_hook("%s")', parts[[2L]]))
}
instrument = function(x) {
  if (!is.call(x)) return(x)
  p = as.list(x)
  op = as.character(p[[1L]])[[1L]]
  for (i in seq_along(p)[-1L]) {
    if (identical(p[i], list(quote(expr = )))) next
    p[i] = list(instrument(p[[i]]))
  }
  if (op == "if") p[2L] = list(call(".bo_scalar", p[[2L]], "if"))
  if (op %in% c("&&", "||")) for (i in 2:3) p[i] = list(call(".bo_scalar", p[[i]], op))
  if (op == "=" && is.symbol(p[[2L]]) && as.character(p[[2L]]) %in% helpers &&
    is.call(p[[3L]]) && identical(p[[3L]][[1L]], as.name("function"))) {
    fn = as.character(p[[2L]])
    body = p[[3L]][[3L]]
    p[[3L]][[3L]] = substitute({
      .bo_enter(FN, parent.env(environment()), environment())
      on.exit(.bo_exit(FN, parent.env(environment())), add = TRUE)
      BODY
    }, list(FN = fn, BODY = body))
  }
  as.call(p)
}
make_observed = function(lines = source_lines) {
  annotated = unlist(lapply(seq_along(lines), function(i) c(hooks[[as.character(i)]], lines[[i]])), use.names = FALSE)
  env = new.env(parent = globalenv())
  eval(instrument(parse(text = annotated)[[1L]]), env)
  env$simplify_cnf
}
observed = make_observed()

ctx = function(n) {
  universe = cnf$CnfUniverse()
  symbols = lapply(seq_len(n), function(i) cnf$CnfSymbol(universe, paste0("X", i), c("0", "1")))
  list(universe = universe, symbols = symbols)
}
public = function(words, context, character_selector = FALSE) {
  lapply(words, function(word) {
    stopifnot(length(word) > 0L, !anyNA(word), !any(-word %in% word))
    literals = unique(word)
    base = cnf$CnfClause(lapply(literals, function(lit) cnf$CnfAtom(
      context$symbols[[abs(lit)]], if (lit > 0L) "1" else "0")))
    selector = if (character_selector) paste0("X", abs(word)) else match(word, literals)
    answer = cnf$`[.CnfClause`(base, matrix(selector, nrow = 1L))
    stopifnot(identical(names(answer), paste0("X", abs(word))),
      identical(unname(c(answer)), lapply(word, function(lit) if (lit > 0L) "1" else "0")))
    answer
  })
}
run_case = function(words, label, fn = observed, compare = TRUE, char_selector = FALSE) {
  context = ctx(if (length(words)) max(1L, abs(unlist(words))) else 1L)
  clauses = public(words, context, char_selector)
  entries = lapply(clauses, c)
  runtime$words = words
  runtime$label = label
  runtime$W0 = sum(lengths(entries))
  runtime$last_weight = runtime$W0
  runtime$phase = "pair"
  runtime$stack = list()
  runtime$registered = integer()
  runtime$failed = FALSE
  runtime$max_depth = 0L
  runtime$hla_target = NULL
  universe = if (length(clauses)) context$universe else NULL
  answer = fn(entries, universe)
  need(length(runtime$stack) == 0L, "balanced helper exits")
  if (compare) {
    need(identical(answer, original(entries, universe)), "observed/unchanged kernel result")
    need(identical(answer, cnf$CnfFormula(clauses)), "direct public constructor result")
  }
  bump("cases")
  totals$maximum_helper_depth <<- max(c(totals$maximum_helper_depth, runtime$max_depth))
  invisible(answer)
}

started = proc.time()[[3L]]
all_false = list(c(-1L, 2L), c(3L, -2L), c(1L, 1L, 1L), c(-3L, -1L))
skip = list(c(-2L, 3L), c(-1L, -3L), c(3L, 2L, 1L, 1L), c(2L, -1L))
directed = list(list(), list(1L), list(1L, 1L), list(1L, -1L),
  list(c(1L, 2L)), list(1L, c(1L, 2L)), list(-1L, c(1L, 1L, 2L, 2L)),
  all_false, skip, list(c(1L, 2L), c(1L, 1L, 3L), c(-1L, -1L, 3L)),
  list(c(1L, 2L), c(1L, 3L), c(-3L, 4L)),
  list(c(1L, 1L, 2L), c(1L, 3L), c(-3L, 4L)),
  list(c(-1L, 2L), c(1L, 3L), c(1L, -3L), c(-2L, 4L, 4L)),
  list(c(1L, -2L), c(2L, -1L), c(1L, -3L), c(-1L, -4L), c(-1L, 4L)))
for (i in seq_along(directed)) run_case(directed[[i]], paste0("directed/", i))
for (base in list(all_false, skip, directed[[10L]])) {
  for (i in seq_along(base)) for (j in seq_along(base[[i]])) for (copies in c(1L, 2L, 5L)) {
    words = base
    words[[i]] = append(words[[i]], rep(words[[i]][[j]], copies), after = j)
    run_case(words, paste("repeated", i, j, copies), char_selector = copies == 2L)
    run_case(lapply(rev(words), rev), paste("reversed", i, j, copies), char_selector = TRUE)
  }
}
for (copies in c(2L, 3L, 16L, 64L, 192L)) {
  run_case(list(rep(1L, copies), rep(-1L, copies)), paste("pseudounits", copies))
  run_case(list(-1L, c(rep(1L, copies), rep(2L, copies))), paste("initial propagation", copies))
  words = all_false
  words[[3L]] = rep(1L, copies)
  run_case(words, paste("orphan width", copies))
}

# Exhaust all ordered clause lists through length three from the 16 proper
# words of physical width one or two over two Boolean symbols.
pool = as.list(c(-2L, -1L, 1L, 2L))
for (a in c(-2L, -1L, 1L, 2L)) for (b in c(-2L, -1L, 1L, 2L)) {
  if (a != -b) pool[[length(pool) + 1L]] = c(a, b)
}
stopifnot(length(pool) == 16L)
max_list = as.integer(Sys.getenv("CNF_TOTALITY_EXHAUSTIVE", "3"))
for (width in seq_len(max_list)) {
  for (code in 0:(length(pool)^width - 1L)) {
    positions = (code %/% length(pool)^(seq_len(width) - 1L)) %% length(pool) + 1L
    run_case(pool[positions], paste("two-symbol ordered", width, code))
  }
  cat("Completed exhaustive list width", width, "cases", totals$cases, "\n")
}
set.seed(6090622L)
trials = as.integer(Sys.getenv("CNF_TOTALITY_RANDOM", "700"))
for (k in seq_len(trials)) {
  n = sample.int(5L, 1L) + 1L
  words = lapply(seq_len(sample.int(7L, 1L) + 1L), function(i) {
    symbols = sample.int(n, sample.int(min(4L, n), 1L))
    signed = symbols * sample(c(-1L, 1L), length(symbols), replace = TRUE)
    word = rep(signed, sample.int(4L, length(symbols), replace = TRUE))
    word[sample.int(length(word))]
  })
  run_case(words, paste("random", k), char_selector = k %% 2L == 0L)
  if (k %% 100L == 0L) cat("Completed random", k, "elapsed", proc.time()[[3L]] - started, "\n")
}
positive = totals
positive_examples = examples

# Read-only observer sensitivity controls on privately corrupted source copies.
controls = list()
control = function(name, words, line, replacement) {
  stopifnot(!is.null(words))
  altered = source_lines
  altered[[line]] = replacement
  message = tryCatch({ run_case(words, paste("control", name), make_observed(altered), FALSE); NA_character_ },
    error = conditionMessage)
  stopifnot(!is.na(message))
  controls[[name]] <<- message
}
control("empty first-order pivot", list(c(1L, 2L), c(1L, -2L)), 314L,
  "symbol_to_restrict = character(0)")
control("deleted-column double decrement", all_false, 266L,
  "not_subset_count[meta_idx, rows_changed] <<- not_subset_count[meta_idx, rows_changed] - 2L")
control("unit-HLA forced donor", all_false, 745L,
  "hla_clause_idx = length(remaining_nonunit_entries)")
control("HLA double decrement", positive_examples$hla_decrement$words, 711L,
  "not_subset_count_current[[roe_idx]] = not_subset_count_current[[roe_idx]] - 2L")
control("future optional matrix read", positive_examples$optional_future_short_circuit$words, 133L,
  "if (inso_column[[s_clause_idx_meta]] && !is_not_subset_of[[s_clause_idx_meta]][unit_idx_meta, nu]) next")

result = list(R = R.version.string, counts = positive, examples = positive_examples,
  controls = controls, source_md5 = tools::md5sum(source_paths),
  seed = 6090622L, random = trials, exhaustive_list_width = max_list,
  seconds = proc.time()[[3L]] - started)
saveRDS(result, file.path(out_dir, paste0("observations_", tag, ".rds")))
print(result[c("R", "counts", "controls", "seconds")])
