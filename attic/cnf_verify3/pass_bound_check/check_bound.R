# Independent checks of the finite-signature pass bound. Run from the repo root.
# Only base R is required. The production file is read, never changed.
options(warn = 2)
out_dir = "attic/cnf_verify3/pass_bound_check"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
expected_sha = "7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc"
sha = strsplit(system2("sha256sum", "R/CnfFormula_simplify.R", stdout = TRUE), " +")[[1L]][[1L]]
stopifnot(identical(sha, expected_sha))
source_env = new.env(parent = baseenv())
sys.source("R/CnfFormula_simplify.R", envir = source_env)
plain_kernel = source_env$simplify_cnf

stats = new.env(parent = emptyenv())
for (name in c("calls", "writes", "callbacks", "row_sums", "frozen_bits",
  "frozen_ranges", "fiber_ranges", "truth_rows", "raw_false_discrepancies",
  "raw_true_discrepancies", "sse2_calls", "ghosts_checked")) stats[[name]] = 0L
stats$write_sites = integer()
stats$callback_sites = integer()
context = new.env(parent = emptyenv())
records = list()

range_at = function(clause, symbol) {
  index = match(symbol, names(clause))
  if (is.na(index)) character() else clause[[index]]
}
included = function(x, y) all(!is.na(match(x, y)))
raw_formula = function(formula) {
  attributes(formula) = NULL
  formula
}
erase_ghosts = function(formula) {
  if (is.logical(formula)) return(formula)
  for (i in seq_along(formula)) attr(formula[[i]], "bound_check_id") = NULL
  formula
}
normal = function(formula) {
  formula = raw_formula(formula)
  if (is.logical(formula)) return(formula)
  sort(vapply(formula, function(clause) {
    paste(vapply(sort(names(clause)), function(s) paste(s,
      paste(sort(clause[[s]]), collapse = ","), sep = "="), ""), collapse = "|")
  }, ""))
}
signature = function(symbol, clauses) {
  ranges = lapply(clauses, range_at, symbol = symbol)
  pairs = unlist(lapply(seq_along(ranges), function(i) {
    vapply(setdiff(seq_along(ranges), i), function(j) included(ranges[[i]], ranges[[j]]), FALSE)
  }), use.names = FALSE)
  paste(as.integer(c(lengths(ranges) > 0L, pairs)), collapse = "")
}
fibers_at = function(symbol, clauses, domain) {
  bits = vapply(domain, function(value) paste(vapply(clauses, function(clause)
    as.integer(value %in% range_at(clause, symbol)), 0L), collapse = ""), "")
  split(domain, bits)
}
mass = function(formula, active_only) {
  formula = raw_formula(formula)
  if (is.logical(formula)) return(0)
  sum(vapply(formula, function(clause) {
    symbols = if (active_only) intersect(names(clause), context$active) else names(clause)
    sum(vapply(symbols, function(s) sum(vapply(context$fibers[[s]], function(fiber)
      any(fiber %in% clause[[s]]), FALSE)), 0L))
  }, 0))
}
potential = function(formula) {
  if (is.logical(formula)) return(0)
  length(formula) + mass(formula, TRUE)
}
truth = function(formula, grid) {
  formula = raw_formula(formula)
  if (is.logical(formula)) return(rep(formula, nrow(grid)))
  result = rep(TRUE, nrow(grid))
  for (clause in formula) {
    this = rep(FALSE, nrow(grid))
    for (s in names(clause)) this = this | !is.na(match(grid[[s]], clause[[s]]))
    result = result & this
  }
  result
}
increment_site = function(field, site) {
  counts = stats[[field]]
  if (!site %in% names(counts)) counts[[site]] = 0L
  counts[[site]] = counts[[site]] + 1L
  stats[[field]] = counts
}

# Inspect all materialized clauses, including clauses already flagged eliminated.
# This is stronger than checking only the final surviving ghost identities.
check_actual = function(entries, site) {
  stats$writes = stats$writes + 1L
  increment_site("write_sites", site)
  for (clause in entries) {
    id = attr(clause, "bound_check_id")
    stopifnot(length(id) == 1L, id %in% seq_along(context$original))
    original = context$original[[id]]
    stats$ghosts_checked = stats$ghosts_checked + 1L
    stopifnot(all(names(clause) %in% names(original)))
    for (s in context$frozen) {
      stopifnot(identical(range_at(clause, s), range_at(original, s)))
      stats$frozen_ranges = stats$frozen_ranges + 1L
    }
    for (s in names(clause)) {
      stopifnot(included(clause[[s]], original[[s]]))
      for (fiber in context$fibers[[s]]) {
        present = fiber %in% clause[[s]]
        stopifnot(!any(present) || all(present))
      }
      stats$fiber_ranges = stats$fiber_ranges + 1L
    }
  }
  invisible(NULL)
}

# Only helpers used before HLA call this hook. We deliberately make no assertion
# that ordinary counts track the virtual target comparisons written during HLA.
check_callback = function(site, frame) {
  stats$callbacks = stats$callbacks + 1L
  increment_site("callback_sites", site)
  matrix_list = get("is_not_subset_of", envir = frame, inherits = TRUE)
  if (is.null(matrix_list)) return(invisible(NULL))
  counts = get("not_subset_count", envir = frame, inherits = TRUE)
  available = get("available", envir = frame, inherits = TRUE)
  entries = get("entries", envir = frame, inherits = TRUE)
  pairs = which(!is.na(counts), arr.ind = TRUE)
  for (k in seq_len(nrow(pairs))) {
    donor = pairs[k, 1L]
    target = pairs[k, 2L]
    row = matrix_list[[donor]][target, ]
    stopifnot(counts[donor, target] == sum(row))
    stats$row_sums = stats$row_sums + 1L
    donor_clause = entries[[available[[donor]]]]
    target_clause = entries[[available[[target]]]]
    donor_id = attr(donor_clause, "bound_check_id")
    target_id = attr(target_clause, "bound_check_id")
    for (s in intersect(context$frozen, names(context$original[[donor_id]]))) {
      if (!included(context$original[[donor_id]][[s]], range_at(context$original[[target_id]], s))) {
        stopifnot(matrix_list[[donor]][target, s])
        stats$frozen_bits = stats$frozen_bits + 1L
      }
    }
    if (context$scan_raw) {
      eliminated = get("eliminated", envir = frame, inherits = TRUE)
      is_unit = get("is_unit", envir = frame, inherits = TRUE)
      if (!any(eliminated[available[c(donor, target)]] | is_unit[available[c(donor, target)]])) {
        raw = vapply(colnames(matrix_list[[donor]]), function(s)
          !included(range_at(donor_clause, s), range_at(target_clause, s)), FALSE)
        stats$raw_false_discrepancies = stats$raw_false_discrepancies + sum(!row & raw)
        stats$raw_true_discrepancies = stats$raw_true_discrepancies + sum(row & !raw)
      }
    }
  }
  if (site == "try_sse_2nd_order") {
    donor = get("meta_idx_twoends", envir = frame)
    target = get("meta_idx_target", envir = frame)
    symbol = get("symbol_target", envir = frame)
    stopifnot(counts[donor, target] == 2L, matrix_list[[donor]][target, symbol])
    stats$sse2_calls = stats$sse2_calls + 1L
  }
  invisible(NULL)
}

# Text additions occur only after complete statements or at function entry.
# Assert the exact source matches so a future source revision cannot silently
# alter which sites are checked. The ordinary function is run beside the copy.
code = readLines("R/CnfFormula_simplify.R", warn = FALSE)
add_after = function(needle, addition, expected = 1L) {
  locations = which(grepl(needle, code, fixed = TRUE))
  stopifnot(length(locations) == expected)
  for (i in rev(locations)) code <<- append(code, addition, after = i)
}
add_after("entries = entries[order(lengths(entries))]", '  check_actual(entries, "sort")')
add_after("entries[[ur]][[1L]] <<- (unit_domains[[nu]] = unit_isct)", '      check_actual(entries, "old_unit_intersection")')
add_after("entries[[clause_idx]] <<- clause", '    check_actual(entries, "range_or_symbol")', 2L)
for (site in c("register_unit", "apply_domain_restriction", "eliminate_symbol_from_clause",
  "on_updated_subset_relations", "on_update_range", "handle_sse_2nd_order_oneend",
  "handle_sse_2nd_order_twoend", "try_sse_2nd_order")) {
  add_after(paste0("  ", site, " = function("),
    paste0('    check_callback("', site, '", environment())'))
}
instrument_env = new.env(parent = source_env)
instrument_env$check_actual = check_actual
instrument_env$check_callback = check_callback
eval(parse(text = code), envir = instrument_env)
checked_kernel = instrument_env$simplify_cnf

run_case = function(label, domains, clauses, scan_raw = FALSE, max_worlds = 2048L) {
  context$original = clauses
  context$scan_raw = scan_raw
  if (!is.logical(clauses)) {
    stopifnot(length(clauses) > 0L)
    for (clause in clauses) {
      stopifnot(length(clause) > 0L, !anyDuplicated(names(clause)))
      for (s in names(clause)) stopifnot(length(clause[[s]]) > 0L,
        length(clause[[s]]) < length(domains[[s]]), !anyDuplicated(clause[[s]]),
        included(clause[[s]], domains[[s]]))
    }
    occurring = unique(unlist(lapply(clauses, names), use.names = FALSE))
    groups = split(occurring, vapply(occurring, signature, "", clauses = clauses))
    context$frozen = unlist(groups[lengths(groups) >= 3L], use.names = FALSE)
    context$active = unlist(groups[lengths(groups) <= 2L], use.names = FALSE)
    context$fibers = setNames(lapply(occurring, function(s) fibers_at(s, clauses, domains[[s]])), occurring)
    tagged = lapply(seq_along(clauses), function(i) structure(clauses[[i]], bound_check_id = i))
    support = vapply(context$active, function(s) sum(vapply(clauses, function(cl)
      s %in% names(cl), FALSE)), 0L)
    sharper = length(clauses) + sum(support * 2^(support - 1L))
  } else {
    stopifnot(length(clauses) == 1L, !is.na(clauses))
    occurring = context$frozen = context$active = character()
    context$fibers = groups = list()
    tagged = clauses
    sharper = 0
  }
  universe = list2env(domains, parent = emptyenv())
  current = structure(tagged, universe = universe, class = "CnfFormula")
  plain_current = structure(clauses, universe = universe, class = "CnfFormula")
  initial_potential = potential(current)
  stopifnot(initial_potential <= sharper)
  masses = initial_potential
  fiber_masses = mass(current, FALSE)
  worlds = prod(lengths(domains))
  grid = if (worlds <= max_worlds) expand.grid(domains,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) else NULL
  expected = if (!is.null(grid)) truth(current, grid) else NULL
  productive = order_only = logical()
  first_nonproductive = NULL
  before = as.list(stats)
  repeat {
    output = checked_kernel(current, universe)
    plain_output = plain_kernel(plain_current, universe)
    stats$calls = stats$calls + 1L
    stopifnot(identical(erase_ghosts(output), plain_output))
    changed = !identical(normal(current), normal(output))
    masses = c(masses, potential(output))
    fiber_masses = c(fiber_masses, mass(output, FALSE))
    stopifnot(tail(masses, 1L) <= tail(masses, 2L)[[1L]],
      tail(fiber_masses, 1L) <= tail(fiber_masses, 2L)[[1L]])
    if (changed) stopifnot(tail(masses, 1L) < tail(masses, 2L)[[1L]],
      tail(fiber_masses, 1L) < tail(fiber_masses, 2L)[[1L]])
    productive = c(productive, changed)
    order_only = c(order_only, !changed && !identical(current, output))
    if (!is.null(grid)) {
      stopifnot(identical(truth(output, grid), expected))
      stats$truth_rows = stats$truth_rows + nrow(grid)
    }
    if (!is.null(first_nonproductive)) {
      stopifnot(!changed, identical(current, output))
      break
    }
    if (!changed) first_nonproductive = length(productive)
    stopifnot(sum(productive) <= initial_potential,
      length(productive) <= initial_potential + 2L)
    current = output
    plain_current = plain_output
  }
  record = list(label = label, clauses = if (is.logical(clauses)) 0L else length(clauses),
    symbols = length(occurring), group_sizes = lengths(groups), frozen = context$frozen,
    productive = productive, order_only = order_only, potential = masses,
    fiber_mass = fiber_masses, worlds = worlds, truth_checked = !is.null(grid),
    callbacks = stats$callbacks - before$callbacks, writes = stats$writes - before$writes,
    raw_false_discrepancies = stats$raw_false_discrepancies - before$raw_false_discrepancies,
    raw_true_discrepancies = stats$raw_true_discrepancies - before$raw_true_discrepancies)
  records[[length(records) + 1L]] <<- record
  invisible(record)
}

# Terminal constants, contradictory units, duplicate frozen clauses, and a
# clause deletion which removes arbitrarily many frozen value occurrences.
binary = c("0", "1")
run_case("constant_true", list(x = binary), TRUE)
run_case("constant_false", list(x = binary), FALSE)
run_case("contradictory_units", list(x = binary), list(list(x = "0"), list(x = "1")))
for (q in c(3L, 8L, 64L)) {
  domains = setNames(rep(list(c("a", "b", "c")), q), paste0("f", seq_len(q)))
  clause = setNames(rep(list(c("a", "b")), q), names(domains))
  result = run_case(paste0("duplicate_frozen_", q), domains, list(clause, clause))
  stopifnot(length(result$frozen) == q, identical(result$potential, c(2, 1, 1, 1)))
}

# All three-clause range profiles with an unused domain value are grouped by
# support/inclusion signature. Test every ordered pair of profiles in each
# class, with the first class member as the third symbol's profile. This is
# 1,621 cases over all 44 signatures, not all 48,241 ordered profile triples.
profiles = lapply(1:127, function(mask) {
  patterns = c(0L, (1:7)[bitwAnd(mask, bitwShiftL(1L, 0:6)) != 0L])
  list(domain = paste0("v", patterns), ranges = lapply(0:2, function(i) {
    selected = patterns[bitwAnd(patterns, bitwShiftL(1L, i)) != 0L]
    if (length(selected)) paste0("v", selected) else character()
  }))
})
profile_signature = function(profile) {
  clauses = lapply(profile$ranges, function(r) if (length(r)) list(s = r) else list())
  signature("s", clauses)
}
profile_groups = split(seq_along(profiles), vapply(profiles, profile_signature, ""))
stopifnot(length(profile_groups) == 44L, sum(lengths(profile_groups)^2L) == 1621L)
case_index = 0L
for (group in profile_groups) for (a in group) for (b in group) {
  chosen = profiles[c(a, b, group[[1L]])]
  domains = setNames(lapply(chosen, `[[`, "domain"), c("x", "y", "z"))
  clauses = lapply(seq_len(3L), function(i) {
    ranges = setNames(lapply(chosen, function(profile) profile$ranges[[i]]), c("x", "y", "z"))
    ranges[lengths(ranges) > 0L]
  })
  for (i in seq_len(3L)) if (!length(clauses[[i]])) {
    symbol = paste0("filler", i)
    domains[[symbol]] = binary
    clauses[[i]][[symbol]] = "0"
  }
  case_index = case_index + 1L
  result = run_case(paste0("profile_pair_", case_index), domains, clauses)
  stopifnot(all(c("x", "y", "z") %in% result$frozen))
  if (case_index %% 400L == 0L) cat("Profile pairs:", case_index, "of 1621\n")
}

# Threshold controls use pairwise-incomparable triples. At two symbols a
# proper range can shrink; at three every group member is frozen.
for (q in c(2L, 3L, 5L)) {
  domains = setNames(rep(list(c("a", "b", "c", "d")), q), paste0("t", seq_len(q)))
  ranges = list(c("a", "b"), c("b", "c"), c("b", "d"))
  clauses = lapply(ranges, function(r) setNames(rep(list(r), q), names(domains)))
  result = run_case(paste0("threshold_", q), domains, clauses, TRUE)
  stopifnot(if (q == 2L) sum(result$productive) == 1L else sum(result$productive) == 0L)
}

# Random interfering clauses plus three antichain realizations which share
# support/inclusion signatures but use independently chosen higher-order
# overlaps, domain sizes, orders and unequal positive fiber multiplicities.
set.seed(6192026L)
pick = function(x, n = 1L) x[sample.int(length(x), n)]
for (trial in seq_len(400L)) {
  m = sample(4:9, 1L)
  q = sample(2:5, 1L)
  domains = setNames(lapply(seq_len(q), function(i) paste0("v", seq_len(sample(3:5, 1L)))), paste0("r", seq_len(q)))
  clauses = lapply(seq_len(m), function(i) {
    symbols = names(domains)[runif(q) < 0.65]
    if (!length(symbols)) symbols = pick(names(domains))
    setNames(lapply(symbols, function(s) pick(domains[[s]], sample.int(length(domains[[s]]) - 1L, 1L))), symbols)
  })
  support = sort(pick(seq_len(m), sample.int(m - 1L, 1L)))
  for (copy in seq_len(3L)) {
    symbol = paste0("frozen", copy)
    membership = diag(length(support)) != 0L
    extra = matrix(runif(length(support) * sample(1:5, 1L)) < 0.5, nrow = length(support))
    membership = cbind(FALSE, membership, extra)
    multiplicities = sample(1:3, ncol(membership), replace = TRUE)
    membership = membership[, rep(seq_len(ncol(membership)), multiplicities), drop = FALSE]
    domain = paste0("g", seq_len(ncol(membership)))
    domains[[symbol]] = pick(domain, length(domain))
    for (i in seq_along(support)) clauses[[support[[i]]]][[symbol]] = pick(domain[membership[i, ]], sum(membership[i, ]))
  }
  clauses = lapply(pick(clauses, length(clauses)), function(cl) cl[sample.int(length(cl))])
  result = run_case(paste0("interference_", trial), domains, clauses,
    scan_raw = trial <= 100L, max_worlds = 512L)
  stopifnot(all(paste0("frozen", 1:3) %in% result$frozen))
  if (trial %% 100L == 0L) cat("Interference cases:", trial, "of 400\n")
}

# Directed transient-cache controls are historical fixtures, separately
# identified from the independently generated examples above.
fixtures = c("attic/cnf_verify3/proof_state/results/decision_failure_seed27_trial1747.rds",
  "attic/cnf_verify3/root/queued_comparison_control_seed_case.rds")
for (path in fixtures) {
  fixture = readRDS(path)
  domains = if (!is.null(fixture$input)) fixture$input$uinfo$domains else fixture$domains
  clauses = if (!is.null(fixture$input)) fixture$input$clauses else fixture$clauses
  for (copy in seq_len(3L)) domains[[paste0("isolated", copy)]] = binary
  clauses[[length(clauses) + 1L]] = setNames(rep(list("0"), 3L), paste0("isolated", 1:3))
  result = run_case(paste0("fixture_", basename(path)), domains, clauses, TRUE)
  stopifnot(all(paste0("isolated", 1:3) %in% result$frozen))
  cat("Transient fixture:", basename(path), "raw FALSE discrepancies =",
    result$raw_false_discrepancies, "raw TRUE discrepancies =", result$raw_true_discrepancies, "\n")
}

# The known seven-clause example distinguishes three productive calls,
# one sorting-only call and the next call which observes exact equality.
domains = list(G = paste0("g", 0:3), T = binary, S0 = binary, S1 = binary, S2 = binary)
clauses = list(list(S0 = "0", S1 = "0"), list(S0 = "1", T = "1", G = "g0"),
  list(S1 = "1", T = "1", G = "g1"), list(S2 = "1", T = "1"),
  list(T = "0", G = c("g0", "g1", "g2")),
  list(S2 = "0", T = "0", G = c("g0", "g1")),
  list(S1 = "0", S2 = "0", T = "0", G = "g0"))
result = run_case("three_productive_then_sort", domains, clauses, TRUE)
stopifnot(identical(result$productive, c(TRUE, TRUE, TRUE, FALSE, FALSE)),
  identical(result$order_only, c(FALSE, FALSE, FALSE, TRUE, FALSE)))

summary = c(list(R = R.version.string, sha256 = sha, cases = length(records),
  profile_signatures = length(profile_groups), profile_pairs = case_index,
  productive_passes = sum(vapply(records, function(x) sum(x$productive), 0L)),
  sorting_only_calls = sum(vapply(records, function(x) sum(x$order_only), 0L))), as.list(stats))
saveRDS(list(summary = summary, records = records), file.path(out_dir, "checks.rds"))
dput(summary, file = file.path(out_dir, "summary.R"))
print(summary)
cat("All paired-output, first-change, fiber, row-count, potential and stability checks passed.\n")
