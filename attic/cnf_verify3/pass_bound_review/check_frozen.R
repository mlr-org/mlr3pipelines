# Reuse only this reviewer's previously independent scalar/set helpers, without
# rerunning the structural tests. No author's bridge/oracle is loaded.
helper_code = parse("attic/cnf_verify3/structural_review/check_structural.R")
first_test = which(vapply(as.list(helper_code), function(x) is.call(x) &&
  identical(x[[1L]], as.name("for")), logical(1)))[[1L]]
for (i in seq_len(first_test - 1L)) eval(helper_code[[i]], envir = .GlobalEnv)
save_dir = "attic/cnf_verify3/pass_bound_review"
set.seed(20260906L + 125L)

signature = function(s, clauses) {
  ranges = lapply(clauses, range_at, s = s)
  support = lengths(ranges) > 0L
  comparisons = unlist(lapply(seq_along(clauses), function(i) vapply(seq_along(clauses),
    function(j) contained(ranges[[i]], ranges[[j]]), logical(1))), use.names = FALSE)
  paste(c(as.integer(support), as.integer(comparisons)), collapse = "")
}
fiber_partition = function(s, clauses, domain) {
  patterns = vapply(domain, function(value) paste(vapply(clauses, function(cl)
    as.integer(value %in% range_at(cl, s)), integer(1)), collapse = ""), character(1))
  split(domain, patterns)
}
strip_ghosts = function(formula) {
  if (is.logical(formula)) return(formula)
  unname(lapply(formula, function(cl) { attr(cl, "review_ghost") = NULL; cl }))
}

context = new.env(parent = emptyenv())
context$mutation_checks = 0L
review_check_actual = function(entries) {
  context$mutation_checks = context$mutation_checks + 1L
  for (clause in entries) {
    id = attr(clause, "review_ghost")
    stopifnot(length(id) == 1L, id %in% seq_along(context$original))
    original = context$original[[id]]
    stopifnot(all(names(clause) %in% names(original)))
    for (s in context$frozen) stopifnot(identical(range_at(clause, s), range_at(original, s)))
    for (s in names(clause)) {
      stopifnot(contained(clause[[s]], original[[s]]))
      for (fiber in context$fibers[[s]]) stopifnot(!any(fiber %in% clause[[s]]) || all(fiber %in% clause[[s]]))
    }
  }
  invisible(NULL)
}
potential = function(formula) {
  if (is.logical(formula)) return(0L)
  length(formula) + sum(vapply(formula, function(cl) sum(vapply(intersect(names(cl), context$active),
    function(s) sum(vapply(context$fibers[[s]], function(f) any(f %in% cl[[s]]), logical(1))), integer(1))), integer(1)))
}

# Four hooks: initial reorder, old-unit intersection, nonempty restriction,
# and whole-literal deletion. The latter two share the same source line.
code = readLines("R/CnfFormula_simplify.R", warn = FALSE)
needles = c("entries = entries[order(lengths(entries))]",
  "entries[[ur]][[1L]] <<- (unit_domains[[nu]] = unit_isct)",
  "entries[[clause_idx]] <<- clause")
counts = vapply(needles, function(x) sum(grepl(x, code, fixed = TRUE)), integer(1))
stopifnot(identical(unname(counts), c(1L, 1L, 2L)))
for (needle in needles) code = gsub(needle, paste0(needle, "\n    review_check_actual(entries)"), code, fixed = TRUE)
instrumented_env = new.env(parent = environment(full_kernel))
instrumented_env$review_check_actual = review_check_actual
eval(parse(text = code), envir = instrumented_env)
instrumented = instrumented_env$simplify_cnf

records = list()
run_case = function(name, domains, clauses) {
  clauses = unname(clauses)
  stopifnot(valid(clauses, domains))
  occurring = unique(unlist(lapply(clauses, names), use.names = FALSE))
  signatures = setNames(vapply(occurring, signature, character(1), clauses), occurring)
  groups = split(occurring, signatures)
  context$frozen = unlist(groups[lengths(groups) >= 3L], use.names = FALSE)
  context$active = unlist(groups[lengths(groups) <= 2L], use.names = FALSE)
  context$original = clauses
  context$fibers = setNames(lapply(occurring, function(s) fiber_partition(s, clauses, domains[[s]])), occurring)
  tagged = lapply(seq_along(clauses), function(i) structure(clauses[[i]], review_ghost = i))
  initial_potential = potential(tagged)
  prepared = make_objects(domains, clauses)
  public = NULL
  current = tagged
  start_checks = context$mutation_checks
  masses = initial_potential
  productive = 0L
  full_worlds = prod(lengths(domains))
  grid = if (full_worlds <= 4096) expand.grid(domains, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE) else NULL
  expected = if (!is.null(grid)) truth(clauses, grid) else NULL
  truth_rows = 0L
  for (pass in seq_len(initial_potential + 2L)) {
    public = if (is.null(public)) CnfFormula(prepared$objects) else CnfFormula(as.list(public))
    output = c(instrumented(current, prepared$universe))
    stopifnot(identical(c(public), strip_ghosts(output)))
    if (!is.null(grid)) {
      stopifnot(identical(truth(output, grid), expected))
      truth_rows = truth_rows + nrow(grid)
    }
    new_potential = potential(output)
    masses = c(masses, new_potential)
    changed = !identical(normal(current), normal(output))
    if (!changed) break
    stopifnot(new_potential < tail(masses, 2L)[[1L]])
    productive = productive + 1L
    current = output
  }
  stopifnot(!changed, productive <= initial_potential)
  record = list(name = name, clauses = length(clauses), symbols = length(occurring),
    initial_signature_groups = unname(lengths(groups)), frozen_symbols = length(context$frozen),
    small_class_symbols = length(context$active), productive_passes = productive,
    potential = masses, actual_mutation_checks = context$mutation_checks - start_checks,
    full_worlds = full_worlds, truth_rows = truth_rows)
  records[[length(records) + 1L]] <<- record
  invisible(record)
}

# Threshold control: two same-signature symbols can change; three or more
# prohibit the first range change. These are separate variables, not aliases.
for (q in c(2L, 3L, 4L, 8L, 32L, 100L)) {
  domains = setNames(rep(list(c("a", "b", "c")), q), paste0("s", seq_len(q)))
  clauses = lapply(c("a", "b", "c"), function(v) setNames(rep(list(v), q), names(domains)))
  result = run_case(paste0("threshold_", q), domains, clauses)
  stopifnot(if (q == 2L) result$productive_passes > 0L else result$productive_passes == 0L)
  cat("Threshold q=", q, ": ", result$productive_passes, " productive passes, ", result$frozen_symbols, " frozen symbols.\n", sep = "")
}

# The signature deliberately records less than a membership profile.
domains = list(x = c("a", "b", "c"), y = c("a", "b", "c"), z = c("a", "b", "c", "d"))
clauses = list(list(x = "a", y = c("a", "b"), z = c("a", "b")),
  list(x = "b", y = c("b", "c"), z = c("b", "c")),
  list(x = "c", y = c("c", "a"), z = c("b", "d")))
result = run_case("same_signature_different_profiles", domains, clauses)
stopifnot(result$frozen_symbols == 3L, result$productive_passes == 0L)

# Rebuild the n=3 reverse family independently, then duplicate its guard
# into arbitrarily many distinct symbols while keeping the clause count 7.
for (guards in c(1L, 2L, 3L, 4L, 8L, 16L)) {
  domains = c(list(T = c("0", "1")), setNames(rep(list(c("0", "1")), 5L), paste0("S", 0:4)),
    setNames(rep(list(paste0("g", 0:3)), guards), paste0("G", seq_len(guards))))
  clauses = list(list(S0 = "0", S1 = "0"))
  for (i in 0:2) clauses[[length(clauses) + 1L]] = c(setNames(list("1", "1"), c(paste0("S", i), "T")),
    setNames(rep(list(paste0("g", i)), guards), paste0("G", seq_len(guards))))
  for (i in 3:1) clauses[[length(clauses) + 1L]] = c(setNames(list("0", "0", "0"), c(paste0("S", i), paste0("S", i + 1L), "T")),
    setNames(rep(list(paste0("g", 0:(i - 1L))), guards), paste0("G", seq_len(guards))))
  result = run_case(paste0("chain_guard_copies_", guards), domains, clauses)
  stopifnot(result$productive_passes == 3L)
  if (guards >= 3L) stopifnot(result$frozen_symbols >= guards)
  cat("Seven clauses, ", guards, " guard symbols: three productive passes; Phi ", paste(result$potential, collapse = " -> "), ".\n", sep = "")
}

for (case in 1:250) {
  m = sample(3:6, 1L)
  q = sample(3:7, 1L)
  domains = domains_for(q, 4L)
  clauses = lapply(seq_len(m), function(i) {
    support = names(domains)[runif(q) < 0.65]
    if (!length(support)) support = pick(names(domains))
    setNames(lapply(support, function(s) proper_range(domains[[s]])), support)
  })
  occurring = unique(unlist(lapply(clauses, names), use.names = FALSE))
  domains = domains[occurring]
  # Add at least two nonuniformly refined copies of one or two source symbols.
  bases = pick(occurring, sample.int(min(2L, length(occurring)), 1L))
  for (base in bases) for (copy in seq_len(sample(2:4, 1L))) {
    s = paste0(base, "_copy", copy)
    fibers = setNames(lapply(domains[[base]], function(v) paste0(v, "_", seq_len(sample(1:3, 1L)))), domains[[base]])
    domains[[s]] = unlist(fibers, use.names = FALSE)
    for (i in seq_along(clauses)) if (base %in% names(clauses[[i]]))
      clauses[[i]][[s]] = unlist(fibers[clauses[[i]][[base]]], use.names = FALSE)
  }
  result = run_case(paste0("random_", case), domains, randomize(clauses))
  stopifnot(result$frozen_symbols >= 3L)
  if (case %% 50L == 0L) cat("Random frozen-group cases: ", case, "/250.\n", sep = "")
}

summary = list(cases = length(records), actual_mutation_checks = context$mutation_checks,
  total_productive_passes = sum(vapply(records, function(x) x$productive_passes, integer(1))),
  frozen_symbol_occurrences = sum(vapply(records, function(x) x$frozen_symbols, integer(1))),
  truth_rows = sum(vapply(records, function(x) x$truth_rows, integer(1))),
  source_hook_counts = as.list(setNames(unname(counts), c("sort", "unit_merge", "range_or_literal_change"))),
  R = R.version.string, package = as.character(packageVersion("mlr3pipelines")))
saveRDS(list(summary = summary, records = records), file.path(save_dir, "frozen_checks.rds"))
jsonlite::write_json(list(summary = summary, records = records), file.path(save_dir, "frozen_checks.json"),
  pretty = TRUE, auto_unbox = TRUE)
print(summary)
cat("All frozen-symbol, whole-fiber, actual-subset, potential-descent, installed-source agreement, and bounded full-truth checks passed.\n")
