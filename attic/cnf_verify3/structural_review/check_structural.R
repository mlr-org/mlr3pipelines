# Independent finite checks of structural claims using installed public objects.
# Source copies are used only to return at the end of initial propagation.
.libPaths(c(normalizePath("attic/cnf_verify3/execution_modes/library"), .libPaths()))
suppressPackageStartupMessages(library(mlr3pipelines))
set.seed(20260906L + 124L)
save_dir = "attic/cnf_verify3/structural_review"
full_kernel = getFromNamespace("simplify_cnf", "mlr3pipelines")
prefix_kernel = full_kernel
statements = as.list(body(full_kernel))[-1L]
boundary = which(vapply(statements, function(x) is.call(x) &&
  identical(x[[1L]], as.name("=")) && identical(x[[2L]], as.name("available")), logical(1)))
stopifnot(length(boundary) == 1L)
body(prefix_kernel) = as.call(c(list(as.name("{")), statements[seq_len(boundary - 1L)],
  list(quote(return(return_entries(entries[!eliminated]))))))

truth = function(formula, grid) {
  if (is.logical(formula)) return(rep(as.vector(formula), nrow(grid)))
  answer = rep(TRUE, nrow(grid))
  for (clause in formula) {
    clause_answer = rep(FALSE, nrow(grid))
    for (i in seq_along(clause)) clause_answer = clause_answer |
      grid[[names(clause)[[i]]]] %in% clause[[i]]
    answer = answer & clause_answer
  }
  answer
}
holds = function(formula, point) {
  if (is.logical(formula)) return(as.vector(formula))
  all(vapply(formula, function(cl) any(vapply(names(cl), function(s)
    point[[s]] %in% cl[[s]], logical(1))), logical(1)))
}
range_at = function(clause, s) if (is.null(clause[[s]])) character() else clause[[s]]
contained = function(a, b) all(a %in% b)
normal = function(formula) {
  if (is.logical(formula)) return(as.vector(formula))
  sort(vapply(formula, function(cl) paste(vapply(sort(names(cl)), function(s)
    paste(s, paste(sort(cl[[s]]), collapse = ","), sep = "="), character(1)),
    collapse = ";"), character(1)))
}

# Fresh set-premise enumerator, independent of fixed_point.py.
local_opportunity = function(formula) {
  if (is.logical(formula) || length(formula) < 2L) return(NULL)
  all_symbols = unique(unlist(lapply(formula, names), use.names = FALSE))
  for (target_idx in seq_along(formula)) {
    target = formula[[target_idx]]
    donors = setdiff(seq_along(formula), target_idx)
    for (donor_idx in donors) {
      donor = formula[[donor_idx]]
      exceptions = names(donor)[!vapply(names(donor), function(s)
        contained(donor[[s]], range_at(target, s)), logical(1))]
      if (!length(exceptions)) return(list(kind = "subsumption", target = target_idx, donor = donor_idx))
      for (pivot in names(target)) if (all(exceptions %in% pivot) &&
          length(setdiff(target[[pivot]], range_at(donor, pivot))))
        return(list(kind = "SSE1", target = target_idx, donor = donor_idx, pivot = pivot))
    }
    for (a_idx in donors) for (b_idx in donors[donors >= a_idx]) {
      a = formula[[a_idx]]
      b = formula[[b_idx]]
      for (pivot in names(target)) {
        if (!length(setdiff(target[[pivot]], union(range_at(a, pivot), range_at(b, pivot))))) next
        for (intersection in setdiff(all_symbols, pivot)) {
          outside = setdiff(union(names(a), names(b)), c(pivot, intersection))
          if (!all(vapply(outside, function(s) contained(range_at(a, s), range_at(target, s)) &&
              contained(range_at(b, s), range_at(target, s)), logical(1)))) next
          if (contained(intersect(range_at(a, intersection), range_at(b, intersection)),
              range_at(target, intersection))) return(list(kind = "SSE2", target = target_idx,
                donors = c(a_idx, b_idx), pivot = pivot, intersection = intersection))
        }
      }
    }
  }
  NULL
}

# Straight domain propagation, independent of source HLA/comparison matrices.
refutes_without = function(formula, target_idx, domains) {
  possible = domains
  for (s in names(formula[[target_idx]])) possible[[s]] = setdiff(possible[[s]], formula[[target_idx]][[s]])
  if (any(lengths(possible) == 0L)) return(TRUE)
  repeat {
    changed = FALSE
    for (clause in formula[-target_idx]) {
      live = names(clause)[vapply(names(clause), function(s)
        any(clause[[s]] %in% possible[[s]]), logical(1))]
      if (!length(live)) return(TRUE)
      if (length(live) == 1L) {
        s = live[[1L]]
        reduced = intersect(possible[[s]], clause[[s]])
        if (length(reduced) < length(possible[[s]])) {
          possible[[s]] = reduced
          changed = TRUE
        }
      }
    }
    if (!changed) return(FALSE)
  }
}
hla_opportunity = function(formula, domains) {
  if (is.logical(formula)) return(NULL)
  for (i in seq_along(formula)) if (refutes_without(formula, i, domains)) return(i)
  NULL
}

graph_ranks = function(clauses) {
  if (is.logical(clauses) || !length(clauses)) return(integer())
  symbols = unique(unlist(lapply(clauses, names), use.names = FALSE))
  parent = seq_len(length(clauses) + length(symbols))
  find = function(i) { while (parent[[i]] != i) i = parent[[i]]; i }
  edge_nodes = list()
  for (i in seq_along(clauses)) for (s in names(clauses[[i]])) {
    j = length(clauses) + match(s, symbols)
    a = find(i)
    b = find(j)
    if (a != b) parent[[a]] = b
    edge_nodes[[length(edge_nodes) + 1L]] = c(i, j)
  }
  roots = vapply(seq_along(parent), find, integer(1))
  vapply(unique(roots), function(r) {
    vertices = sum(roots == r)
    edges = sum(vapply(edge_nodes, function(e) roots[[e[[1L]]]] == r, logical(1)))
    as.integer(edges - vertices + 1L)
  }, integer(1))
}

valid = function(formula, domains) {
  if (is.logical(formula)) return(length(formula) == 1L && !is.na(formula))
  all(vapply(formula, function(cl) length(cl) > 0L && !anyDuplicated(names(cl)) &&
    all(vapply(names(cl), function(s) s %in% names(domains) && is.character(cl[[s]]) &&
      length(cl[[s]]) > 0L && !anyDuplicated(cl[[s]]) &&
      contained(cl[[s]], domains[[s]]) && length(cl[[s]]) < length(domains[[s]]), logical(1))), logical(1)))
}
check_initial_properness = function(formula, domains) {
  if (is.logical(formula)) return(invisible(NULL))
  units = formula[lengths(formula) == 1L]
  unit_symbols = unlist(lapply(units, names), use.names = FALSE)
  stopifnot(!anyDuplicated(unit_symbols))
  allowed = domains
  for (unit in units) allowed[[names(unit)]] = unit[[1L]]
  for (clause in formula[lengths(formula) > 1L]) for (s in names(clause))
    stopifnot(contained(clause[[s]], allowed[[s]]), length(clause[[s]]) < length(allowed[[s]]))
}
check_no_unit_occurrences = function(formula) {
  if (is.logical(formula)) return(invisible(NULL))
  units = unlist(lapply(formula[lengths(formula) == 1L], names), use.names = FALSE)
  others = unlist(lapply(formula[lengths(formula) > 1L], names), use.names = FALSE)
  stopifnot(!anyDuplicated(units), !any(units %in% others))
}
make_objects = function(domains, clauses) {
  universe = CnfUniverse()
  symbols = lapply(names(domains), function(s) CnfSymbol(universe, s, domains[[s]]))
  names(symbols) = names(domains)
  objects = lapply(clauses, function(cl) CnfClause(lapply(names(cl), function(s) CnfAtom(symbols[[s]], cl[[s]]))))
  list(universe = universe, objects = objects)
}
essentiality = function(formula, grid, expected) {
  counts = c(clause_witnesses = 0L, value_witnesses = 0L)
  if (is.logical(formula)) return(counts)
  for (i in seq_along(formula)) {
    stopifnot(any(truth(formula[-i], grid) != expected))
    counts[[1L]] = counts[[1L]] + 1L
    for (s in names(formula[[i]])) for (value in formula[[i]][[s]]) {
      changed = formula
      changed[[i]][[s]] = setdiff(changed[[i]][[s]], value)
      stopifnot(any(truth(changed, grid) != expected))
      counts[[2L]] = counts[[2L]] + 1L
    }
  }
  counts
}

# General augmenting-path matcher; it does not use the tree/cycle construction.
matching_model = function(formula, domains) {
  if (is.logical(formula)) return(NULL)
  units = formula[lengths(formula) == 1L]
  clauses = formula[lengths(formula) > 1L]
  allowed = domains
  for (unit in units) allowed[[names(unit)]] = unit[[1L]]
  owner = setNames(integer(length(domains)), names(domains))
  visit = function(i) {
    for (s in names(clauses[[i]])) {
      if (seen[[s]]) next
      seen[[s]] <<- TRUE
      if (owner[[s]] == 0L || visit(owner[[s]])) {
        owner[[s]] <<- i
        return(TRUE)
      }
    }
    FALSE
  }
  for (i in seq_along(clauses)) {
    seen = setNames(rep(FALSE, length(domains)), names(domains))
    stopifnot(visit(i))
  }
  point = vapply(allowed, function(x) x[[1L]], character(1))
  for (s in names(owner)[owner > 0L]) point[[s]] = clauses[[owner[[s]]]][[s]][[1L]]
  stopifnot(holds(formula, point))
  point
}

stats = list()
counterexamples = list()
check_case = function(kind, domains, clauses, saturation = FALSE, recognition = FALSE,
    forest = FALSE, pseudoforest = FALSE, horn_default = NULL, blocks = NULL) {
  stopifnot(valid(clauses, domains))
  prepared = make_objects(domains, clauses)
  initial = c(prefix_kernel(lapply(prepared$objects, c), prepared$universe))
  output_object = CnfFormula(prepared$objects)
  output = c(output_object)
  grid = expand.grid(domains, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  expected = truth(clauses, grid)
  stopifnot(identical(truth(initial, grid), expected), identical(truth(output, grid), expected),
    valid(initial, domains), valid(output, domains))
  check_initial_properness(initial, domains)
  if (recognition) stopifnot(identical(isFALSE(initial), !any(expected)), identical(isFALSE(output), !any(expected)))
  current = if (is.null(stats[[kind]])) c(cases = 0L, worlds = 0L, unsatisfiable = 0L,
    clause_witnesses = 0L, value_witnesses = 0L, matching_models = 0L,
    broad_clause_inputs = 0L) else stats[[kind]]
  current[["cases"]] = current[["cases"]] + 1L
  current[["worlds"]] = current[["worlds"]] + nrow(grid)
  current[["unsatisfiable"]] = current[["unsatisfiable"]] + !any(expected)
  current[["broad_clause_inputs"]] = current[["broad_clause_inputs"]] + any(lengths(clauses) > 2L)
  if (forest) {
    stopifnot(all(graph_ranks(clauses) == 0L), identical(normal(initial), normal(output)))
    counts = essentiality(output, grid, expected)
    current[names(counts)] = current[names(counts)] + counts
  }
  if (pseudoforest && !is.logical(initial)) {
    stopifnot(all(graph_ranks(clauses) <= 1L))
    matching_model(initial, domains)
    current[["matching_models"]] = current[["matching_models"]] + 1L
  }
  if (!is.null(horn_default) && !is.logical(initial)) {
    check_no_unit_occurrences(initial)
    point = horn_default
    for (unit in initial[lengths(initial) == 1L]) point[[names(unit)]] = unit[[1L]]
    stopifnot(holds(initial, point))
  }
  if (!is.null(blocks) && !is.logical(output)) {
    check_no_unit_occurrences(output)
    for (clause in output) for (s in names(clause))
      stopifnot(any(vapply(blocks[[s]], function(block) setequal(clause[[s]], block), logical(1))))
    original_nonunits = normal(clauses[lengths(clauses) == 2L])
    stopifnot(all(normal(output[lengths(output) == 2L]) %in% original_nonunits))
  }
  if (saturation) {
    stopifnot(is.null(local_opportunity(output)), is.null(hla_opportunity(output, domains)))
    second = c(CnfFormula(as.list(output_object)))
    stopifnot(identical(normal(output), normal(second)))
  }
  stats[[kind]] <<- current
  invisible(list(initial = initial, output = output, models = sum(expected)))
}

pick = function(x, n = 1L) x[sample.int(length(x), n)]
proper_range = function(domain) pick(domain, sample.int(length(domain) - 1L, 1L))
randomize = function(clauses) {
  if (!length(clauses)) return(clauses)
  clauses = clauses[sample.int(length(clauses))]
  lapply(clauses, function(cl) cl[sample.int(length(cl))])
}
domains_for = function(n, max_size = 3L) setNames(lapply(seq_len(n), function(i)
  paste0("v", seq_len(sample.int(max_size - 1L, 1L) + 1L))), paste0("s", seq_len(n)))
label_supports = function(supports, domains) lapply(supports, function(support)
  setNames(lapply(support, function(s) proper_range(domains[[s]])), support))
add_units = function(clauses, domains, count) {
  for (j in seq_len(count)) {
    s = pick(names(domains))
    clauses[[length(clauses) + 1L]] = setNames(list(proper_range(domains[[s]])), s)
  }
  clauses
}

for (case in 1:240) {
  n = sample(3:6, 1L)
  domains = domains_for(n)
  reached = names(domains)[[1L]]
  fresh = names(domains)[-1L]
  supports = list()
  while (length(fresh)) {
    child_count = sample.int(min(3L, length(fresh)), 1L)
    children = fresh[seq_len(child_count)]
    supports[[length(supports) + 1L]] = c(pick(reached), children)
    reached = c(reached, children)
    fresh = fresh[-seq_len(child_count)]
  }
  clauses = randomize(add_units(label_supports(supports, domains), domains, sample(0:4, 1L)))
  check_case("forest", domains, clauses, saturation = TRUE, recognition = TRUE, forest = TRUE)
}
cat("Forest checks passed.\n")
print(stats$forest)

for (case in 1:240) {
  cycle = sample(2:4, 1L)
  n = cycle + sample(0:2, 1L)
  domains = domains_for(n)
  core = names(domains)[seq_len(cycle)]
  supports = lapply(seq_len(cycle), function(i) c(core[[i]], core[[i %% cycle + 1L]]))
  fresh = setdiff(names(domains), core)
  for (s in fresh) {
    if (sample(c(TRUE, FALSE), 1L)) {
      i = sample.int(length(supports), 1L)
      supports[[i]] = c(supports[[i]], s)
    } else supports[[length(supports) + 1L]] = c(pick(setdiff(names(domains), fresh)), s)
    fresh = setdiff(fresh, s)
  }
  clauses = randomize(add_units(label_supports(supports, domains), domains, sample(0:4, 1L)))
  stopifnot(all(graph_ranks(clauses) == 1L))
  check_case("pseudoforest", domains, clauses, recognition = TRUE, pseudoforest = TRUE)
}
cat("Pseudoforest checks passed.\n")
print(stats$pseudoforest)

for (case in 1:400) {
  n = sample(3:6, 1L)
  domains = setNames(rep(list(c("0", "1")), n), paste0("s", seq_len(n)))
  default = setNames(sample(c("0", "1"), n, replace = TRUE), names(domains))
  clauses = lapply(seq_len(sample(1:14, 1L)), function(i) {
    support = pick(names(domains), sample.int(min(5L, n), 1L))
    vals = default[support]
    if (sample(c(TRUE, FALSE), 1L)) {
      positive = sample.int(length(support), 1L)
      vals[[positive]] = if (vals[[positive]] == "0") "1" else "0"
    }
    as.list(vals)
  })
  check_case("renamable_horn", domains, randomize(clauses), recognition = TRUE, horn_default = default)
}
cat("Renamable Horn checks passed.\n")
print(stats$renamable_horn)

for (case in 1:300) {
  n = sample(3:4, 1L)
  symbols = paste0("s", seq_len(n))
  blocks = setNames(lapply(symbols, function(s) {
    sizes = sample(1:3, sample(2:3, 1L), replace = TRUE)
    split(paste0("v", seq_len(sum(sizes))), rep(seq_along(sizes), sizes))
  }), symbols)
  domains = lapply(blocks, unlist, use.names = FALSE)
  clauses = lapply(seq_len(sample(1:14, 1L)), function(i) {
    support = pick(symbols, sample(1:2, 1L))
    setNames(lapply(support, function(s) blocks[[s]][[sample.int(length(blocks[[s]]), 1L)]]), support)
  })
  check_case("equal_disjoint_binary", domains, randomize(clauses), saturation = TRUE, blocks = blocks)
}
cat("Equal/disjoint binary checks passed.\n")
print(stats$equal_disjoint_binary)

forcing_cycle = function(sign, stem) {
  list(setNames(list(sign, "1"), c("x", paste0(stem, 1L))),
    setNames(list("0", "1"), paste0(stem, 1:2)),
    setNames(list("0", "1"), paste0(stem, 2:3)),
    setNames(list("0", sign), c(paste0(stem, 3L), "x")))
}
single = forcing_cycle("1", "a")
domains = setNames(rep(list(c("0", "1")), 4L), c("x", paste0("a", 1:3)))
single_result = check_case("single_cycle_control", domains, single, saturation = TRUE, recognition = TRUE, pseudoforest = TRUE)
grid = expand.grid(domains, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
stopifnot(identical(normal(single), normal(single_result$output)), all(grid$x[truth(single, grid)] == "1"))
counterexamples$single_cycle = list(domains = domains, clauses = single, output = single_result$output,
  models = single_result$models, forced_symbol = "x", forced_value = "1", incidence_ranks = graph_ranks(single))
opposed = c(single, forcing_cycle("0", "b"))
domains = setNames(rep(list(c("0", "1")), 7L), c("x", paste0("a", 1:3), paste0("b", 1:3)))
opposed_result = check_case("opposed_cycles_control", domains, opposed, saturation = TRUE)
stopifnot(opposed_result$models == 0L, identical(normal(opposed), normal(opposed_result$output)),
  !isFALSE(opposed_result$initial))
grid = expand.grid(domains, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
stopifnot(all(vapply(seq_along(opposed), function(i) any(truth(opposed[-i], grid)), logical(1))))
counterexamples$opposed_cycles = list(domains = domains, clauses = opposed, output = opposed_result$output,
  models = opposed_result$models, incidence_ranks = graph_ranks(opposed))

saved = jsonlite::fromJSON("attic/cnf_verify3/independent_solver/minimized_first_order_phase_sse1.json", simplifyVector = FALSE)
domains = lapply(saved$domains, unlist, use.names = FALSE)
clauses = lapply(saved$clauses, function(cl) lapply(cl, unlist, use.names = FALSE))
overlap_result = check_case("overlapping_binary_control", domains, clauses)
stopifnot(all(lengths(clauses) == 2L), !is.null(local_opportunity(overlap_result$output)))
second_objects = make_objects(domains, overlap_result$output)
second = c(CnfFormula(second_objects$objects))
stopifnot(!identical(normal(second), normal(overlap_result$output)))
counterexamples$overlapping_binary = list(domains = domains, clauses = clauses, output = overlap_result$output,
  second = second, opportunity = local_opportunity(overlap_result$output), incidence_ranks = graph_ranks(clauses))

# Calibrate the new independent predicates with direct rule/refutation examples.
stopifnot(local_opportunity(list(list(x = "0"), list(x = "0", y = "0")))$kind == "subsumption")
stopifnot(!is.null(local_opportunity(list(list(x = "0", y = "0"), list(x = "1", y = "0")))))
stopifnot(refutes_without(list(list(x = "0"), list(x = "0", y = "0"), list(y = "1", x = "0")),
  1L, list(x = c("0", "1"), y = c("0", "1"))))
cat("Boundary examples and independent predicate calibrations passed.\n")
cat("R:", R.version.string, "; installed package:", as.character(packageVersion("mlr3pipelines")), "\n")
print(stats)
saveRDS(list(stats = stats, counterexamples = counterexamples, R = R.version.string,
  package = as.character(packageVersion("mlr3pipelines")), initial_boundary_statement = boundary),
  file.path(save_dir, "checks.rds"))
jsonlite::write_json(list(stats = lapply(stats, as.list), counterexamples = counterexamples, R = R.version.string,
  package = as.character(packageVersion("mlr3pipelines"))), file.path(save_dir, "checks.json"),
  pretty = TRUE, auto_unbox = TRUE)
cat("All structural review checks passed.\n")
