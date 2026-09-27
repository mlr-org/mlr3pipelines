# Independent controls for the fixed-point and pure-implication claims.
# Run from the repository root; no package functions or root scripts are edited.
source("R/CnfFormula_simplify.R")

output_dir = "attic/cnf_verify3/fixed_graph_review"
stats = list(graph_cases = 0L, duplicate_graph_cases = 0L,
  static_sse1_pairs = 0L, static_sse2_guard_candidates = 0L,
  fixed_inputs = 0L, productive_passes = 0L, nonproductive_passes = 0L,
  exact_checks = 0L, rule_audits = 0L, constant_audits = 0L,
  sort_only_passes = 0L, max_productive_passes = 0L)

bare = function(x) {
  attributes(x) = NULL
  x
}
mass = function(x) {
  if (is.logical(x)) return(0L)
  sum(vapply(x, function(clause) sum(lengths(clause)), 0L))
}
clause_key = function(x) paste(vapply(sort(names(x)), function(s) {
  paste0(s, "=", paste(sort(x[[s]]), collapse = ","))
}, ""), collapse = ";")
multiset = function(x) {
  if (is.logical(x)) return(if (x) "TRUE" else "FALSE")
  if (!length(x)) return("TRUE")
  sort(vapply(x, clause_key, ""))
}
is_subset = function(x, y) all(x %in% y)
full_truth = function(x, domains) {
  grid = expand.grid(domains, stringsAsFactors = FALSE)
  if (is.logical(x)) return(rep(x, nrow(grid)))
  good = rep(TRUE, nrow(grid))
  for (clause in x) {
    satisfied = rep(FALSE, nrow(grid))
    for (s in names(clause)) satisfied = satisfied | grid[[s]] %in% clause[[s]]
    good = good & satisfied
  }
  good
}

# A direct possible-value-domain oracle, with repeated full clause scans.
# It neither constructs virtual clauses nor uses production comparison rows.
refutes_target = function(target, donors, domains) {
  possible = lapply(names(domains), function(s) setdiff(domains[[s]], target[[s]]))
  names(possible) = names(domains)
  if (any(lengths(possible) == 0L)) return(TRUE)
  repeat {
    changed = FALSE
    for (donor in donors) {
      choices = names(donor)[vapply(names(donor), function(s) {
        any(donor[[s]] %in% possible[[s]])
      }, FALSE)]
      if (!length(choices)) return(TRUE)
      if (length(choices) != 1L) next
      s = choices[[1L]]
      reduced = intersect(possible[[s]], donor[[s]])
      if (length(reduced) != length(possible[[s]])) {
        possible[[s]] = reduced
        changed = TRUE
      }
    }
    if (!changed) return(FALSE)
  }
}

audit_rules = function(x, domains) {
  if (is.logical(x)) {
    stats$constant_audits <<- stats$constant_audits + 1L
    return(invisible(NULL))
  }
  count = length(x)
  symbols = names(domains)
  for (i in seq_len(count)) {
    target = x[[i]]
    others = setdiff(seq_len(count), i)
    stopifnot(!refutes_target(target, x[others], domains))
    for (j in others) {
      donor = x[[j]]
      if (length(donor) == 1L) {
        s = names(donor)
        stopifnot(is_subset(target[[s]], donor[[s]]),
          !is_subset(donor[[s]], target[[s]]))
      }
      contained = vapply(symbols, function(s) is_subset(donor[[s]], target[[s]]), FALSE)
      stopifnot(!all(contained))
      for (t in names(target)) if (all(contained[symbols != t])) {
        stopifnot(is_subset(target[[t]], donor[[t]]))
      }
    }
    # Full abstract SSE2 set premises, without the implementation's candidate
    # filters. Repeated donors are included as an additional boundary control.
    for (a in others) for (b in others) {
      donor_a = x[[a]]
      donor_b = x[[b]]
      for (t in names(target)) for (s in setdiff(symbols, t)) {
        off = setdiff(symbols, c(s, t))
        if (!all(vapply(off, function(v) {
          is_subset(donor_a[[v]], target[[v]]) && is_subset(donor_b[[v]], target[[v]])
        }, FALSE))) next
        if (!is_subset(intersect(donor_a[[s]], donor_b[[s]]), target[[s]])) next
        stopifnot(is_subset(target[[t]], union(donor_a[[t]], donor_b[[t]])))
      }
    }
  }
  stats$rule_audits <<- stats$rule_audits + 1L
  invisible(NULL)
}

check_to_fixed = function(input, domains) {
  original_truth = full_truth(input, domains)
  current = input
  initial_mass = mass(current)
  productive = 0L
  repeat {
    result = bare(simplify_cnf(current, domains))
    stopifnot(identical(original_truth, full_truth(result, domains)))
    if (identical(multiset(current), multiset(result))) {
      stats$nonproductive_passes <<- stats$nonproductive_passes + 1L
      if (!identical(current, result) && !is.logical(result)) {
        stats$sort_only_passes <<- stats$sort_only_passes + 1L
      }
      audit_rules(result, domains)
      stopifnot(identical(result, bare(simplify_cnf(result, domains))))
      stats$exact_checks <<- stats$exact_checks + 1L
      break
    }
    stopifnot(mass(result) < mass(current))
    productive = productive + 1L
    stopifnot(productive <= initial_mass)
    stats$productive_passes <<- stats$productive_passes + 1L
    current = result
  }
  stats$fixed_inputs <<- stats$fixed_inputs + 1L
  stats$max_productive_passes <<- max(stats$max_productive_passes, productive)
  invisible(result)
}

edges = function(n) {
  pairs = do.call(rbind, lapply(seq_len(n), function(i) {
    cbind(i, setdiff(seq_len(n), i))
  }))
  storage.mode(pairs) = "integer"
  pairs
}
edge_keys = function(e) if (nrow(e)) paste(e[, 1L], e[, 2L], sep = ":") else character()
as_clauses = function(e, reverse_names = FALSE) {
  lapply(seq_len(nrow(e)), function(i) {
    cl = setNames(list("0", "1"), paste0("X", e[i, ]))
    if (reverse_names && i %% 2L == 1L) cl = rev(cl)
    cl
  })
}
as_edges = function(x) {
  if (is.logical(x)) {
    stopifnot(x)
    return(matrix(integer(), ncol = 2L))
  }
  do.call(rbind, lapply(x, function(cl) {
    stopifnot(length(cl) == 2L, all(lengths(cl) == 1L),
      setequal(unlist(cl, use.names = FALSE), c("0", "1")))
    as.integer(sub("X", "", names(cl)[match(c("0", "1"), unlist(cl, use.names = FALSE))]))
  }))
}
# Breadth-first search provides a separate oracle from root's Warshall closure.
reachable = function(e, from, to) {
  pending = from
  visited = integer()
  while (length(pending)) {
    at = pending[[1L]]
    pending = pending[-1L]
    if (at == to) return(TRUE)
    if (at %in% visited) next
    visited = c(visited, at)
    pending = c(pending, setdiff(e[e[, 1L] == at, 2L], visited))
  }
  FALSE
}
greedy_edges = function(e) {
  e = e[!duplicated(edge_keys(e)), , drop = FALSE]
  active = rep(TRUE, nrow(e))
  for (i in seq_len(nrow(e))) {
    available = which(active & seq_len(nrow(e)) != i)
    if (reachable(e[available, , drop = FALSE], e[i, 1L], e[i, 2L])) active[[i]] = FALSE
  }
  e[active, , drop = FALSE]
}

# A source-prefix assertion catches any unexpected successful restriction.
# It requires every call to the restriction helper to have an absent pivot,
# stronger than merely comparing the final prefix with a deduplicated input.
prefix = simplify_cnf
body_parts = as.list(body(prefix))
is_assignment_to = function(expr, name) {
  is.call(expr) && identical(expr[[1L]], as.name("=")) && identical(expr[[2L]], as.name(name))
}
restriction_index = which(vapply(body_parts, is_assignment_to, FALSE, "apply_domain_restriction"))
restriction_function = body_parts[[restriction_index]][[3L]]
restriction_body = as.list(restriction_function[[3L]])
restriction_function[[3L]] = as.call(c(restriction_body[1L],
  list(quote(stopifnot(!symbol %in% names(entries[[clause_idx]])))), restriction_body[-1L]))
body_parts[[restriction_index]][[3L]] = restriction_function
boundary = which(vapply(body_parts, is_assignment_to, FALSE, "remaining_entries"))
stopifnot(length(restriction_index) == 1L, length(boundary) == 1L)
body(prefix) = as.call(c(body_parts[seq_len(boundary - 1L)],
  list(quote(return_entries(entries[!eliminated])))))

check_graph = function(e, n) {
  domains = setNames(rep(list(c("0", "1")), n), paste0("X", seq_len(n)))
  input = as_clauses(e, reverse_names = TRUE)
  actual = bare(simplify_cnf(input, domains))
  expected = greedy_edges(e)
  stopifnot(identical(edge_keys(as_edges(actual)), edge_keys(expected)),
    identical(edge_keys(as_edges(bare(prefix(input, domains)))),
      edge_keys(e[!duplicated(edge_keys(e)), , drop = FALSE])),
    identical(full_truth(input, domains), full_truth(actual, domains)),
    identical(actual, bare(simplify_cnf(actual, domains))))
  for (i in seq_len(nrow(expected))) {
    stopifnot(!reachable(expected[-i, , drop = FALSE], expected[i, 1L], expected[i, 2L]))
  }
  stats$graph_cases <<- stats$graph_cases + 1L
  stats$duplicate_graph_cases <<- stats$duplicate_graph_cases + as.integer(anyDuplicated(edge_keys(e)) != 0L)
  invisible(NULL)
}

# Static graph proof control: all pairs and triples of edges on four vertices.
static_clauses = as_clauses(edges(4L))
for (target in static_clauses) for (donor in static_clauses) {
  exceptional = names(donor)[!vapply(names(donor), function(s) is_subset(donor[[s]], target[[s]]), FALSE)]
  if (length(exceptional) == 1L) {
    stats$static_sse1_pairs = stats$static_sse1_pairs + 1L
    stopifnot(!exceptional %in% names(target))
  }
  if (length(exceptional) != 2L) next
  for (t in intersect(exceptional, names(target))) {
    s = setdiff(exceptional, t)
    for (other in static_clauses) {
      if (!s %in% names(other)) next
      if (!all(vapply(setdiff(names(other), c(s, t)), function(v) is_subset(other[[v]], target[[v]]), FALSE))) next
      if (is_subset(target[[t]], other[[t]]) || is_subset(target[[t]], donor[[t]])) next
      stats$static_sse2_guard_candidates = stats$static_sse2_guard_candidates + 1L
      stopifnot(!is_subset(intersect(other[[s]], donor[[s]]), target[[s]]))
    }
  }
}

edges3 = edges(3L)
# Every ordered edge multiset of length at most four, including duplicates.
visit_graphs = function(sequence = integer()) {
  check_graph(edges3[sequence, , drop = FALSE], 3L)
  if (length(sequence) == 4L) return(invisible(NULL))
  for (i in seq_len(nrow(edges3))) visit_graphs(c(sequence, i))
}
visit_graphs()
stopifnot(stats$graph_cases == sum(6L^(0:4)))
set.seed(202609061L)
for (i in seq_len(200L)) {
  n = 4L + (i %% 3L)
  e = edges(n)
  chosen = sample.int(nrow(e), sample.int(2L * nrow(e), 1L) - 1L, replace = TRUE)
  check_graph(e[chosen, , drop = FALSE], n)
}
cat("Graph controls complete:", stats$graph_cases, "cases\n")

# Complete clause sets on two ternary variables, up to three distinct clauses.
# Every nonempty proper subset is represented at each symbol, including
# overlapping ranges; each clause is given in reversed symbol/value order.
ternary_domains = list(A = c("0", "1", "2"), B = c("0", "1", "2"))
ranges = c(list(character()), lapply(1:6, function(mask) {
  ternary_domains[[1L]][bitwAnd(mask, bitwShiftL(1L, 0:2)) != 0L]
}))
clause_pool = list()
for (a in ranges) for (b in ranges) {
  if (!length(a) && !length(b)) next
  cl = list(B = rev(b), A = rev(a))
  clause_pool[[length(clause_pool) + 1L]] = cl[lengths(cl) > 0L]
}
stopifnot(length(clause_pool) == 48L)
check_to_fixed(list(), ternary_domains)
for (i in seq_along(clause_pool)) {
  check_to_fixed(clause_pool[i], ternary_domains)
  for (j in seq_len(i - 1L)) {
    check_to_fixed(clause_pool[c(i, j)], ternary_domains)
    for (k in seq_len(j - 1L)) check_to_fixed(clause_pool[c(j, k, i)], ternary_domains)
  }
}
cat("Ternary fixed-point controls complete:", stats$fixed_inputs, "inputs\n")

# Three-symbol Boolean clause sets up to three clauses include units, all
# polarities, and length-three targets with a genuinely distinct SSE2 pivot.
boolean_domains = setNames(rep(list(c("0", "1")), 3L), c("A", "B", "C"))
choices = list(character(), "0", "1")
boolean_pool = list()
for (a in choices) for (b in choices) for (c_value in choices) {
  cl = list(C = c_value, B = b, A = a)
  cl = cl[lengths(cl) > 0L]
  if (length(cl)) boolean_pool[[length(boolean_pool) + 1L]] = cl
}
stopifnot(length(boolean_pool) == 26L)
for (i in seq_along(boolean_pool)) {
  check_to_fixed(boolean_pool[i], boolean_domains)
  for (j in seq_len(i - 1L)) {
    check_to_fixed(boolean_pool[c(i, j)], boolean_domains)
    for (k in seq_len(j - 1L)) check_to_fixed(boolean_pool[c(j, k, i)], boolean_domains)
  }
}

# Directed boundaries: proper unit containment, sort-only storage change,
# duplicate units/clauses, and both scalar logical constants.
check_to_fixed(list(list(A = "0", B = "0"), list(A = c("0", "1"))), ternary_domains)
check_to_fixed(list(list(A = "0"), list(A = "0")), ternary_domains)
check_to_fixed(list(list(A = "0", B = "0"), list(A = "0", B = "0")), ternary_domains)
check_to_fixed(TRUE, ternary_domains)
check_to_fixed(FALSE, ternary_domains)

# Full-domain raw literals are intentionally outside the theorem. A sole
# tautological unit is an exact raw-kernel fixed point although the domain
# oracle refutes its negation without donors. Keep this as a premise control.
noncanonical = list(list(A = ternary_domains$A))
stopifnot(identical(noncanonical, bare(simplify_cnf(noncanonical, ternary_domains))),
  refutes_target(noncanonical[[1L]], list(), ternary_domains))

result = list(R_version = R.version.string,
  source_md5 = unname(tools::md5sum("R/CnfFormula_simplify.R")), stats = stats,
  noncanonical_full_domain_control = TRUE)
suffix = if (getRversion() < "4.0.0") "r36" else "r46"
saveRDS(result, file.path(output_dir, paste0("control_", suffix, ".rds")))
dput(result, file = file.path(output_dir, paste0("control_", suffix, ".txt")))
print(result)
