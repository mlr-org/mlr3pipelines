# Read-only hooks in the exact diagnostic source copy. Production is unchanged.
# Run from the repository root with Rscript and an optional trial count.
bridge_lines = readLines("attic/cnf_verify3/independent_solver/r_bridge.R")
bridge_end = match("normalize_clauses = function(clauses) {", bridge_lines) - 1L
stopifnot(!is.na(bridge_end))
eval(parse(text = bridge_lines[seq_len(bridge_end)]))

review = new.env(parent = emptyenv())
record_transition = function(site, source, target, symbol, frame) {
  frame = parent.env(frame)
  if (frame$not_subset_count[source, target] != 1L) return(invisible(NULL))
  source_idx = frame$available[[source]]
  target_idx = frame$available[[target]]
  if (any(frame$eliminated[c(source_idx, target_idx)] | frame$is_unit[c(source_idx, target_idx)])) {
    return(invisible(NULL))
  }
  donor = frame$entries[[source_idx]]
  target_clause = frame$entries[[target_idx]]
  removed = setdiff(target_clause[[symbol]], donor[[symbol]])
  review$events[[length(review$events) + 1L]] = list(
    kind = "zero_to_one", site = site, source = source, target = target,
    symbol = symbol, donor = donor, target_clause = target_clause,
    removed = removed, second_order_enabled = frame$second_order_enabled,
    calls = vapply(sys.calls(), function(call) paste(deparse(call), collapse = " "), ""))
}
record_visit = function(source, target, second_order_only, frame) {
  frame = parent.env(frame)
  tracked = vapply(review$events, function(event) {
    event$kind == "zero_to_one" && event$source == source && event$target == target
  }, FALSE)
  if (!any(tracked)) return(invisible(NULL))
  review$events[[length(review$events) + 1L]] = list(
    kind = "visit", source = source, target = target,
    count = frame$not_subset_count[source, target],
    pivot = colnames(frame$is_not_subset_of[[source]])[frame$is_not_subset_of[[source]][target, ]],
    second_order_only = second_order_only)
}

hooked_src = replace_once(all_candidate_src,
  "          not_subset_count[other_meta_idx, meta_idx] <<- not_subset_count[other_meta_idx, meta_idx] + 1L",
  paste0(
    "          not_subset_count[other_meta_idx, meta_idx] <<- not_subset_count[other_meta_idx, meta_idx] + 1L\n",
    "          record_transition('range', other_meta_idx, meta_idx, symbol, environment())"))
hooked_src = replace_once(hooked_src,
  "        not_subset_count[meta_idx_other, meta_idx] <<- not_subset_count[meta_idx_other, meta_idx] + 1L",
  paste0(
    "        not_subset_count[meta_idx_other, meta_idx] <<- not_subset_count[meta_idx_other, meta_idx] + 1L\n",
    "        record_transition('deletion', meta_idx_other, meta_idx, symbol, environment())"))
hooked_src = replace_once(hooked_src,
  "    rowsum = not_subset_count[meta_idx, meta_idx_other]",
  paste0(
    "    rowsum = not_subset_count[meta_idx, meta_idx_other]\n",
    "    record_visit(meta_idx, meta_idx_other, second_order_only, environment())"))
eval(parse(text = hooked_src))
hooked_simplify = simplify_cnf

inspect_case = function(case) {
  review$events = list()
  result = hooked_simplify(case$clauses, case$domains)
  # Instrumentation must leave the exact diagnostic result unchanged.
  stopifnot(identical(result, combined_rescan_candidate(case$clauses, case$domains)))
  useful = which(vapply(review$events, function(event) {
    event$kind == "zero_to_one" && length(event$removed) > 0L
  }, FALSE))
  list(found = length(useful) > 0L, result = unclass(result), events = review$events)
}

arguments = commandArgs(trailingOnly = TRUE)
trials = if (length(arguments)) as.integer(arguments[[1L]]) else 5000L
set.seed(649813L)
found_case = NULL
for (trial in seq_len(trials)) {
  n_symbols = sample.int(3L, 1L) + 1L
  size = sample.int(4L, 1L) + 2L
  domains = setNames(rep(list(as.character(seq_len(size))), n_symbols), paste0("X", seq_len(n_symbols)))
  n_clauses = sample.int(10L, 1L) + 3L
  clauses = lapply(seq_len(n_clauses), function(i) {
    symbols = names(domains)[sample.int(n_symbols, sample.int(n_symbols - 1L, 1L) + 1L)]
    setNames(lapply(symbols, function(symbol) {
      domains[[symbol]][sample.int(size, sample.int(size - 1L, 1L))]
    }), symbols)
  })
  case = list(domains = domains, clauses = clauses)
  answer = inspect_case(case)
  if (answer$found) {
    found_case = case
    break
  }
}

if (is.null(found_case)) {
  cat("No useful zero-to-one transition in", trials, "cases. This is not an exclusion proof.\n")
} else {
  # Greedily reduce clauses, literals, and values while retaining the event.
  repeat {
    candidates = list()
    if (length(found_case$clauses) > 2L) {
      candidates = lapply(seq_along(found_case$clauses), function(i) {
        candidate = found_case
        candidate$clauses = candidate$clauses[-i]
        candidate
      })
    }
    for (i in seq_along(found_case$clauses)) {
      clause = found_case$clauses[[i]]
      for (symbol in names(clause)) {
        if (length(clause) > 1L) {
          candidate = found_case
          candidate$clauses[[i]][[symbol]] = NULL
          candidates[[length(candidates) + 1L]] = candidate
        }
        if (length(clause[[symbol]]) > 1L) {
          for (j in seq_along(clause[[symbol]])) {
            candidate = found_case
            candidate$clauses[[i]][[symbol]] = clause[[symbol]][-j]
            candidates[[length(candidates) + 1L]] = candidate
          }
        }
      }
    }
    improved = FALSE
    for (candidate in candidates) {
      if (inspect_case(candidate)$found) {
        found_case = candidate
        improved = TRUE
        break
      }
    }
    if (!improved) break
  }
  answer = inspect_case(found_case)
  result = c(list(discovery_trial = trial, case = found_case), answer)
  write_json(result, "attic/cnf_verify3/scheduler_review/zero_to_one.json",
    pretty = TRUE, auto_unbox = TRUE, null = "null")
  cat(toJSON(result, auto_unbox = TRUE, null = "null", pretty = TRUE), "\n")
}
