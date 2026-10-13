# Recursive decision-boundary invariants, independent of phase-boundary I5.
#
# Run from the repository root. No production source is changed.
#   CNF_TRIALS=1000 CNF_SEED=27 Rscript attic/cnf_verify3/proof_state/exp02_decision_invariants.R
#
# The previous campaign's generators are reused deliberately; the new venue
# of attack is the transient recursive state, not a new random distribution.
# The truth evaluator here is small and independently implemented.
source("attic/cnf_verify/harness.R")
source_cnf()

audit_dir = "attic/cnf_verify3/proof_state"
dir.create(file.path(audit_dir, "results"), recursive = TRUE, showWarnings = FALSE)
audit = new.env(parent = emptyenv())
audit$totals = list()
audit$first_raw_stale = NULL
audit$first_deferred_skip = NULL
audit$require_properness = identical(Sys.getenv("CNF_AUDIT_PROPERNESS", "true"), "true")
audit$check_matrix_debts = identical(Sys.getenv("CNF_AUDIT_MATRIX_DEBTS", "false"), "true")
audit$first_matrix_debt = NULL

bump = function(name, amount = 1L) {
  previous = audit$totals[[name]]
  if (is.null(previous)) previous = 0L
  audit$totals[[name]] = previous + amount
}

models = function(entries, assignments) {
  if (is.logical(entries)) return(rep(entries, nrow(assignments)))
  answer = rep(TRUE, nrow(assignments))
  for (clause in entries) {
    answer_clause = rep(FALSE, nrow(assignments))
    for (symbol in names(clause)) {
      answer_clause = answer_clause | assignments[[symbol]] %in% clause[[symbol]]
    }
    answer = answer & answer_clause
  }
  answer
}

abort_audit = function(message, state = NULL, local = NULL) {
  audit$failed = TRUE
  if (!is.null(state)) {
    audit$failure_state = list(entries = state$entries,
      eliminated = state$eliminated, is_unit = state$is_unit,
      units = as.list(state$unit_domains),
      symbol_registry = as.list(state$symbol_registry),
      stack = audit$stack)
  }
  stop(message, call. = FALSE)
}

assert_global_equivalence = function(state, event) {
  current = models(state$entries[!state$eliminated], audit$assignments)
  if (!identical(current, audit$initial_models)) {
    abort_audit(paste("transient semantic mismatch:", event), state)
  }
  bump("equivalence_checks")
}

effective = function(state, clause, symbol) {
  values = clause[[symbol]]
  unit = state$unit_domains[[symbol]]
  if (is.null(unit)) values else values[values %in% unit]
}

check_pair = function(state, source, target, event, symbols = NULL, allow_inactive = FALSE) {
  if (is.null(state$is_not_subset_of)) return(invisible(NULL))
  if (is.na(source) || is.na(target)) return(invisible(NULL))
  indices = state$available[c(source, target)]
  if (!allow_inactive && any(state$eliminated[indices] | state$is_unit[indices])) {
    bump("pair_guard_exits")
    return(invisible(NULL))
  }
  count = state$not_subset_count[source, target]
  if (is.na(count)) return(invisible(NULL))
  matrix_source = state$is_not_subset_of[[source]]
  row = matrix_source[target, ]
  if (count != sum(row)) {
    abort_audit(sprintf("matrix count mismatch at %s: %d -> %d", event, source, target), state)
  }
  source_clause = state$entries[[state$available[[source]]]]
  target_clause = state$entries[[state$available[[target]]]]
  certified_symbols = colnames(matrix_source)[!row]
  if (!is.null(symbols)) certified_symbols = intersect(certified_symbols, symbols)
  for (symbol in certified_symbols) {
    source_effective = effective(state, source_clause, symbol)
    target_effective = effective(state, target_clause, symbol)
    if (!all(source_effective %in% target_effective)) {
      abort_audit(sprintf("invalid projected subset certificate at %s: meta %d -> %d, symbol %s",
        event, source, target, symbol), state)
    }
    if (!all(source_clause[[symbol]] %in% target_clause[[symbol]])) {
      bump("raw_stale_positive")
      if (is.null(audit$first_raw_stale)) {
        audit$first_raw_stale = list(trial = audit$trial, event = event,
          source = source, target = target, symbol = symbol,
          source_clause = source_clause, target_clause = target_clause,
          units = as.list(state$unit_domains), stack = audit$stack,
          input = audit$input)
      }
    }
    bump("projected_certificates")
  }
  bump("pair_checks")
  invisible(NULL)
}

check_unit_boundary = function(state, symbol) {
  # An inner same-symbol registration may rely on pending work in its parent.
  # Only the outermost completed registration must leave every raw occurrence
  # restricted to a proper subset of the effective unit range.
  unit = state$unit_domains[[symbol]]
  for (clause_idx in state$symbol_registry[[symbol]]) {
    if (state$eliminated[[clause_idx]]) next
    values = state$entries[[clause_idx]][[symbol]]
    if (!all(values %in% unit) || (audit$require_properness && length(values) >= length(unit))) {
      abort_audit(sprintf("outermost unit propagation incomplete: %s in clause %d",
        symbol, clause_idx), state)
    }
  }
  bump("outermost_unit_postconditions")
}

check_matrix_debts = function(state, event) {
  if (!audit$check_matrix_debts || audit$in_hla || is.null(state$is_not_subset_of)) {
    return(invisible(NULL))
  }
  # Singleton candidates have a brief transition before register_unit marks
  # them inactive. No production pairwise callback runs in that transition.
  active = which(!state$eliminated[state$available] &
    !state$is_unit[state$available] & lengths(state$entries[state$available]) > 1L)
  for (source in active) for (target in active) {
    if (is.na(state$not_subset_count[source, target])) next
    source_idx = state$available[[source]]
    target_idx = state$available[[target]]
    matrix_source = state$is_not_subset_of[[source]]
    row = matrix_source[target, ]
    if (state$not_subset_count[source, target] != sum(row)) {
      abort_audit(paste("matrix count mismatch in debt audit at", event), state)
    }
    source_clause = state$entries[[source_idx]]
    target_clause = state$entries[[target_idx]]
    for (symbol in colnames(matrix_source)) {
      if (!row[[symbol]]) {
        if (!all(effective(state, source_clause, symbol) %in%
            effective(state, target_clause, symbol))) {
          abort_audit(paste("projected FALSE mismatch in debt audit at", event), state)
        }
        next
      }
      if (!all(source_clause[[symbol]] %in% target_clause[[symbol]])) next
      owner = vapply(audit$stack, function(frame) {
        identical(frame$fn, "apply_domain_restriction") &&
          identical(frame$clause_idx, source_idx) && identical(frame$symbol, symbol)
      }, logical(1))
      if (!any(owner)) {
        abort_audit(sprintf("unowned raw TRUE debt at %s: %d -> %d on %s",
          event, source_idx, target_idx, symbol), state)
      }
      bump("owned_raw_true_debts")
      if (is.null(audit$first_matrix_debt)) {
        audit$first_matrix_debt = list(trial = audit$trial, event = event,
          source = source_idx, target = target_idx, symbol = symbol,
          source_clause = source_clause, target_clause = target_clause,
          stack = audit$stack, input = audit$input)
      }
    }
    bump("matrix_debt_pair_checks")
  }
  invisible(NULL)
}

decision_enter = function(state, local, fn) {
  if (audit$failed) return(0L)
  audit$serial = audit$serial + 1L
  token = audit$serial
  symbol = if (fn == "register_unit") names(state$entries[[local$unit_idx]]) else
    if (fn %in% c("apply_domain_restriction", "eliminate_symbol_from_clause")) local$symbol else NULL
  clause_idx = if (fn %in% c("apply_domain_restriction", "eliminate_symbol_from_clause")) local$clause_idx else NULL
  audit$stack[[length(audit$stack) + 1L]] = list(token = token, fn = fn,
    symbol = symbol, clause_idx = clause_idx)
  bump(paste0("enter_", fn))
  assert_global_equivalence(state, paste("enter", fn))
  check_matrix_debts(state, paste("enter", fn))
  if (fn == "on_updated_subset_relations") {
    indices = state$available[c(local$meta_idx, local$meta_idx_other)]
    if (any(state$eliminated[indices] | state$is_unit[indices])) {
      abort_audit("on_updated_subset_relations received inactive clause", state)
    }
    check_pair(state, local$meta_idx, local$meta_idx_other, fn)
  } else if (fn %in% c("handle_sse_2nd_order_oneend", "handle_sse_2nd_order_twoend")) {
    check_pair(state, local$meta_idx, local$meta_idx_target, fn)
  } else if (fn == "try_sse_2nd_order") {
    indices = state$available[c(local$meta_idx_oneend, local$meta_idx_twoends, local$meta_idx_target)]
    if (any(state$eliminated[indices] | state$is_unit[indices])) {
      abort_audit("try_sse_2nd_order received inactive clause", state)
    }
    check_pair(state, local$meta_idx_oneend, local$meta_idx_target, fn)
    check_pair(state, local$meta_idx_twoends, local$meta_idx_target, fn)
    check_pair(state, local$meta_idx_target, local$meta_idx_oneend, fn)
    check_pair(state, local$meta_idx_target, local$meta_idx_twoends, fn)
  }
  token
}

decision_exit = function(state, local, fn, token, return_value) {
  if (audit$failed) return(invisible(NULL))
  top = audit$stack[[length(audit$stack)]]
  if (top$token != token) abort_audit("instrumentation stack unbalanced", state)
  audit$stack[[length(audit$stack)]] = NULL
  assert_global_equivalence(state, paste("exit", fn))
  check_matrix_debts(state, paste("exit", fn))
  if (identical(return_value, TRUE) && any(audit$initial_models)) {
    abort_audit(paste("false contradiction signal from", fn), state)
  }
  if (fn == "register_unit") {
    parent_same_symbol = any(vapply(audit$stack, function(frame) {
      identical(frame$fn, "register_unit") && identical(frame$symbol, top$symbol)
    }, logical(1)))
    # If a contradiction was detected, registration may leave the prior
    # bookkeeping intact intentionally. The initial model set certifies that.
    if (!parent_same_symbol && !identical(return_value, TRUE)) check_unit_boundary(state, top$symbol)
    if (parent_same_symbol) bump("nested_same_symbol_register_exit")
  }
  invisible(NULL)
}

skip_hook = function(state, local) {
  bump("matrix_skips")
  check_pair(state, local$s_clause_idx_meta, local$unit_idx_meta, "register_unit skip",
    symbols = local$nu, allow_inactive = TRUE)
  values = state$entries[[local$s_clause_idx]][[local$nu]]
  unit = state$unit_domains[[local$nu]]
  if (!all(values %in% unit)) {
    bump("deferred_raw_restriction_skips")
    if (is.null(audit$first_deferred_skip)) {
      audit$first_deferred_skip = list(trial = audit$trial,
        symbol = local$nu, clause_idx = local$s_clause_idx,
        values = values, unit = unit, stack = audit$stack,
        entries = state$entries, input = audit$input)
    }
  }
  invisible(NULL)
}

src = paste(readLines("R/CnfFormula_simplify.R"), collapse = "\n")
helpers = c("register_unit", "apply_domain_restriction", "eliminate_symbol_from_clause",
  "on_updated_subset_relations", "on_update_range", "handle_sse_2nd_order_oneend",
  "handle_sse_2nd_order_twoend", "try_sse_2nd_order", "eliminate_clause_update_sr")
for (fn in helpers) {
  pattern = paste0("  ", fn, " = function(")
  lines = strsplit(src, "\n", fixed = TRUE)[[1L]]
  found = lines[startsWith(lines, pattern)]
  stopifnot(length(found) == 1L, nzchar(found))
  replacement = paste0(found,
    "\n    audit_token = decision_enter(parent.env(environment()), environment(), \"", fn, "\")",
    "\n    on.exit(decision_exit(parent.env(environment()), environment(), \"", fn, "\", audit_token, returnValue()), add = TRUE)")
  src = sub(found, replacement, src, fixed = TRUE)
}
skip = "if (s_clause_idx_meta <= meta_idx_outer && inso_column[[s_clause_idx_meta]] && !is_not_subset_of[[s_clause_idx_meta]][unit_idx_meta, nu]) next"
stopifnot(grepl(skip, src, fixed = TRUE))
src = sub(skip, sub(" next$", " { skip_hook(parent.env(environment()), environment()); next }", skip), src, fixed = TRUE)
hla_anchor = "  # Now for the big one: Asymmetric Hidden Literal Addition (Marijn et al.)"
stopifnot(grepl(hla_anchor, src, fixed = TRUE))
src = sub(hla_anchor, paste0("  check_matrix_debts(environment(), \"pre_hla\")\n",
  "  audit$in_hla = TRUE\n", hla_anchor), src, fixed = TRUE)
eval(parse(text = src), envir = globalenv())

one_trial = function(i, seed, trial = NULL) {
  if (is.null(trial) && i %% 2L == 0L) {
    set.seed(seed * 7000003L + i)
    n_sym = sample(2:5, 1)
    uinfo = gen_universe(n_sym, sample(3:5, n_sym, replace = TRUE))
    trial = list(uinfo = uinfo, clauses = gen_unit_merge_hla_clauses(uinfo$domains), gen = "unit_merge")
  } else if (is.null(trial)) {
    trial = gen_standard_trial(i, seed)
  }
  audit$trial = i
  audit$input = trial
  audit$assignments = do.call(expand.grid, c(trial$uinfo$domains,
    list(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)))
  audit$initial_models = models(trial$clauses, audit$assignments)
  audit$serial = 0L
  audit$stack = list()
  audit$failed = FALSE
  audit$in_hla = FALSE
  audit$failure_state = NULL
  result = tryCatch(simplify_cnf(trial$clauses, trial$uinfo$universe), error = identity)
  if (inherits(result, "error")) {
    path = file.path(audit_dir, "results", sprintf("decision_failure_seed%d_trial%d.rds", seed, i))
    saveRDS(list(input = trial, message = conditionMessage(result),
      state = audit$failure_state, totals = audit$totals), path)
    stop(sprintf("trial %d: %s; saved %s", i, conditionMessage(result), path))
  }
  result_models = models(unclass(result), audit$assignments)
  stopifnot(identical(audit$initial_models, result_models), length(audit$stack) == 0L)
}

n = n_trials(1000L)
seed = base_seed()
replay_path = Sys.getenv("CNF_AUDIT_REPLAY", "")
replay_trial = NULL
if (nzchar(replay_path)) {
  replay = jsonlite::fromJSON(replay_path, simplifyVector = FALSE)
  domains = lapply(replay$domains, unlist, use.names = FALSE)
  if (prod(lengths(domains)) > 100000L) {
    stop("This truth-table replay is limited to 100,000 assignments; use a SAT/MDD oracle for larger inputs.")
  }
  clauses = lapply(replay$clauses, function(clause) lapply(clause, unlist, use.names = FALSE))
  universe = CnfUniverse()
  for (symbol in names(domains)) CnfSymbol(universe, symbol, domains[[symbol]])
  replay_trial = list(uinfo = list(domains = domains, universe = universe),
    clauses = clauses, gen = replay_path)
  n = 1L
}
cat(sprintf("decision invariants: n=%d seed=%d\n", n, seed))
started = proc.time()[["elapsed"]]
for (i in seq_len(n)) {
  one_trial(i, seed, replay_trial)
  if (i %% 100L == 0L) {
    cat(sprintf("%d/%d trials; elapsed %.1fs; pairs %d; projected certificates %d\n",
      i, n, proc.time()[["elapsed"]] - started,
      audit$totals[["pair_checks"]], audit$totals[["projected_certificates"]]))
    flush.console()
  }
}
print(audit$totals)
saveRDS(list(n = n, seed = seed, totals = audit$totals,
  first_raw_stale = audit$first_raw_stale,
  first_deferred_skip = audit$first_deferred_skip,
  first_matrix_debt = audit$first_matrix_debt),
  file.path(audit_dir, "results", sprintf("decision_success_seed%d%s.rds", seed,
    if (nzchar(replay_path)) "_replay" else "")))
