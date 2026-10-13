# Persistent JSON-lines bridge to the actual R implementation.
# No earlier campaign harness or generator is sourced.
suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
suppressMessages(library(jsonlite))
for (name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(name, ".R")))
}
plain_simplify = simplify_cnf
audit = new.env(parent = emptyenv())
audit$events = list()
record_event = function(kind, ...) {
  frame = parent.frame()
  event = c(list(kind = kind), list(...), list(units = as.list(get("unit_domains", frame, inherits = TRUE))))
  if (isTRUE(audit$detailed)) {
    event$active = get("entries", frame, inherits = TRUE)[!get("eliminated", frame, inherits = TRUE)]
  }
  audit$events[[length(audit$events) + 1L]] = event
}
replace_once = function(src, old, new) {
  found = gregexpr(old, src, fixed = TRUE)[[1L]]
  stopifnot(length(found) == 1L, found[[1L]] > 0L)
  sub(old, new, src, fixed = TRUE)
}
src = paste(readLines("R/CnfFormula_simplify.R"), collapse = "\n")
old = "        if (s_clause_idx_meta <= meta_idx_outer && inso_column[[s_clause_idx_meta]] && !is_not_subset_of[[s_clause_idx_meta]][unit_idx_meta, nu]) next"
src = replace_once(src, old, paste0(
  "        if (s_clause_idx_meta <= meta_idx_outer && inso_column[[s_clause_idx_meta]] && !is_not_subset_of[[s_clause_idx_meta]][unit_idx_meta, nu]) {\n",
  "          record_event('unit_skip', target = entries[[s_clause_idx]], symbol = nu, allowed = unit_domains[[nu]], source = unit, source_id = unit_idx, target_id = s_clause_idx)\n",
  "          next\n",
  "        }"))
skip_src = src
old = "      eliminate_clause_update_sr(available[[meta_idx_other]])"
src = replace_once(src, old, paste0(
  "      record_event('subsumption', donor = entries[[available[[meta_idx]]]], target = entries[[available[[meta_idx_other]]]])\n", old))
old = "      adr = apply_domain_restriction(available[[meta_idx_other]], symbol_to_restrict, entries[[available[[meta_idx]]]][[symbol_to_restrict]], FALSE)"
src = replace_once(src, old, paste0(
  "      record_event('sse1', donor = entries[[available[[meta_idx]]]], target = entries[[available[[meta_idx_other]]]], symbol = symbol_to_restrict, active_donor = !eliminated[[available[[meta_idx]]]] && !is_unit[[available[[meta_idx]]]], active_target = !eliminated[[available[[meta_idx_other]]]] && !is_unit[[available[[meta_idx_other]]]])\n", old))
old = "    apply_domain_restriction(idx_target, symbol_target, char_union(clause_oneend[[symbol_target]], clause_twoends[[symbol_target]]), FALSE)"
src = replace_once(src, old, paste0(
  "    record_event('sse2', donor_a = clause_oneend, donor_b = clause_twoends, target = entries[[idx_target]], intersect_symbol = symbol_intersect, restrict_symbol = symbol_target, active_donors = !eliminated[c(idx_oneend, idx_twoends)] & !is_unit[c(idx_oneend, idx_twoends)], active_target = !eliminated[[idx_target]] && !is_unit[[idx_target]])\n", old))
old = "      range_new = c(range_old, char_setdiff(universe[[symbol]], c(range_old, entries[[clause_idx_other]][[symbol]])))"
src = replace_once(src, old, paste0(old, "\n",
  "      record_event('hla', donor = entries[[clause_idx_other]], target = clause, target_id = clause_idx, donor_id = clause_idx_other, symbol = symbol, extended = range_new)"))
old = "      range_new = c(range_old, char_setdiff(universe[[symbol]], c(range_old, clause_other[[symbol]])))"
src = replace_once(src, old, paste0(old, "\n",
  "      record_event('unit_hla', donor = clause_other, target = clause, target_id = clause_idx, donor_id = clause_idx_other, symbol = symbol, extended = range_new)"))
eval(parse(text = src))
audited_simplify = simplify_cnf
eval(parse(text = skip_src))
skip_simplify = simplify_cnf
# Registration guard probe; no helper argument is forced before production's
# own first use. Existing input units are permitted while the matrix is NULL.
guard_src = src
guard_old = "    unit = entries[[unit_idx]]\n    nu = names(unit)"
guard_new = paste0(
  "    unit = entries[[unit_idx]]\n",
  "    if (!is.null(is_not_subset_of) && (eliminated[[unit_idx]] || is_unit[[unit_idx]])) stop('lifecycle audit: registering an inactive clause')\n",
  "    nu = names(unit)")
guard_src = replace_once(guard_src, guard_old, guard_new)
eval(parse(text = guard_src))
lifecycle_simplify = simplify_cnf
birth_src = replace_once(src, guard_old, paste0(
  "    unit = entries[[unit_idx]]\n",
  "    record_event('unit_birth', source = unit, source_id = unit_idx)\n",
  "    nu = names(unit)"))
eval(parse(text = birth_src))
unit_birth_simplify = simplify_cnf
# Phase isolation: retain ordinary unit propagation and direct subsumption,
# but leave all nonunit ranges untouched by SSE before running the real HLA
# loops. This permits an independent deletion-completeness experiment.
hla_only_src = replace_once(src,
  "  on_updated_subset_relations = function(meta_idx, meta_idx_other, second_order_only) {",
  paste0(
    "  on_updated_subset_relations = function(meta_idx, meta_idx_other, second_order_only) {\n",
    "    if (not_subset_count[meta_idx, meta_idx_other] == 0L) {\n",
    "      record_event('subsumption', donor = entries[[available[[meta_idx]]]], target = entries[[available[[meta_idx_other]]]])\n",
    "      eliminate_clause_update_sr(available[[meta_idx_other]])\n",
    "      return(NULL)\n",
    "    }\n",
    "    return(FALSE)"))
eval(parse(text = hla_only_src))
hla_only_simplify = simplify_cnf
# Diagnostic candidate only: require the skipped clause's *current* range to
# be strictly shorter than the effective unit range. This only prevents skips.
candidate_src = paste(readLines("R/CnfFormula_simplify.R"), collapse = "\n")
candidate_old = "        if (s_clause_idx_meta <= meta_idx_outer && inso_column[[s_clause_idx_meta]] && !is_not_subset_of[[s_clause_idx_meta]][unit_idx_meta, nu]) next"
candidate_new = "        if (s_clause_idx_meta <= meta_idx_outer && inso_column[[s_clause_idx_meta]] && !is_not_subset_of[[s_clause_idx_meta]][unit_idx_meta, nu] && length(entries[[s_clause_idx]][[nu]]) < length(unit_domains[[nu]])) next"
candidate_src = replace_once(candidate_src, candidate_old, candidate_new)
eval(parse(text = candidate_src))
unit_skip_length_candidate = simplify_cnf

# Phase boundary experiment: inspect only initial units and pairwise first-order
# operations. HLA and the explicit second-order pass have not run yet.
first_order_src = paste(readLines("R/CnfFormula_simplify.R"), collapse = "\n")
first_order_src = replace_once(first_order_src, "  second_order_enabled = TRUE\n",
  "  return(return_entries(entries[!eliminated]))\n  second_order_enabled = TRUE\n")
eval(parse(text = first_order_src))
first_order_simplify = simplify_cnf
first_order_audit_src = replace_once(src, "  second_order_enabled = TRUE\n",
  "  return(return_entries(entries[!eliminated]))\n  second_order_enabled = TRUE\n")
eval(parse(text = first_order_audit_src))
first_order_audited_simplify = simplify_cnf

# Diagnostic source-copy repair: a changed donor range can make its existing
# oneend restriction useful even when the exceptional bit stays TRUE.
sse1_candidate_src = paste(readLines("R/CnfFormula_simplify.R"), collapse = "\n")
sse1_candidate_old = "    if (second_order_enabled) on_update_range(meta_idx, symbol) else FALSE"
sse1_candidate_new = paste0(
  "    oneend_targets = which(not_subset_count[meta_idx, ] == 1L & is_not_subset_of[[meta_idx]][, is_not_subset_of_col] & !eliminated[available] & !is_unit[available])\n",
  "    for (other_meta_idx in oneend_targets) {\n",
  "      if (eliminated[[clause_idx]] || is_unit[[clause_idx]]) return(NULL)\n",
  "      if (eliminated[[available[[other_meta_idx]]]] || is_unit[[available[[other_meta_idx]]]] || not_subset_count[meta_idx, other_meta_idx] != 1L || !is_not_subset_of[[meta_idx]][other_meta_idx, is_not_subset_of_col]) next\n",
  "      ousr = on_updated_subset_relations(meta_idx, other_meta_idx, FALSE)\n",
  "      if (identical(ousr, TRUE)) return(TRUE)\n",
  "    }\n",
  "    if (eliminated[[clause_idx]] || is_unit[[clause_idx]]) return(NULL)\n",
  sse1_candidate_old)
sse1_candidate_src = replace_once(sse1_candidate_src, sse1_candidate_old, sse1_candidate_new)
eval(parse(text = sse1_candidate_src))
sse1_changed_range_candidate = simplify_cnf

# Broader diagnostic rescan: include already-subset donor range changes,
# complete symbol removal, and incoming pairs whose target has changed.
sse2_rescan_src = replace_once(sse1_candidate_src,
  "    if (second_order_enabled) on_update_range(meta_idx, symbol) else FALSE",
  "    rescan_sse2_for_clause(meta_idx)")
sse2_rescan_src = replace_once(sse2_rescan_src,
  "    FALSE\n  }\n\n  # returns 'NULL' when meta_idx_other",
  "    rescan_sse2_for_clause(meta_idx)\n  }\n\n  # returns 'NULL' when meta_idx_other")
rescan_definition = paste(c(
  "  rescan_sse2_for_clause = function(meta_idx) {",
  "    if (!second_order_enabled) return(FALSE)",
  "    current_idx = available[[meta_idx]]",
  "    active = !eliminated[available] & !is_unit[available]",
  "    outgoing = which(active & not_subset_count[meta_idx, ] >= 1L & not_subset_count[meta_idx, ] <= 2L)",
  "    for (other in outgoing) {",
  "      if (eliminated[[current_idx]] || is_unit[[current_idx]]) return(NULL)",
  "      if (eliminated[[available[[other]]]] || is_unit[[available[[other]]]] || !second_order_enabled_matrix[meta_idx, other]) next",
  "      count = not_subset_count[meta_idx, other]",
  "      if (count == 1L) {",
  "        pivot = colnames(is_not_subset_of[[meta_idx]])[is_not_subset_of[[meta_idx]][other, ]]",
  "        answer = handle_sse_2nd_order_oneend(meta_idx, other, pivot)",
  "      } else if (count == 2L) {",
  "        answer = handle_sse_2nd_order_twoend(meta_idx, other, NULL)",
  "      } else next",
  "      if (identical(answer, TRUE)) return(TRUE)",
  "    }",
  "    if (eliminated[[current_idx]] || is_unit[[current_idx]]) return(NULL)",
  "    incoming = which(!eliminated[available] & !is_unit[available] & not_subset_count[, meta_idx] == 2L)",
  "    for (other in incoming) {",
  "      if (eliminated[[current_idx]] || is_unit[[current_idx]]) return(NULL)",
  "      if (eliminated[[available[[other]]]] || is_unit[[available[[other]]]] || !second_order_enabled_matrix[other, meta_idx] || not_subset_count[other, meta_idx] != 2L) next",
  "      answer = handle_sse_2nd_order_twoend(other, meta_idx, NULL)",
  "      if (identical(answer, TRUE)) return(TRUE)",
  "    }",
  "    if (eliminated[[current_idx]] || is_unit[[current_idx]]) NULL else FALSE",
  "  }",
  ""
), collapse = "\n")
sse2_rescan_src = replace_once(sse2_rescan_src, "  # process units:\n", paste0(rescan_definition, "\n  # process units:\n"))
eval(parse(text = sse2_rescan_src))
sse2_rescan_candidate = simplify_cnf
all_candidate_src = replace_once(sse2_rescan_src, candidate_old, candidate_new)
eval(parse(text = all_candidate_src))
combined_rescan_candidate = simplify_cnf

normalize_clauses = function(clauses) {
  if (is.logical(clauses)) return(clauses)
  lapply(clauses, function(clause) {
    lapply(clause, function(values) as.character(unlist(values, use.names = FALSE)))
  })
}
run_request = function(request) {
  domains = lapply(request$domains, function(d) as.character(unlist(d, use.names = FALSE)))
  universe = CnfUniverse()
  for (symbol in names(domains)) CnfSymbol(universe, symbol, domains[[symbol]])
  clauses = normalize_clauses(request$clauses)
  audit$events = list()
  audit$detailed = isTRUE(request$detailed)
  simplify_cnf <<- if (isTRUE(request$audit)) audited_simplify else if (identical(request$audit, "skip")) skip_simplify else plain_simplify
  if (identical(request$variant, "unit_skip_length")) simplify_cnf <<- unit_skip_length_candidate
  if (identical(request$variant, "lifecycle")) simplify_cnf <<- lifecycle_simplify
  if (identical(request$variant, "unit_birth")) simplify_cnf <<- unit_birth_simplify
  if (identical(request$variant, "hla_only")) simplify_cnf <<- hla_only_simplify
  if (identical(request$variant, "first_order_phase")) simplify_cnf <<- if (isTRUE(request$audit)) first_order_audited_simplify else first_order_simplify
  if (identical(request$variant, "sse1_changed_range")) simplify_cnf <<- sse1_changed_range_candidate
  if (identical(request$variant, "sse2_rescan")) simplify_cnf <<- sse2_rescan_candidate
  if (identical(request$variant, "combined_rescan")) simplify_cnf <<- combined_rescan_candidate
  elapsed_start = proc.time()[[3L]]
  if (isTRUE(request$direct)) {
    result = simplify_cnf(clauses, universe)
  } else if (is.logical(clauses)) {
    result = as.CnfFormula(clauses)
  } else {
    objects = lapply(clauses, function(clause) {
      CnfClause(lapply(names(clause), function(symbol) {
        CnfAtom(structure(symbol, universe = universe, class = "CnfSymbol"), clause[[symbol]])
      }))
    })
    result = CnfFormula(objects)
  }
  list(ok = TRUE, result = c(result), events = audit$events,
    seconds = proc.time()[[3L]] - elapsed_start)
}
input = file("stdin", "r")
repeat {
  line = readLines(input, n = 1L, warn = FALSE)
  if (!length(line)) break
  answer = tryCatch(run_request(fromJSON(line, simplifyVector = FALSE)),
    error = function(error) list(ok = FALSE, message = conditionMessage(error)))
  cat(toJSON(answer, auto_unbox = TRUE, null = "null", digits = NA), "\n", sep = "")
  flush.console()
}
