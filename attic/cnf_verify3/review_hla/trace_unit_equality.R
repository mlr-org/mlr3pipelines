# Demonstrate the precise equality skip behind the production counterexample.
# This only evaluates an instrumented in-memory source copy.
source("R/CnfFormula_simplify.R")
plain_simplify = simplify_cnf
unit_skip_events = list()
src = paste(readLines("R/CnfFormula_simplify.R"), collapse = "\n")
old = "        if (s_clause_idx_meta <= meta_idx_outer && inso_column[[s_clause_idx_meta]] && !is_not_subset_of[[s_clause_idx_meta]][unit_idx_meta, nu]) next"
stopifnot(length(gregexpr(old, src, fixed = TRUE)[[1L]]) == 1L)
new = paste0(
  "        if (s_clause_idx_meta <= meta_idx_outer && inso_column[[s_clause_idx_meta]] && !is_not_subset_of[[s_clause_idx_meta]][unit_idx_meta, nu]) {\n",
  "          unit_skip_events[[length(unit_skip_events) + 1L]] <<- list(\n",
  "            registering_unit = unit, current_unit = unit_domains[[nu]],\n",
  "            target = entries[[s_clause_idx]], symbol = nu,\n",
  "            cached_unit_not_subset_target = inso_column[[s_clause_idx_meta]],\n",
  "            cached_target_not_subset_unit = is_not_subset_of[[s_clause_idx_meta]][unit_idx_meta, nu],\n",
  "            raw_target_equals_unit = setequal(entries[[s_clause_idx]][[nu]], unit_domains[[nu]]))\n",
  "          next\n",
  "        }")
src = sub(old, new, src, fixed = TRUE)
eval(parse(text = src))
universe = list(y = as.character(0:2), x = as.character(0:3))
entries = list(list(y = "0", x = c("0", "1")),
  list(y = "1", x = c("0", "2")), list(y = "2", x = c("0", "3")))
result = simplify_cnf(entries, universe)
stopifnot(identical(result, plain_simplify(entries, universe)))
equality_events = Filter(function(event) event$raw_target_equals_unit, unit_skip_events)
stopifnot(length(equality_events) > 0L)
dput(equality_events)
cat("Final formula:\n")
dput(c(result))
