# Independent parser catalog, saved-witness replay, and local branch contracts.
# Run from the repository root; no root or production artifact is modified.
suppressPackageStartupMessages({
  library(checkmate)
  library(mlr3misc)
  library(digest)
})
here = "attic/cnf_verify3/branch_review"
dir.create(here, recursive = TRUE, showWarnings = FALSE)
kernel_path = "R/CnfFormula_simplify.R"
for (name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(name, ".R")))
}
plain_simplify = simplify_cnf

reports = lapply(c("attic/cnf_verify3/root", "attic/cnf_verify3/root/branch_dense"), function(path) {
  rds = readRDS(file.path(path, "branch_obligations.rds"))
  tsv = read.delim(file.path(path, "branch_obligations.tsv"), stringsAsFactors = FALSE,
    check.names = FALSE)
  stopifnot(isTRUE(rds$complete), length(rds$sites) == 108L,
    identical(tsv$id, seq_len(108L)), length(rds$first_inputs) == 216L,
    identical(as.numeric(rds$counts), as.numeric(as.matrix(tsv[c("false", "true")]))))
  for (i in seq_len(108L)) {
    stopifnot(identical(rds$sites[[i]]$condition, tsv$condition[[i]]),
      identical(rds$sites[[i]]$owner, tsv$owner[[i]]))
  }
  for (i in seq_len(216L)) stopifnot((rds$counts[[i]] == 0) == is.null(rds$first_inputs[[i]]))
  list(path = path, data = rds, table = tsv)
})
stopifnot(identical(reports[[1L]]$data$sites, reports[[2L]]$data$sites))
combined = reports[[1L]]$data$counts + reports[[2L]]$data$counts
stopifnot(identical(which(combined[, "true"] == 0), c(26L, 75L, 98L, 104L)),
  all(combined[, "false"] > 0))

# Use R's token/source-range parser, independently of the root's AST visitor.
parsed = getParseData(parse(kernel_path, keep.source = TRUE), includeText = TRUE)
if_tokens = parsed[parsed$token == "IF", ]
if_tokens = if_tokens[order(if_tokens$line1, if_tokens$col1), ]
function_tokens = parsed[parsed$token == "FUNCTION", ]
functions = lapply(seq_len(nrow(function_tokens)), function(i) {
  definition = parsed[match(function_tokens$parent[[i]], parsed$id), ]
  siblings = parsed[parsed$parent == definition$parent & parsed$token == "expr", ]
  siblings = siblings[order(siblings$line1, siblings$col1), ]
  list(owner = siblings$text[[1L]], line1 = definition$line1, line2 = definition$line2,
    col1 = definition$col1, col2 = definition$col2)
})
catalog = do.call(rbind, lapply(seq_len(nrow(if_tokens)), function(i) {
  token = if_tokens[i, ]
  children = parsed[parsed$parent == token$parent & parsed$token == "expr", ]
  children = children[order(children$line1, children$col1), ]
  condition = paste(deparse(parse(text = children$text[[1L]])[[1L]],
    width.cutoff = 500L), collapse = " ")
  enclosing = which(vapply(functions, function(f) {
    f$line1 <= token$line1 && f$line2 >= token$line2
  }, FALSE))
  owner = functions[[enclosing[[which.min(vapply(functions[enclosing],
    function(f) f$line2 - f$line1, 0L))]]]]$owner
  data.frame(id = i, line = token$line1, column = token$col1, owner = owner,
    condition = condition, false = combined[i, 1L], true = combined[i, 2L],
    stringsAsFactors = FALSE)
}))
stopifnot(nrow(catalog) == 108L,
  identical(catalog$condition, reports[[1L]]$table$condition),
  identical(catalog$owner, reports[[1L]]$table$owner))
write.table(catalog, file.path(here, "independent_catalog.tsv"), sep = "\t",
  quote = TRUE, row.names = FALSE)

audit = new.env(parent = emptyenv())
audit$seen = matrix(FALSE, 108L, 2L)
audit$checks = c(range_target = 0L, stale_unit_absent_symbol = 0L,
  symbol_deletion = 0L, clause_deletion = 0L, nonunit_hla = 0L, unit_hla = 0L)
audit$hla_first = list()
audit$stale_unit_first = NULL
audit$current_case = NULL

review_range_target = function(frame) {
  symbol_idx = get("symbol_idx", frame)
  idx = get("clause_idx", frame)
  if (is.na(symbol_idx)) {
    if (get("is_unit", frame, inherits = TRUE)[[idx]]) {
      audit$checks[["stale_unit_absent_symbol"]] = audit$checks[["stale_unit_absent_symbol"]] + 1L
      if (is.null(audit$stale_unit_first)) audit$stale_unit_first = list(
        case = audit$current_case, clause_idx = idx, clause = get("clause", frame),
        propagated_symbol = get("symbol", frame),
        unit_domains = as.list(get("unit_domains", frame, inherits = TRUE)))
    }
    return(invisible(NULL))
  }
  stopifnot(!get("eliminated", frame, inherits = TRUE)[[idx]],
    !get("is_unit", frame, inherits = TRUE)[[idx]], length(get("clause", frame)) >= 2L)
  audit$checks[["range_target"]] = audit$checks[["range_target"]] + 1L
}
review_symbol_target = function(frame) {
  idx = get("clause_idx", frame)
  stopifnot(length(get("clause", frame)) >= 2L,
    !get("eliminated", frame, inherits = TRUE)[[idx]],
    !get("is_unit", frame, inherits = TRUE)[[idx]])
  audit$checks[["symbol_deletion"]] = audit$checks[["symbol_deletion"]] + 1L
}
review_if = function(id, condition) {
  frame = parent.frame()
  value = force(condition)
  stopifnot(length(value) == 1L, !is.na(value),
    is.logical(value) || is.integer(value) || is.double(value))
  outcome = as.integer(as.logical(value)) + 1L
  audit$seen[id, outcome] = TRUE
  if (id == 75L) {
    idx = get("clause_idx", frame)
    stopifnot(!value, !get("eliminated", frame, inherits = TRUE)[[idx]],
      length(get("entries", frame, inherits = TRUE)[[idx]]) >= 2L)
    audit$checks[["clause_deletion"]] = audit$checks[["clause_deletion"]] + 1L
  }
  if (id %in% c(98L, 104L)) {
    symbol = get("symbol", frame)
    virtual = get("clause", frame)
    donor = get("entries", frame)[[get("clause_idx_other", frame)]]
    domain = get("universe", frame)[[symbol]]
    new = get("range_new", frame)
    missing = setdiff(donor[[symbol]], virtual[[symbol]])
    stopifnot(length(symbol) == 1L, length(missing) > 0L,
      all(missing %in% domain), !any(missing %in% new),
      !anyDuplicated(new), all(new %in% domain), length(new) < length(domain), !value)
    if (id == 98L) {
      bits = get("is_not_subset_of", frame)[[get("meta_idx_other", frame)]][get("meta_idx", frame), ]
      count = get("not_subset_count_current", frame)[[get("hla_clause_idx", frame)]]
    } else {
      bits = get("is_not_subset_entry", frame)
      count = get("not_subset_count", frame)[[get("hla_clause_idx", frame)]]
    }
    expected = vapply(names(bits), function(s) !all(donor[[s]] %in% virtual[[s]]), FALSE)
    stopifnot(identical(unname(bits), unname(expected)), sum(bits) == 1L, count == 1L)
    name = if (id == 98L) "nonunit_hla" else "unit_hla"
    audit$checks[[name]] = audit$checks[[name]] + 1L
    if (is.null(audit$hla_first[[name]])) audit$hla_first[[name]] = list(
      donor = donor, virtual = virtual, symbol = symbol, missing = missing,
      range_new = new, domain = domain)
  }
  value
}

# New observer, used only to replay the exact saved witnesses. Additional
# contracts are placed after the production function first forces its inputs.
site_index = 0L
transform = function(expr, owner = "simplify_cnf") {
  if (!is.call(expr)) return(expr)
  if (identical(expr[[1L]], as.name("=")) && length(expr) == 3L &&
      is.call(expr[[3L]]) && identical(expr[[3L]][[1L]], as.name("function"))) {
    expr[[3L]] = transform(expr[[3L]], as.character(expr[[2L]]))
    return(expr)
  }
  if (identical(expr[[1L]], as.name("if"))) {
    site_index <<- site_index + 1L
    stopifnot(identical(paste(deparse(expr[[2L]], width.cutoff = 500L), collapse = " "),
      catalog$condition[[site_index]]), identical(owner, catalog$owner[[site_index]]))
    expr[[2L]] = as.call(list(as.name("review_if"), site_index, expr[[2L]]))
  }
  for (i in seq_along(expr)[-1L]) if (is.call(expr[[i]])) expr[[i]] = transform(expr[[i]], owner)
  if (identical(expr[[1L]], as.name("{")) && owner %in% c("apply_domain_restriction", "eliminate_symbol_from_clause")) {
    items = as.list(expr)
    sought = if (owner == "apply_domain_restriction") "symbol_idx" else "clause"
    after = which(vapply(items, function(item) is.call(item) && length(item) == 3L &&
      identical(item[[1L]], as.name("=")) && identical(item[[2L]], as.name(sought)), FALSE))
    if (length(after)) {
      stopifnot(length(after) == 1L)
      hook = if (owner == "apply_domain_restriction") quote(review_range_target(environment())) else quote(review_symbol_target(environment()))
      expr = as.call(append(items, list(hook), after = after))
    }
  }
  expr
}
observed_simplify = plain_simplify
body(observed_simplify) = transform(body(plain_simplify))
stopifnot(site_index == 108L)

check_canonical = function(case) {
  ds = case$domains
  stopifnot(!anyNA(names(ds)), !anyDuplicated(names(ds)))
  for (domain in ds) stopifnot(is.character(domain), length(domain) > 0L,
    !anyNA(domain), !anyDuplicated(domain))
  cs = case$clauses
  if (is.logical(cs)) return(stopifnot(length(cs) == 1L, !is.na(cs)))
  for (clause in cs) {
    stopifnot(length(clause) > 0L, !is.null(names(clause)),
      !anyNA(names(clause)), !anyDuplicated(names(clause)), all(names(clause) %in% names(ds)))
    for (s in names(clause)) stopifnot(is.character(clause[[s]]), length(clause[[s]]) > 0L,
      !anyNA(clause[[s]]), !anyDuplicated(clause[[s]]),
      all(clause[[s]] %in% ds[[s]]), length(clause[[s]]) < length(ds[[s]]))
  }
}

cases = list()
required = list()
for (report in reports) for (i in which(report$data$counts > 0)) {
  case = report$data$first_inputs[[i]]
  check_canonical(case)
  key = digest(list(case$domains, case$clauses), algo = "sha256")
  if (is.null(cases[[key]])) cases[[key]] = case
  required[[key]] = union(required[[key]], i)
}
all_seen = matrix(FALSE, 108L, 2L)
replayed_obligations = 0L
for (key in names(cases)) {
  case = cases[[key]]
  audit$current_case = case
  universe = CnfUniverse()
  for (symbol in names(case$domains)) CnfSymbol(universe, symbol, case$domains[[symbol]])
  audit$seen[,] = FALSE
  expected = plain_simplify(case$clauses, universe)
  observed = observed_simplify(case$clauses, universe)
  stopifnot(identical(observed, expected), all(audit$seen[required[[key]]]))
  all_seen = all_seen | audit$seen
  replayed_obligations = replayed_obligations + length(required[[key]])
}
stopifnot(identical(which(!all_seen[, 2L]), c(26L, 75L, 98L, 104L)), all(all_seen[, 1L]))
result = list(cases_recorded = vapply(reports, function(r) r$data$cases, 0L),
  versions_recorded = vapply(reports, function(r) r$data$version, ""),
  parser_if_sites = nrow(catalog), parser_function_sites = length(functions),
  combined_evaluations = sum(combined), covered_outcomes = sum(combined > 0),
  replayed_cases = length(cases), replayed_saved_obligations = replayed_obligations,
  contract_checks = audit$checks, hla_examples = audit$hla_first,
  stale_unit_example = audit$stale_unit_first,
  all_seen = all_seen, source_sha256 = system2("sha256sum", kernel_path, stdout = TRUE),
  version = R.version.string)
stopifnot(all(vapply(reports, function(r) result$source_sha256 %in% r$data$source_hashes, FALSE)))
saveRDS(result, file.path(here, "review_results.rds"))
print(result[setdiff(names(result), c("hla_examples", "all_seen", "stale_unit_example"))])
