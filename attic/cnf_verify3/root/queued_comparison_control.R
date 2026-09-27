# Test a necessary reentrancy guard using a deliberately changed private copy.
# Any discrepancy here belongs to that changed copy, not the production code.
suppressPackageStartupMessages({
  library(checkmate)
  library(mlr3misc)
  library(jsonlite)
})
for (file in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(file, ".R")))
}
plain_simplify = simplify_cnf
source_text = paste(readLines("R/CnfFormula_simplify.R"), collapse = "\n")
guard = "      if (!is_not_subset_of[[meta_idx]][other_meta_idx, is_not_subset_of_col]) next"
stopifnot(length(gregexpr(guard, source_text, fixed = TRUE)[[1L]]) == 1L,
  grepl(guard, source_text, fixed = TRUE))
changed_environment = new.env(parent = globalenv())
eval(parse(text = sub(guard, "", source_text, fixed = TRUE)), changed_environment)
changed_simplify = changed_environment$simplify_cnf

evaluate = function(clauses, assignments) {
  if (is.logical(clauses)) return(rep(as.vector(clauses), nrow(assignments)))
  answer = rep(TRUE, nrow(assignments))
  for (clause in clauses) {
    satisfies = rep(FALSE, nrow(assignments))
    for (symbol in names(clause)) satisfies = satisfies | assignments[[symbol]] %in% clause[[symbol]]
    answer = answer & satisfies
  }
  answer
}
valid = function(case) {
  is.list(case$clauses) && length(case$clauses) > 0L && all(vapply(case$clauses, function(clause) {
    length(clause) > 0L && all(vapply(names(clause), function(symbol) {
      length(clause[[symbol]]) > 0L && length(clause[[symbol]]) < length(case$domains[[symbol]])
    }, FALSE))
  }, FALSE))
}
assess = function(case, details = FALSE) {
  if (!valid(case)) return(FALSE)
  universe = CnfUniverse()
  for (symbol in names(case$domains)) CnfSymbol(universe, symbol, case$domains[[symbol]])
  plain = plain_simplify(case$clauses, universe)
  changed = tryCatch(changed_simplify(case$clauses, universe), error = function(e) conditionMessage(e))
  if (!details && identical(plain, changed)) return(FALSE)
  grid = expand.grid(case$domains, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  expected = evaluate(case$clauses, grid)
  stopifnot(identical(evaluate(unclass(plain), grid), expected))
  different = is.character(changed) || !identical(evaluate(unclass(changed), grid), expected)
  if (!details) return(different)
  list(case = case, changed_copy_differs = different,
    baseline = unclass(plain), changed = if (is.character(changed)) changed else unclass(changed),
    valuations = nrow(grid), expected_models = sum(expected),
    witness = if (is.character(changed) || !different) NULL else
      grid[which(evaluate(unclass(changed), grid) != expected)[[1L]], , drop = FALSE])
}

observations = readRDS("attic/cnf_verify3/root/branch_dense/branch_obligations.rds")
case = observations$first_inputs[[length(observations$sites) + 21L]]
if (!assess(case)) {
  cat("The first site-21 witness preserves truth even with the changed copy; trying all saved witnesses.\n")
  case = NULL
  for (candidate in observations$first_inputs) {
    if (is.null(candidate) || !assess(candidate)) next
    case = candidate
    break
  }
}
if (is.null(case)) {
  cat("No saved first witness separates truth; checking finite dense formulas with planted models.\n")
  set.seed(9061420L)
  for (trial in seq_len(20000L)) {
    n_symbols = sample(3:6, 1L)
    domains = lapply(seq_len(n_symbols), function(si) paste0("v", seq_len(sample(3:6, 1L))))
    names(domains) = paste0("X", seq_len(n_symbols))
    assignment = lapply(domains, function(domain) sample(domain, 1L))
    clauses = lapply(seq_len(sample(8:35, 1L)), function(ci) {
      symbols = sample(names(domains), sample.int(min(4L, n_symbols) - 1L, 1L) + 1L)
      ranges = lapply(symbols, function(symbol) {
        domain = domains[[symbol]]
        sample(domain, sample.int(length(domain) - 1L, 1L))
      })
      names(ranges) = symbols
      if (!any(vapply(symbols, function(symbol) assignment[[symbol]] %in% ranges[[symbol]], FALSE))) {
        symbol = sample(symbols, 1L)
        ranges[[symbol]][[1L]] = assignment[[symbol]]
      }
      ranges
    })
    candidate = list(label = paste0("planted:", trial), domains = domains, clauses = clauses)
    if (assess(candidate)) { case = candidate; break }
    if (trial %% 1000L == 0L) cat("checked planted cases:", trial, "\n")
  }
}
if (is.null(case)) stop("No truth-separating case found in this completed finite control search")
saveRDS(case, "attic/cnf_verify3/root/queued_comparison_control_seed_case.rds")
cat("The changed copy differs on:", case$label, "\n")
attempts = 0L
accept = function(candidate) {
  attempts <<- attempts + 1L
  if (!assess(candidate)) return(FALSE)
  case <<- candidate
  TRUE
}

repeat {
  progress = FALSE
  for (ci in seq_along(case$clauses)) {
    candidate = case
    candidate$clauses = candidate$clauses[-ci]
    if (accept(candidate)) { progress = TRUE; break }
  }
  if (progress) next
  for (ci in seq_along(case$clauses)) {
    for (symbol in names(case$clauses[[ci]])) {
      candidate = case
      candidate$clauses[[ci]][[symbol]] = NULL
      if (accept(candidate)) { progress = TRUE; break }
    }
    if (progress) break
  }
  if (progress) next
  for (ci in seq_along(case$clauses)) {
    for (symbol in names(case$clauses[[ci]])) {
      for (vi in seq_along(case$clauses[[ci]][[symbol]])) {
        candidate = case
        candidate$clauses[[ci]][[symbol]] = candidate$clauses[[ci]][[symbol]][-vi]
        if (accept(candidate)) { progress = TRUE; break }
      }
      if (progress) break
    }
    if (progress) break
  }
  if (progress) next
  for (symbol in names(case$domains)) {
    if (length(case$domains[[symbol]]) <= 2L) next
    for (value in case$domains[[symbol]]) {
      candidate = case
      candidate$domains[[symbol]] = setdiff(candidate$domains[[symbol]], value)
      candidate$clauses = lapply(candidate$clauses, function(clause) {
        if (symbol %in% names(clause)) clause[[symbol]] = setdiff(clause[[symbol]], value)
        clause
      })
      if (accept(candidate)) { progress = TRUE; break }
    }
    if (progress) break
  }
  if (!progress) break
}
case$domains = case$domains[unique(unlist(lapply(case$clauses, names), use.names = FALSE))]
result = assess(case, details = TRUE)
result$attempts = attempts
result$source_sha256 = "7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc"
result$R_version = R.version.string
saveRDS(result, "attic/cnf_verify3/root/queued_comparison_control.rds")
writeLines(toJSON(result, auto_unbox = TRUE, pretty = TRUE, null = "null"),
  "attic/cnf_verify3/root/queued_comparison_control.json")
cat("Reduced clauses:", length(case$clauses), "; symbols:", length(case$domains),
  "; valuations:", result$valuations, "; candidate attempts:", attempts, "\n")
cat("Changed-copy result:", if (is.character(result$changed)) result$changed else "different truth values", "\n")
