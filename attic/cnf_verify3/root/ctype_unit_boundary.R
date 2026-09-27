# Independent public reproduction and complete small-domain ordered enumeration.
suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
suppressMessages(library(jsonlite))
for (source_name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(source_name, ".R")))
}
candidate_mode = "--candidate" %in% commandArgs(TRUE)
if (candidate_mode) {
  old_expression = quote(char_intersect(names(entries[[clause_idx]]), names(unit_domains)))
  new_expression = quote(Filter(function(symbol) !is.null(unit_domains[[symbol]]),
    names(entries[[clause_idx]])))
  replacements = 0L
  replace_expression = function(node) {
    if (identical(node, old_expression)) {
      replacements <<- replacements + 1L
      return(new_expression)
    }
    if (is.call(node)) for (i in seq_along(node)) {
      if (!identical(node[[i]], quote(expr = ))) node[i] = list(replace_expression(node[[i]]))
    }
    node
  }
  body(simplify_cnf) = replace_expression(body(simplify_cnf))
  stopifnot(replacements == 1L)
}

run_boundary = function() {
  old_ctype = Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", old_ctype))
  unicode_name = intToUtf8(0xe9L)
  stopifnot(Encoding(unicode_name) == "UTF-8")
  domains = list(c("a", "b", "c"), c("a", "b"))
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  masks = lapply(domains, function(domain) {
    lapply(seq_len(2^length(domain) - 2L), function(mask) {
      domain[as.logical(intToBits(mask)[seq_along(domain)])]
    })
  })

  truth_clause = function(clause, symbol_names) {
    bare = c(clause)
    if (is.logical(bare)) return(rep(bare, nrow(assignments)))
    output = rep(FALSE, nrow(assignments))
    for (position in seq_along(bare)) {
      symbol_position = match(names(bare)[[position]], symbol_names)
      stopifnot(!is.na(symbol_position))
      output = output | assignments[[symbol_position]] %in% bare[[position]]
    }
    output
  }
  truth_formula = function(formula, symbol_names) {
    bare = c(formula)
    if (is.logical(bare)) return(rep(bare, nrow(assignments)))
    output = rep(TRUE, nrow(assignments))
    for (clause in bare) output = output & truth_clause(clause, symbol_names)
    output
  }
  plain_payload = function(formula, symbol_names) {
    bare = c(formula)
    if (is.logical(bare)) return(unname(bare))
    lapply(bare, function(clause) {
      setNames(lapply(seq_along(clause), function(i) clause[[i]]),
        c("X", "Y")[match(names(clause), symbol_names)])
    })
  }

  all_records = list()
  saved = list()
  started = proc.time()[["elapsed"]]
  for (ctype in c("C", "C.UTF-8")) {
    stopifnot(nzchar(Sys.setlocale("LC_CTYPE", ctype)))
    for (name_kind in c("ascii", "utf8")) {
      symbol_names = c(if (name_kind == "utf8") unicode_name else "X", "Y")
      u = CnfUniverse()
      symbol_handles = list(CnfSymbol(u, symbol_names[[1L]], domains[[1L]]),
        CnfSymbol(u, symbol_names[[2L]], domains[[2L]]))
      atom = function(position, range) CnfAtom(symbol_handles[[position]], range)
      palette = list()
      for (symbol in 1:2) for (range in masks[[symbol]]) {
        palette[[length(palette) + 1L]] = as.CnfClause(atom(symbol, range))
      }
      for (left in masks[[1L]]) for (right in masks[[2L]]) {
        palette[[length(palette) + 1L]] = CnfClause(list(atom(1L, left), atom(2L, right)))
      }
      stopifnot(length(palette) == 20L)
      clause_truth = vapply(palette, truth_clause, logical(6L), symbol_names = symbol_names)
      stopifnot(all(vapply(palette, function(clause) !anyDuplicated(names(c(clause))), TRUE)))
      environment_names = names(u)
      for (clause_count in 0:3) {
        records = c(calls = 0L, errors = 0L, truth_differences = 0L,
          false_unsat = 0L, missed_unsat = 0L, changed_satisfiable = 0L,
          added_assignments = 0L, removed_assignments = 0L)
        orders = if (!clause_count) matrix(integer(), nrow = 1L, ncol = 0L) else
          as.matrix(expand.grid(rep(list(seq_along(palette)), clause_count)))
        for (row in seq_len(nrow(orders))) {
          indices = orders[row, ]
          clauses = palette[indices]
          expected = if (!clause_count) rep(TRUE, 6L) else
            apply(clause_truth[, indices, drop = FALSE], 1L, all)
          result = tryCatch(CnfFormula(clauses), error = identity)
          records[["calls"]] = records[["calls"]] + 1L
          if (inherits(result, "error")) {
            records[["errors"]] = records[["errors"]] + 1L
            key = paste(ctype, name_kind, clause_count, "error", sep = ":")
            if (is.null(saved[[key]])) saved[[key]] = list(indices = indices,
              input = lapply(clauses, c), message = conditionMessage(result))
            next
          }
          actual = truth_formula(result, symbol_names)
          if (!identical(expected, actual)) {
            records[["truth_differences"]] = records[["truth_differences"]] + 1L
            records[["false_unsat"]] = records[["false_unsat"]] + (any(expected) && !any(actual))
            records[["missed_unsat"]] = records[["missed_unsat"]] + (!any(expected) && any(actual))
            records[["changed_satisfiable"]] = records[["changed_satisfiable"]] + any(expected)
            records[["added_assignments"]] = records[["added_assignments"]] + sum(actual & !expected)
            records[["removed_assignments"]] = records[["removed_assignments"]] + sum(expected & !actual)
            category = if (any(expected)) "satisfiable" else "unsatisfiable"
            key = paste(ctype, name_kind, clause_count, category, sep = ":")
            if (is.null(saved[[key]])) saved[[key]] = list(indices = indices,
              input = lapply(clauses, function(clause) setNames(c(clause),
                c("X", "Y")[match(names(c(clause)), symbol_names)])),
              output = plain_payload(result, symbol_names), expected = expected, actual = actual)
          }
        }
        cohort = list(ctype = ctype, name_kind = name_kind, clause_count = clause_count,
          stats = as.list(records))
        all_records[[length(all_records) + 1L]] = cohort
        cat(toJSON(cohort, auto_unbox = TRUE), "\n")
        flush.console()
      }
      # All names are accepted and exact marked lookup succeeds, but enumeration
      # is not an identity round trip in the failing native character locale.
      saved[[paste(ctype, name_kind, "names", sep = ":")]] = list(
        supplied = symbol_names, enumerated = environment_names,
        supplied_encoding = Encoding(symbol_names), enumeration_encoding = Encoding(environment_names),
        name_membership = symbol_names %in% environment_names,
        exact_lookup = lapply(symbol_names, function(name) u[[name]]))
    }
  }
  # The enumeration is complete for the stated palette and ordered lengths.
  stopifnot(sum(vapply(all_records, function(x) x$stats$calls, 0L)) == 33684L)
  for (cohort in all_records) {
    stopifnot(cohort$stats$errors == 0L)
    if (candidate_mode || cohort$ctype != "C" || cohort$name_kind != "utf8" || cohort$clause_count < 3L) {
      stopifnot(cohort$stats$truth_differences == 0L)
    }
  }
  stopifnot(identical(any(vapply(all_records, function(x) x$stats$truth_differences > 0L, TRUE)),
    !candidate_mode))
  list(runtime = R.version.string, candidate = candidate_mode, initial_ctype = old_ctype, records = all_records,
    examples = saved, elapsed_seconds = proc.time()[["elapsed"]] - started)
}

result = run_boundary()
suffix = if (getRversion() < "4.0") "r36" else "r46"
if (candidate_mode) suffix = paste0("candidate_", suffix)
base_path = file.path("attic", "cnf_verify3", "root", paste0("ctype_unit_boundary_", suffix))
saveRDS(result, paste0(base_path, ".rds"))
# Saved name bytes/encodings remain in the lossless RDS. JSON contains only
# ASCII abstract formula names and cohort summaries.
summary = result
summary$examples = summary$examples[!grepl(":names$", names(summary$examples))]
write_json(summary, paste0(base_path, ".json"), auto_unbox = TRUE, pretty = TRUE)
cat("COMPLETE", suffix, result$elapsed_seconds, "seconds\n")
