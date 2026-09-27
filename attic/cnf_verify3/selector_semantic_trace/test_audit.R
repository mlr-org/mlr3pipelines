# Run from repository root with Rscript, including the isolated R 4.6 launcher.
source("attic/cnf_verify3/selector_semantic_trace/common.R")
suppressPackageStartupMessages(library(testthat))

test_that("the independent oracle retains every unequal duplicate position", {
  assignments = expand.grid(X = letters[1:3], Y = letters[1:2], stringsAsFactors = FALSE)
  clause = list(X = "a", X = c("b", "c"))
  expect_identical(truth_clause(clause, assignments), rep(TRUE, 6L))
  expect_identical(truth_clause(clause, assignments, TRUE), c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_identical(truth_formula(list(clause, list(Y = "a")), assignments),
    c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(truth_formula(list(), assignments), rep(TRUE, 6L))
  expect_identical(truth_formula(FALSE, assignments), rep(FALSE, 6L))
})

runs = lapply(case_specs, run_trace_case)

first_divergence = function(run, interpretation) {
  which(vapply(run$events, function(event) {
    length(event$delta[[interpretation]]) > 0L
  }, logical(1)))[[1L]]
}

test_that("the four-clause discrepancy has different first semantic events", {
  run = runs$four_clause
  expect_null(run$error)
  expect_false(any(run$expected))
  expect_identical(run$output, list(list(X = "c"), list(Y = "a")))
  positional = run$events[[first_divergence(run, "positional")]]
  projection = run$events[[first_divergence(run, "projection")]]
  expect_identical(positional$line, 474L)
  expect_identical(positional$eliminated, c(FALSE, TRUE, FALSE, FALSE))
  expect_identical(positional$delta$positional, 3L)
  expect_length(positional$delta$projection, 0L)
  expect_identical(projection$line, 244L)
  expect_identical(projection$entries[[1L]], list(X = c("b", "c")))
  expect_identical(projection$delta$projection, 3L)
  expect_length(projection$delta$positional, 0L)
  expect_identical(run$assignments[3L, ], data.frame(X = "c", Y = "a", row.names = 3L))
  expect_length(run$evidence$proposals, 0L)
  expect_length(run$evidence$commits, 7L)
  expect_length(run$evidence$units, 3L)
  for (unit in run$evidence$units) {
    expect_length(unit$checks$positional$live_context_delta, 0L)
    expect_length(unit$checks$projection$live_context_delta, 0L)
  }
  narrowed = run$evidence$commits[[1L]]
  expect_identical(narrowed$after, list(X = "b", X = c("b", "c")))
  expect_length(narrowed$checks$positional$raw_delta, 0L)
  expect_identical(narrowed$checks$projection$raw_delta, c(3L, 6L, 9L))
  expect_length(narrowed$checks$projection$live_context_delta, 0L)
  # There are no live units at either first divergence, so units do not
  # repair these invalid local premises.
  expect_length(positional$state$unit_domains, 0L)
  expect_length(projection$state$unit_domains, 0L)
  expect_identical(positional$state$is_not_subset_of[[1L]][2L, ], c(X = FALSE, X = TRUE))
  expect_identical(positional$state$not_subset_count[1L, 2L], 1L)
})

test_that("the first narrowing has a nonvacuous donor-relative proof", {
  run = runs$four_clause
  narrowed = run$evidence$commits[[1L]]
  donor_context = run$input[c(2L, 4L)]
  proof = local_change(narrowed$before, narrowed$after, donor_context, run$assignments)
  expect_length(proof$checks$projection$live_context_delta, 0L)
  expect_identical(proof$checks$projection$before_models, 2L)
  expect_identical(proof$checks$projection$after_models, 2L)
  # Under the second pair of still-live donors, removing the first X=b
  # position exposes X={b,c}, changing the projection at (c,a).
  exposed = run$evidence$commits[[3L]]
  expect_identical(exposed$checks$projection$live_context_delta, 3L)
  expect_length(exposed$checks$positional$raw_delta, 0L)
  # Reinstating the already deleted c2 would hide this later projection
  # mismatch, but it is no longer a permissible live-context premise.
  restored = local_change(exposed$before, exposed$after,
    c(exposed$context, run$input[2L]), run$assignments)
  expect_length(restored$checks$projection$live_context_delta, 0L)
})

test_that("stale ranges break positional deletion while the projection stays correct", {
  run = runs$stale_range
  expect_null(run$error)
  expect_identical(run$output, list(list(Y = "c", X = "a", Y = c("c", "a"))))
  event = run$events[[first_divergence(run, "positional")]]
  expect_identical(event$line, 474L)
  expect_identical(event$delta$positional, 2L)
  expect_identical(run$assignments[2L, ], data.frame(X = "b", Y = "a", row.names = 2L))
  expect_true(all(vapply(run$events, function(event) identical(event$truth$projection, run$expected), logical(1))))
  expect_length(run$evidence$commits[[1L]]$checks$positional$raw_delta, 0L)
  expect_length(run$evidence$commits[[1L]]$checks$projection$live_context_delta, 0L)
})

test_that("the two-clause unit-HLA error is a count-mask disagreement", {
  run = runs$unit_hla_error
  expect_match(run$error, "attempt to select less than one element")
  expect_true(all(vapply(run$events, function(event) identical(event$truth$positional, run$expected), logical(1))))
  expect_true(all(vapply(run$events, function(event) identical(event$truth$projection, run$expected), logical(1))))
  final = tail(run$events, 1L)[[1L]]
  expect_identical(final$line, 753L)
  expect_identical(final$state$not_subset_count, 1L)
  expect_identical(final$contexts[[1L]]$is_not_subset_entry, c(X = FALSE, X = FALSE))
  expect_identical(final$contexts[[1L]]$symbol, character())
  expect_identical(final$state$symbol_registry$X, c(2L, 2L))
})

test_that("observers cover virtual HLA and preserve its live context", {
  virtual = runs$hla_virtual_control
  deleted = runs$hla_orphan_deletion_control
  expect_length(virtual$evidence$proposals, 3L)
  expect_length(deleted$evidence$proposals, 2L)
  expect_true(any(vapply(virtual$events, function(event) event$line == 722L, logical(1))))
  for (run in list(virtual, deleted)) {
    expect_null(run$error)
    expect_true(all(vapply(run$events, function(event) identical(event$truth$positional, run$expected), logical(1))))
    for (proposal in run$evidence$proposals) {
      expect_length(proposal$checks$positional$live_context_delta, 0L)
      expect_length(proposal$checks$projection$live_context_delta, 0L)
    }
  }
  expect_true(any(vapply(virtual$evidence$proposals, function(proposal) {
    length(proposal$checks$positional$raw_delta) > 0L
  }, logical(1))))
})

test_that("the selector candidate prevents all three control failures", {
  for (name in c("four_clause", "stale_range", "unit_hla_error")) {
    repaired = run_trace_case(case_specs[[name]], candidate = TRUE)
    expect_null(repaired$error)
    expect_true(repaired$production_subset_unchanged)
    expect_identical(truth_formula(repaired$output, repaired$assignments), repaired$expected)
    expect_identical(truth_formula(repaired$output, repaired$assignments, TRUE), repaired$expected)
    expect_true(all(vapply(repaired$input, function(clause) !anyDuplicated(names(clause)), logical(1))))
    expect_true(all(vapply(repaired$events, function(event) {
      identical(event$truth$positional, repaired$expected)
    }, logical(1))))
  }
})

test_that("missing rejection also covers checkmate's all-NA numeric and character coercion", {
  cnf = new_cnf_environment()
  candidate = private_selector_candidate(cnf)
  universe = cnf$CnfUniverse()
  symbol = cnf$CnfSymbol(universe, "X", letters[1:3])
  clause = cnf$CnfClause(list(cnf$CnfAtom(symbol, "a")))
  for (selector in list(NA, NA_real_, NA_character_)) {
    baseline = attempt(cnf$`[.CnfClause`(clause, selector))
    expect_null(baseline$error)
    expect_true(anyNA(names(baseline$value)))
    expect_match(attempt(candidate(clause, selector))$error, "missing")
  }
})

test_that("trace storage and instrumented source preserve original objects and code", {
  copy_path = tempfile(tmpdir = trace_dir, fileext = ".rds")
  on.exit(unlink(copy_path), add = TRUE)
  saveRDS(runs, copy_path)
  expect_identical(readRDS(copy_path), runs)
  copy = readLines(file.path(trace_dir, "instrumented_simplify.R"))
  stripped = copy[!grepl("^[[:space:]]*(trace_event\\(|\\.trace_root = environment\\(\\))", copy)]
  expect_identical(stripped, readLines("R/CnfFormula_simplify.R"))
})
