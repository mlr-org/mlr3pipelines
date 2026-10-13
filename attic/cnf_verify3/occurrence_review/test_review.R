source("attic/cnf_verify3/occurrence_review/review_events.R")
suppressPackageStartupMessages(library(testthat))

test_that("the range-list oracle preserves repeated names positionally", {
  assignment = context(2L)$assignment
  duplicate_tautology = structure(list("no", "yes"), names = c("X1", "X1"))
  expect_identical(truth_entries(list(duplicate_tautology), assignment), rep(TRUE, 4L))
  expect_identical(truth_words(list(c(1L, 2L), -1L), assignment), c(FALSE, FALSE, TRUE, FALSE))
  expect_identical(truth_entries(list(list(X1 = "no")), assignment), c(TRUE, FALSE, TRUE, FALSE))
  saved = tempfile(fileext = ".rds")
  on.exit(unlink(saved), add = TRUE)
  saveRDS(duplicate_tautology, saved)
  expect_identical(readRDS(saved), duplicate_tautology)
})

test_that("numeric and character public selectors keep long occurrence words", {
  ctx = context(3L)
  words = list(c(rep(1L, 48L), -2L, rep(1L, 48L), 3L, -2L))
  numeric = make_public(words, ctx)
  character = make_public(words, ctx, TRUE)
  expect_identical(numeric, character)
  expect_length(numeric[[1L]], 99L)
  expect_identical(names(numeric[[1L]]), paste0("X", abs(words[[1L]])))
  expect_identical(truth_entries(lapply(numeric, c), ctx$assignment), truth_words(words, ctx$assignment))
})

test_that("source observations reach the difficult claimed paths", {
  install_observer()
  check_case(list(c(-2L, 3L), c(-1L, -3L), c(3L, 2L, 1L, 1L), c(2L, -1L)), "exact unit skip")
  check_case(list(c(1L, 2L), c(1L, 1L, 3L), c(-1L, -1L, 3L)), "exact same-name exceptions")
  expect_gt(review$counts$unit_snapshot_skip, 0L)
  expect_gt(review$counts$same_name_unrelated_exception, 0L)
  expect_gt(review$counts$orphan, 0L)
  expect_gt(review$counts$first_copy_removed, 0L)
  expect_gt(review$counts$unit_hla_initializations, 0L)
  expect_gt(review$counts$hla_virtual_semantics, 0L)
  expect_gt(review$counts$hla_cached_pairs, 0L)
})

test_that("the completed saved campaign checked virtual HLA and deletions", {
  result = readRDS(file.path(review_dir, "events.rds"))
  expect_gt(result$counts$formulas, 2000L)
  expect_gt(result$counts$hla_virtual_semantics, 0L)
  expect_gt(result$counts$hla_cached_pairs, 0L)
  expect_gt(result$counts$orphan_clause_deleted, 0L)
  expect_gt(result$counts$hla_clause_deleted, 0L)
  expect_gt(result$counts$unit_snapshot_skip, 0L)
  expect_false(any(startsWith(names(result$counts), "FAIL:")))
  expect_identical(result$source_md5, tools::md5sum(source_paths))
})

test_that("raw cache containment must change to virtual containment during HLA", {
  result = readRDS(file.path(review_dir, "hla_column_scope.rds"))
  event = result$examples$hla_containment_needs_virtual_target
  expect_gt(result$counts$hla_containment_needs_virtual_target, 0L)
  expect_identical(event$words, list(c(1L, 2L), c(1L, 3L), c(-3L, 4L)))
  expect_null(event$entries[[event$detail$target]][[event$detail$symbol]])
  expect_identical(event$detail$virtual[[event$detail$symbol]], "no")
  expect_false(any(event$eliminated))
})

test_that("sharing one unchanged universe is an explicit necessary input condition", {
  left = context(1L)
  right = context(1L)
  clauses = c(make_public(list(1L), left), make_public(list(-1L), right))
  expect_error(cnf$CnfFormula(clauses), "All clauses must be in the same universe", fixed = TRUE)
})
