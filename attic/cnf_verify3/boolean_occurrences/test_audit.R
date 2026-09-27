source("attic/cnf_verify3/boolean_occurrences/common.R")
suppressPackageStartupMessages(library(testthat))
install_instrumentation()

test_that("the two independent truth encodings match a hand formula", {
  context = boolean_context(3L)
  assignments = context$assignments
  words = list(c(1L, 2L), c(-1L, 3L))
  expected = (assignments$S1 == 1L | assignments$S2 == 1L) &
    (assignments$S1 == 0L | assignments$S3 == 1L)
  expect_identical(word_truth(words, assignments), expected)
  expect_identical(occurrence_truth(lapply(public_clauses(words, context), c), assignments), expected)
})

test_that("the positional oracle retains unequal duplicate occurrences", {
  assignments = boolean_context(1L)$assignments
  entries = list(structure(list("0", "1"), names = c("S1", "S1")))
  expect_identical(occurrence_truth(entries, assignments), c(TRUE, TRUE))
  expect_identical(occurrence_truth(list(entries[[1]][1L]), assignments), c(TRUE, FALSE))
  path = tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(entries, path)
  expect_identical(readRDS(path), entries)
  expect_identical(occurrence_truth(readRDS(path), assignments), c(TRUE, TRUE))
})

test_that("public selectors preserve long words and global occurrence order", {
  context = boolean_context(3L)
  words = list(c(rep(1L, 12L), -2L, 3L, -2L, 1L))
  selected = public_clauses(words, context)[[1L]]
  expect_length(selected, 16L)
  expect_identical(names(selected), paste0("S", abs(words[[1L]])))
  expect_identical(unname(c(selected)), lapply(words[[1L]], function(x) as.character(as.integer(x > 0))))
})

test_that("first-copy deletion really leaves a live registry orphan", {
  audit$examples = list()
  output = one_case(list(c(-1L, 2L), c(1L, 1L, 2L, 3L)), label = "test orphan")
  event = audit$examples$orphan_occurrence
  expect_type(event, "list")
  expect_identical(event$detail$symbol, "S1")
  expect_true(any(vapply(event$entries, function(cl) any(names(cl) == "S1"), logical(1))))
  expect_null(audit$examples$semantic_error)
  expect_s3_class(output, "CnfFormula")
})

test_that("same-name exceptions can pass the count check with an unrelated name", {
  audit$examples = list()
  output = one_case(list(c(1L, 2L), c(1L, 1L, 3L), c(-1L, -1L, 3L)),
    label = "test same-name extra exception")
  event = audit$examples$same_name_extra_exception
  expect_type(event, "list")
  expect_identical(event$detail$exceptional, c("S1", "S2"))
  expect_identical(event$detail$symbol, "S1")
  expect_s3_class(output, "CnfFormula")
  expect_null(audit$examples$semantic_error)
})

test_that("initial and derived Boolean units consume every registered copy", {
  audit$examples = list()
  for (copies in c(2L, 3L, 7L, 24L)) {
    expect_true(isFALSE(one_case(list(rep(1L, copies), -1L), label = "test duplicate contradiction")))
    result = one_case(list(c(rep(1L, copies), rep(2L, copies)), -1L), label = "test copy draining")
    context = boolean_context(2L)
    expect_identical(occurrence_truth(c(result), context$assignments),
      context$assignments$S1 == 0L & context$assignments$S2 == 1L)
  }
  one_case(list(c(-1L, 2L), c(-2L, 3L), c(-3L, 4L), c(-4L, 1L),
    c(1L, 2L), c(1L, -2L)), label = "test late unit cycle")
  expect_null(audit$examples$unit_cache_skip)
  expect_null(audit$examples$unit_hla_donor)
  expect_null(audit$examples$runtime_error)
})

test_that("directed evidence includes the difficult paths with no mismatches", {
  directed = readRDS(file.path(audit_dir, "directed.rds"))
  hla = readRDS(file.path(audit_dir, "hla.rds"))
  expect_identical(directed$cases, 1514L)
  expect_identical(hla$cases, 1872L)
  for (result in list(directed, hla)) {
    expect_equal(result$totals$correct_formulas, result$cases)
    expect_gt(result$totals$delete_first_with_copy_left, 0L)
    expect_gt(result$totals$orphan_occurrence, 0L)
    expect_gt(result$totals$same_name_twoend_try, 0L)
    expect_gt(result$totals$nonunit_hla_donor, 0L)
    expect_null(result$totals$unit_hla_donor)
    expect_null(result$totals$runtime_error)
    expect_null(result$totals$semantic_error)
    expect_false(any(startsWith(names(result$totals), "FAIL:")))
  }
  expect_gt(hla$totals$nonunit_hla_deletion, 0L)
  expect_gt(directed$totals$delete_orphan_clause + hla$totals$delete_orphan_clause, 0L)
  expect_gt(directed$totals$unit_cache_skip, 0L)
  expect_false(directed$examples$unit_cache_skip$detail$registered_now)
})
