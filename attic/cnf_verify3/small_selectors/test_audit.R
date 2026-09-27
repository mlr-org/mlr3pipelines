# Focused tests of the audit machinery, independent of package loading.
source("attic/cnf_verify3/small_selectors/common.R")
suppressPackageStartupMessages(library(testthat))

test_that("the truth oracle and RDS preserve every duplicate position", {
  assignments = expand.grid(X = c("a", "b"), Y = c("a", "b"), stringsAsFactors = FALSE)
  stale = list(X = "a", X = "b")
  expect_identical(eval_clause(stale, assignments), rep(TRUE, 4L))
  expect_identical(eval_clause(stale, assignments, first_only = TRUE), c(TRUE, FALSE, TRUE, FALSE))
  expect_identical(eval_formula(list(stale, list(Y = "a")), assignments), c(TRUE, TRUE, FALSE, FALSE))
  path = tempfile("positional_", tmpdir = audit_dir)
  saveRDS(list(stale), path)
  restored = readRDS(path)
  unlink(path)
  expect_identical(restored, list(stale))
  expect_identical(names(restored[[1L]]), c("X", "X"))
  expect_identical(restored[[1L]][[2L]], "b")
})

test_that("occurrence words cover permutations and every repeated position", {
  expect_length(selector_words(1L, 4L), 4L)
  expect_length(selector_words(2L, 4L), 22L)
  expect_length(selector_words(3L, 4L), 42L)
  words = selector_words(2L, 3L)
  expect_true(any(vapply(words, identical, logical(1), c(2L, 1L, 2L))))
  expect_true(any(vapply(words, identical, logical(1), c(1L, 1L, 2L))))
  context = make_context(list(X = c("a", "b"), Y = c("a", "b")))
  expect_length(make_bank(context, 3L), 44L)
})

test_that("representative categories distinguish representation and logical failures", {
  context = make_context(list(X = letters[1:3], Y = letters[1:3]))
  a = make_selected(list(X = "a"), c(1L, 1L), context)
  expect_identical(classify(list(a), context)$category, "noncanonical_correct")
  u = make_selected(list(X = c("a", "b")), 1L, context)
  expect_identical(classify(list(a, u), context)$category, "error")
  a = make_selected(list(X = "a", Y = c("a", "c")), c(2L, 1L, 2L), context)
  b = make_selected(list(X = "a", Y = c("b", "c")), 2:1, context)
  expect_identical(classify(list(b, a), context)$category, "noncanonical_stale")
  selected = list(
    make_selected(list(X = c("b", "c")), c(1L, 1L), context),
    make_selected(list(X = "b", Y = "c"), 1:2, context),
    make_selected(list(X = "c", Y = "b"), 1:2, context),
    make_selected(list(X = "a", Y = "a"), 1:2, context)
  )
  expect_identical(classify(selected, context)$category, "canonical_wrong")
})

test_that("nonuniform splitting of membership cells preserves exact control outcomes", {
  set.seed(907317)
  # Compare raw positional outputs, after projecting the split labels back
  # to their original cell. This also checks exception outcomes.
  signature = function(output, reverse_map = NULL) {
    if (inherits(output, "error")) return(conditionMessage(output))
    if (is.logical(output)) return(c(output))
    lapply(c(output), function(clause) {
      lapply(seq_along(clause), function(j) {
        symbol = names(clause)[[j]]
        values = clause[[j]]
        if (!is.null(reverse_map)) values = unname(reverse_map[[symbol]][values])
        list(symbol = symbol, values = sort(unique(values)))
      })
    })
  }
  cases = list()
  for (name in c("pair_profiles_4_occurrences", "three_pseudounit_profiles", "three_ternary_one_duplicate")) {
    saved = readRDS(file.path(audit_dir, paste0(name, ".rds")))
    cases = c(cases, saved$examples)
  }
  for (i in seq_len(100L)) {
    n_symbols = sample.int(3L, 1L)
    domains = setNames(rep(list(letters[1:3]), n_symbols), LETTERS[seq_len(n_symbols)])
    raw = lapply(seq_len(sample.int(4L, 1L)), function(ci) {
      symbols = sort(sample.int(length(domains), sample.int(length(domains), 1L)))
      lapply(domains[symbols], function(d) d[sort(sample.int(length(d), sample.int(length(d) - 1L, 1L)))])
    })
    selectors = lapply(raw, function(cl) {
      word = c(seq_along(cl), sample.int(length(cl), sample.int(3L, 1L), replace = TRUE))
      word[sample.int(length(word))]
    })
    cases[[length(cases) + 1L]] = list(domains = domains, raw = raw, selectors = selectors)
  }
  for (case in cases) {
    old_context = make_context(case$domains)
    old_selected = Map(function(cl, sel) make_selected(cl, sel, old_context), case$raw, case$selectors)
    forward = lapply(case$domains, function(domain) {
      setNames(lapply(seq_along(domain), function(i) paste0("cell", i, "_", seq_len(i + 1L))), domain)
    })
    new_domains = lapply(forward, function(map) unname(unlist(map, use.names = FALSE)))
    reverse = lapply(forward, function(map) {
      setNames(rep(names(map), lengths(map)), unlist(map, use.names = FALSE))
    })
    new_context = make_context(new_domains)
    new_raw = lapply(case$raw, function(cl) {
      result = lapply(seq_along(cl), function(j) {
        unname(unlist(forward[[names(cl)[[j]]]][cl[[j]]], use.names = FALSE))
      })
      names(result) = names(cl)
      result
    })
    new_selected = Map(function(cl, sel) make_selected(cl, sel, new_context), new_raw, case$selectors)
    old = tryCatch(CnfFormula(lapply(old_selected, `[[`, "clause")), error = identity)
    new = tryCatch(CnfFormula(lapply(new_selected, `[[`, "clause")), error = identity)
    expect_identical(signature(new, reverse), signature(old))
  }
})
