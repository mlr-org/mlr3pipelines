# Run from the repository root:
# Rscript attic/cnf_verify3/input_contract_fixes/run_inputs_r36.R
#
# This is a small assertion shim for the exact functions used by test_CnfInputs.R,
# not a testthat installation or a replacement for the devtools test run.
# All CNF functions and checkmate assertions are the real production definitions.
suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))

source_files = file.path("R", paste0(c("CnfUniverse", "CnfSymbol", "CnfAtom",
  "CnfClause", "CnfFormula", "CnfFormula_simplify"), ".R"))
test_file = "tests/testthat/test_CnfInputs.R"
source_hashes = tools::md5sum(c(source_files, test_file))
for (file in source_files) source(file)

counts = new.env(parent = emptyenv())
counts$assertions = 0L
counts$tests = 0L
counts$skips = 0L
counts$warnings = 0L

check = function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
  counts$assertions = counts$assertions + 1L
  invisible(NULL)
}

expect_true = function(object) {
  check(is.logical(object) && length(object) == 1L && !is.na(object) && object,
    "Expected one nonmissing TRUE value")
}

expect_false = function(object) {
  check(is.logical(object) && length(object) == 1L && !is.na(object) && !object,
    "Expected one nonmissing FALSE value")
}

expect_identical = function(object, expected) {
  check(identical(object, expected), "Objects are not identical")
}

expect_length = function(object, expected) {
  check(length(object) == expected, "Unexpected object length")
}

expect_s3_class = function(object, expected) {
  check(inherits(object, expected), "Missing expected S3 class")
}

expect_error = function(object, regexp = NULL) {
  condition = tryCatch({ force(object); NULL }, error = function(e) e)
  matches = inherits(condition, "error")
  if (matches && !is.null(regexp)) matches = grepl(regexp, conditionMessage(condition))
  check(matches, "Expected error was absent or did not match its pattern")
  invisible(condition)
}

skip = function(message) {
  stop(structure(list(message = message), class = c("cnf_input_skip", "error", "condition")))
}

test_that = function(description, code) {
  before = counts$assertions
  skipped = FALSE
  tryCatch(withCallingHandlers(
    eval(substitute(code), envir = new.env(parent = parent.frame())),
    warning = function(w) {
      counts$warnings = counts$warnings + 1L
      cat("WARNING:", conditionMessage(w), "\n")
      invokeRestart("muffleWarning")
    }),
    cnf_input_skip = function(e) {
      skipped <<- TRUE
      counts$skips = counts$skips + 1L
      cat("SKIP:", description, "-", conditionMessage(e), "\n")
    },
    error = function(e) stop(sprintf("Test '%s' failed: %s", description, conditionMessage(e)), call. = FALSE)
  )
  counts$tests = counts$tests + 1L
  if (!skipped) cat("PASS:", description, "(", counts$assertions - before, " assertions)\n", sep = "")
}

cat("Harness: focused base-R assertion shim, using final test_CnfInputs.R verbatim\n")
cat(R.version.string, "\n")
cat("checkmate", as.character(packageVersion("checkmate")), "mlr3misc",
  as.character(packageVersion("mlr3misc")), "\n")
cat("Source/test MD5 hashes:\n")
for (file in names(source_hashes)) cat(source_hashes[[file]], file, "\n")
source(test_file)
stopifnot(identical(source_hashes, tools::md5sum(c(source_files, test_file))))
stopifnot(counts$warnings == 0L, counts$skips == 0L)
cat("RESULT:", counts$tests, "tests;", counts$assertions, "assertions;",
  counts$warnings, "warnings;", counts$skips, "skips\n")
