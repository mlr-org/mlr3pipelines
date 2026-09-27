# Reproduce the native-name failure in actual package namespaces.
suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
suppressMessages(library(jsonlite))
mode = commandArgs(TRUE)[[1L]]
if (mode == "development") {
  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)
} else if (mode %in% c("installed", "installed_no_bytecode")) {
  local_library = if (mode == "installed") "library" else "library_no_bytecode"
  library("mlr3pipelines", character.only = TRUE,
    lib.loc = file.path("attic", "cnf_verify3", "execution_modes", local_library))
} else stop("Unknown package mode")

ns = asNamespace("mlr3pipelines")
reference = new.env(parent = globalenv())
for (source_name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  sys.source(file.path("R", paste0(source_name, ".R")), envir = reference)
}
checked_definitions = 0L
for (name in ls(reference, all.names = TRUE)) {
  if (!is.function(reference[[name]])) next
  actual = get(name, envir = ns, inherits = FALSE)
  stopifnot(identical(formals(actual), formals(reference[[name]])),
    identical(body(utils::removeSource(actual)), body(utils::removeSource(reference[[name]]))))
  checked_definitions = checked_definitions + 1L
}

orders = rbind(c(1L, 2L, 3L), c(1L, 3L, 2L), c(2L, 1L, 3L),
  c(2L, 3L, 1L), c(3L, 1L, 2L), c(3L, 2L, 1L))
run_cases = function() {
  old_ctype = Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", old_ctype))
  records = list()
  for (ctype in c("C", "C.UTF-8")) {
    stopifnot(nzchar(Sys.setlocale("LC_CTYPE", ctype)))
    for (encoding in c("utf8", "latin1", "ascii")) {
      symbol_name = if (encoding == "ascii") "X" else intToUtf8(0xe9L)
      if (encoding == "latin1") symbol_name = iconv(symbol_name, from = "UTF-8", to = "latin1")
      warning_messages = character()
      for (order_idx in seq_len(nrow(orders))) {
        observed = withCallingHandlers({
          u = CnfUniverse()
          X = CnfSymbol(u, symbol_name, c("a", "b", "c"))
          Y = CnfSymbol(u, "Y", c("a", "b"))
          clauses = list(as.CnfClause(CnfAtom(X, "a")),
            CnfClause(list(CnfAtom(X, "b"), CnfAtom(Y, "a"))),
            CnfClause(list(CnfAtom(X, "c"), CnfAtom(Y, "b"))))
          result = CnfFormula(clauses[orders[order_idx, ]])
          bare = c(result)
          assignments = expand.grid(X = c("a", "b", "c"), Y = c("a", "b"),
            stringsAsFactors = FALSE)
          source_truth = with(assignments, X == "a" & (X == "b" | Y == "a") &
            (X == "c" | Y == "b"))
          actual_truth = if (is.logical(bare)) rep(bare, 6L) else
            vapply(seq_len(6L), function(row) {
              all(vapply(bare, function(clause) {
                any(vapply(seq_along(clause), function(position) {
                  variable = match(names(clause)[[position]], c(symbol_name, "Y"))
                  stopifnot(!is.na(variable))
                  assignments[[variable]][[row]] %in% clause[[position]]
                }, TRUE))
              }, TRUE))
            }, TRUE)
          expected_failure = ctype == "C" && encoding != "ascii"
          stopifnot(!any(source_truth), sum(actual_truth) == if (expected_failure) 2L else 0L)
          list(ctype = ctype, encoding = encoding, order = unname(orders[order_idx, ]),
            source_models = sum(source_truth), output_models = sum(actual_truth),
            witnesses = assignments[actual_truth, , drop = FALSE])
        }, warning = function(condition) {
          warning_messages <<- unique(c(warning_messages, conditionMessage(condition)))
          invokeRestart("muffleWarning")
        })
        observed$warnings = warning_messages
        records[[length(records) + 1L]] = observed
      }
    }
  }
  records
}
records = run_cases()
result = list(mode = mode, runtime = R.version.string, checked_definitions = checked_definitions,
  kernel_environment = environmentName(environment(get("simplify_cnf", ns))),
  calls = length(records), wrong_truth_calls = sum(vapply(records, function(x) x$output_models > 0L, TRUE)),
  records = records)
base_path = file.path("attic", "cnf_verify3", "root", paste0("ctype_package_", mode))
saveRDS(result, paste0(base_path, ".rds"))
write_json(result, paste0(base_path, ".json"), auto_unbox = TRUE, pretty = TRUE)
cat(toJSON(result[1:6], auto_unbox = TRUE, pretty = TRUE), "\n")
